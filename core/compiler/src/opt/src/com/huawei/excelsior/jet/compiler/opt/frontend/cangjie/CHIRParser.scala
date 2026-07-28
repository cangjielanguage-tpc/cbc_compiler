/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.cangjie

import com.google.flatbuffers.Table
import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.{PreparationRequired, RTSProc, Stage, StatsKind}
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.cangjie.CHIRVTable
import com.huawei.excelsior.jet.compiler.chir.CHIRUtils.*
import com.huawei.excelsior.jet.compiler.chir.{Attribute, CHIRLoader, CHIRResolver, EnumKind, PackageFormat, ParsedCHIRPackage}
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.{BlockGroup, CHIRExprKind, CHIRTypeKind, EnumDef, Expression, FuncType, Function, Block as BlockVal}
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, ConstBranchElimination, Universe}
import com.huawei.excelsior.jet.compiler.opt.ir.nodes.HLIRNodes
import com.huawei.excelsior.jet.compiler.opt.middle.patterns.Arrays
import com.huawei.excelsior.jet.compiler.opt.middle.{ContextTypesRecalculation, DCEComponent, UCEComponent}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{ContextTypesInParsing, DetailedParsingLogs, GenerateWriteBarriers, PackageInitFromMain, RealCheckedOps, SkipCHIRGarbageCalls}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParameter
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.fromSymType
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeFieldReference, BitcodeMethodReference, CangjieFieldReference, Field, InstantiatedMethodReference, Method, MethodReference, MethodSignature, MethodType, SignatureType, ClassType as SymClassType, MethodReferenceAccessKind as MAK, Type as SymType}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.util.ScalaCollections.*
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.{Numbering, ScalaCollections}

import scala.PartialFunction.cond
import scala.collection.mutable
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParameter.GenericFuncParams
import com.huawei.excelsior.jet.compiler.types.CompiledType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ReferenceType

trait CHIRParser
  extends UCEComponent
     with ConstBranchElimination
     with CangjieParsingCleanup
     with DCEComponent
     with HLIRNodes
     with ContextTypesRecalculation
     with Arrays { self: Universe =>

  implicit object TableSetsAndMaps extends Sets.Default[Table] with Maps.Default[Table]

  def loadCHIRMethod(method: Method, args0: Seq[Node]): RTPartsInfo = stage(Stage.LoadCHIR) {
    val args = args0
    val (ret, message) = method match {
      case _ => (loadNormal(method, args), "after load normal")
    }

    dbgPrinter.debugNodes(message)
    dbgPrinter.debugGraphs(message)
    currentScope.setResult(ret)

    RTPartsInfo(isDirtyForClassGC = false)
  }

  private def convertRecord(tpe: Type, n: Node): Node = (n.tpe, tpe) match {
    case (from, to) if from == to => n

    case (RecordAddrType(x), RecordAddrType(y)) if x.isArraySliceLike && y.isArraySliceLike => n

    case (from: RecordAddrType, to: RecordAddrType) =>
      // Such casts can happen when the same record is instantiated in different packages with different mangled names.
      // TODO: check actual layout of records or better -- prohibit such casts at all
      assert(from.sigType.getRawObjectSize == to.sigType.getRawObjectSize, s"inconsistent record type size: cast $from -> $to")
      ReinterpretCast(from, to)(n)

    case (from @ (_: RecordAddrType | AddrType), to @ (_: RecordAddrType | AddrType)) =>
      // Such casts are needed to convert @C structs to/from C pointers.
      ReinterpretCast(from, to)(n)

    case _ => n
  }

  private def withRecordConversion[T](action: => T): T = {
    Node.withImplicitArgConversion(convertRecord) {
      action
    }
  }

  private def loadNormal(method: Method, args: Seq[Node]): Return = {
    val Some(source, idx) = method.getCHIRDef
    implicit val resolver: CHIRResolver = CHIRLoader.getCHIRResolver(source.toString)(env)
    implicit val pkg: ParsedCHIRPackage = resolver.pkg

    val func = pkg.getValue[Function](idx)

    stage(Stage.CangjieFunctionParsing) {
      val blockMap = withFreeUnreachableBlocks {
        makeCFG(method, func)
      }
      dbgPrinter.debugCFG("CFG after parsing")

      checkGraphConsistency(CheckLevels.Important, cfg)
      checkIRConsistency(CheckLevels.Important)

      def convertNullAndProxy(tpe: Type, n: Node) = (n, n.tpe, tpe) match {
        case (_: AnyNull, EopType.Null, EopType.Plain | EopType.Eop(_)) => AnyNull(tpe)

        // Adjust type in case when target expects the rich value
        case (_: AnyNull, EopType.Plain, EopType.Eop(_)) => AnyNull(tpe)

        case (_: Proxy, EopType.Plain, EopType.Eop(_)) => ReinterpretCast(n.tpe, tpe)(n)
        case _ => n
      }

      Node.withImplicitArgConversion(convertNullAndProxy) {
        withRecordConversion {
          // TODO: withPosFactory
          CHIRInterpreter(method, func, blockMap).iterate()
        }
      }
      dbgPrinter.debugNodes("All graph after BCP")
      dbgPrinter.debugGraphs("Graph after BCP")
    }

    if (!isO1Compiled) {
      if (eliminateUnreachableCode()) {
        dbgPrinter.debugNodes("All graph after UCE")
      }
      // Cleanup proxies
      if (eliminateDeadCode()) {
        dbgPrinter.debugNodes("All graph after DCE")
      }
    }

    if (simplifyLocalVariables()) {
      dbgPrinter.debugNodes("All graph after local variables simplification")
      completeSSA()
      dbgPrinter.debugNodes("All graph after variables to SSA conversion")
    }


    // Must be done after all parsing optimizations that might accidentally remove this unified return (see JET-16162).
    val returnType = method.getReturnType
    val retValType = ValueType.fromSig(returnType, instantiateRich = true)
    val ret = unifyReturns(retValType)

    dbgPrinter.debugNodes("All graph after returns unification")

    ret
  }

  private class BlockMap {
    private val _blocks = mutable.LinkedHashMap.empty[BlockVal, Block]
    private val _blockVals = mutable.LinkedHashMap.empty[Block, BlockVal]

    def link(value: BlockVal, block: Block): Unit = {
      _blocks(value) = block
      _blockVals(block) = value
    }

    def apply(value: BlockVal): Block = _blocks(value)
    def apply(block: Block): BlockVal = _blockVals(block)

    def contains(block: Block): Boolean = _blockVals.contains(block)

    def iterator: Iterator[(BlockVal, Block)] = _blocks.iterator
    def blocks: Iterator[Block] = _blocks.valuesIterator
    def blockVals: Iterator[BlockVal] = _blocks.keysIterator
  }

  private def makeCFG(method: Method, func: Function)(implicit pkg: ParsedCHIRPackage): BlockMap = {
    val bg = pkg.getValue[BlockGroup](func.body)

    // Create blocks
    val blockMap = new BlockMap
    for (id <- bg.blocksVector.iterator; v = pkg.getValue[BlockVal](id)) {
      val b = if (v.isLandingPadBlock) {
        val handler = XBlock()
        Catch(handler)
        handler
      } else {
        BBlock()
      }
      blockMap.link(v, b)
    }

    // Create block ends
    for ((bv, b) <- blockMap.iterator) {

      def succBlocks(t: Expression): Seq[Block] = {
        val operands = t.operandsVector.toSeq
        val blockArgs = t.kind match {
          case CHIRExprKind.Branch | CHIRExprKind.MultiBranch | CHIRExprKind.RaiseException => operands.tail
          case CHIRExprKind.TryApply | CHIRExprKind.TryInvoke | CHIRExprKind.TryIntrinsic |
               CHIRExprKind.TrySpawn | CHIRExprKind.TryNumericCast | CHIRExprKind.TryAllocate | CHIRExprKind.TryRawArrayAllocate |
               CHIRExprKind.TryNeg | CHIRExprKind.TryAdd | CHIRExprKind.TrySub | CHIRExprKind.TryMul |
               CHIRExprKind.TryDiv | CHIRExprKind.TryMod | CHIRExprKind.TryExp |
               CHIRExprKind.TryLShift | CHIRExprKind.TryRShift => operands.takeRight(2)
          case _ => operands
        }
        blockArgs.map(id => blockMap(pkg.getValue[BlockVal](id)))
      }

      def goto(t: Expression): Unit = {
        val end = Goto(b, b)
        succBlocks(t) foreach (_ addArg end)
      }

      def branch(t: Expression): Unit = {
        val end = If(b, b, Proxy(ConditionType)(b))
        val Seq(trueBlock, falseBlock) = succBlocks(t)
        trueBlock addArg end.trueExit
        falseBlock addArg end.falseExit
      }

      def multiBranch(v: PackageFormat.MultiBranch): Unit = {
        val end = Switch(v.caseValuesVector.toSeq.map(_.toInt))(b, b, Proxy(IntType)(b))
        for ((exit, target) <- end.exits zip succBlocks(v.base)) {
          target addArg exit
        }
      }

      def exit(): Unit = {
        val retType = ValueType.fromSig(method.getReturnType) // FIXME: EOPs
        Return(b, b, Proxy(retType)(b))
      }

      def exceptGoto(t: Expression): Unit = {
        val anchor = HandlerAnchor(b)
        val end = Goto(anchor, b)
        val Seq(target: BBlock, handler: XBlock) = succBlocks(t)
        target addArg end
        handler addArg anchor.xpoint
      }

      def throwOp(t: Expression): Unit = {
        var ctrl: ControlNode = b
        for (handler <- ScalaCollections.singleton(succBlocks(t))) {
          val anchor = HandlerAnchor(b)
          assert(handler.isInstanceOf[XBlock])
          handler addArg anchor.xpoint
          ctrl = anchor
        }
        Halt.empty()(ctrl, b)
      }

      def processTerminator(t: Expression): Unit = t.kind match {
        case CHIRExprKind.Goto => goto(t)
        case CHIRExprKind.Exit => exit()
        case CHIRExprKind.RaiseException => throwOp(t)
        case CHIRExprKind.TryApply |
             CHIRExprKind.TryInvoke |
             CHIRExprKind.TryIntrinsic |
             CHIRExprKind.TrySpawn |
             CHIRExprKind.TryNumericCast |
             CHIRExprKind.TryAllocate |
             CHIRExprKind.TryRawArrayAllocate |
             CHIRExprKind.TryNeg |
             CHIRExprKind.TryAdd |
             CHIRExprKind.TrySub |
             CHIRExprKind.TryMul |
             CHIRExprKind.TryDiv |
             CHIRExprKind.TryMod |
             CHIRExprKind.TryExp |
             CHIRExprKind.TryLShift |
             CHIRExprKind.TryRShift => exceptGoto(t)
        case CHIRExprKind.Branch | CHIRExprKind.MultiBranch => shouldNotReachHere("Branch and MultiBranch are processed outside")
        case _ => // the rest are not terminators
      }

      (pkg.getExpr[Table](bv.exprs(bv.exprsLength - 1)): @unchecked) match {
        case v: PackageFormat.MultiBranch => multiBranch(v)
        case v: PackageFormat.Expression => processTerminator(v)
        case v: PackageFormat.AllocateBase => processTerminator(v.base)
        case v: PackageFormat.ApplyBase => processTerminator(v.base.base)
        case v: PackageFormat.BinaryExpressionBase => processTerminator(v.base)
        case v: PackageFormat.Branch => branch(v.base)
        case v: PackageFormat.IntrinsicBase => processTerminator(v.base.base)
        case v: PackageFormat.InvokeBase => processTerminator(v.base.base)
        case v: PackageFormat.NumericCastBase => processTerminator(v.base)
        case v: PackageFormat.RawArrayAllocateBase => processTerminator(v.base)
        case v: PackageFormat.SpawnBase => processTerminator(v.base)
        case v: PackageFormat.UnaryExpressionBase => processTerminator(v.base)
      }
    }

    // Link prologue with entry block
    val chirEntryBlock = blockMap(pkg.getValue[BlockVal](bg.entryBlock))
    chirEntryBlock addArg Goto(entryBlock, entryBlock)

    blockMap
  }

  private class CHIRInterpreter(method: Method, func: Function, blockMap: BlockMap)(implicit pkg: ParsedCHIRPackage, resolver: CHIRResolver) extends AbstractInterpreter {
    interpreter =>

    private val entryBlockVal = pkg.getValue[BlockVal](pkg.getValue[BlockGroup](func.body).entryBlock)

    private val params = func.paramsVector.toSeq.map(pkg.getValue[PackageFormat.Parameter](_))

    private def exprs(b: BlockVal): Iterator[Table] = b.exprsVector.iterator.map(pkg.getExpr[Table](_))

    private def operands(e: PackageFormat.Expression): Seq[Table] = e.operandsVector.toSeq.map(pkg.getValue[Table](_))

    private val localValues = Numbering[Table](params ++ blockMap.blockVals.flatMap(exprs))

    class State(private var locals: Array[NodeRef], _memory: NodeRef, _contextTypes: ContextTypesMap)
      extends Scope.State(null, _memory, _contextTypes) {

      protected type This = State

      override protected def forkImpl() =
        new State(locals, memory, if (contextTypes != null) contextTypes.clone() else null)

      override def makeUnreachableCopy() =
        new State(locals map (_ => NoValue()), memory, ContextTypesMap.Unreachable)

      def apply(id: Long): Node =
        apply(pkg.getExpr[Table](id))

      def apply(x: Table): Node = {
        val v = x match {
          case x: PackageFormat.LocalVar => pkg.getExpr[Table](x.associatedExpr)
          case x => x
        }
        val i = localValues.number(v)
        locals(i).deref ensuring (_ != null)
      }

      def update(id: Long, value: Node): Unit =
        update(pkg.getExpr[Table](id), value)

      def update(x: Table, value: Node): Unit = {
        val v = x match {
          case x: PackageFormat.LocalVar => pkg.getExpr[Table](x.associatedExpr)
          case x => x
        }
        assert(value != null)
        val i = localValues.number(v)
        if (locals(i) != value) {
          copyOnWrite()
          locals(i) = value
        }
      }

      override protected def copyOnWriteImpl(): Unit = {
        locals = locals.clone()
      }

      override def mergeFrom(block: Block, states: Seq[State], identity: Boolean)(mergeFunc: (Type, Seq[Node]) => Node): State = {
        assert(states forall (_.locals.length == this.locals.length))

        /** Merge sequence of values by computing merged value's type and
          * invoking merge function `doMerge` to do actual merge.
          * Handle special cases when
          * - values are incompatible and cannot be merged
          * - there are any dead values in the sequence
          * - all values are the same and merge function of same values is identity
          * Used to implement both `Phi` and `Proxy` creation.
          */
        def mergeValues(values: Seq[Node]): Node = {
          // TODO: remove copy-paste with VarProcessor.SSACompleter.State.mergeFrom and VMStates.VMState.mergeFrom
          assert(!(values contains null))
          val tpe = {
            val mergedType = values map (_.tpe) reduce (_ | _)
            // Pretend that result is TRef if all values are null.
            if (mergedType == EopType.Null) EopType.Plain else mergedType
          }
          ScalaCollections.uniqueValue(values) match {
            case None if tpe eq ValueType => Invalid // Incompatible types.
            case Some(value) if identity => value
            case _ => mergeFunc(tpe, values)
          }
        }

        if (!identity || states.exists(_.locals ne this.locals)) {
          this.copyOnWrite()
          for (i <- 0 until locals.length) {
            this.locals(i) = mergeValues(states map (_.locals(i).deref))
          }
        }

        super.mergeFrom(block, states, identity)(mergeFunc)
      }

      override def foreachPair(that: State)(action: (Node, Node) => Unit): Unit = {
        assert(this.locals.length == that.locals.length)

        for ((x, y) <- this.locals zip that.locals) action(x.deref, y.deref)

        super.foreachPair(that)(action)
      }
    }

    override protected def debug(msg: String): Unit = {
      if (env.enabled(DetailedParsingLogs)) {
        dbgPrinter.debugNodes("All graph after " + msg)
      }
    }

    // TODO: reduce copy-paste with VMStateInterpreter
    private def registerXCtrl(n: Node, state: State, handler: XBlock): Unit = {
      if (handler != null) n match {
        case sn: SpinalNode if sn.canThrow =>
          val xpoint = sn.xpoint
          handler.addArg(xpoint)

          val xstate = state.fork()
          xstate.memory = Invalid
          xstate.rewind(sn)
          xstate.add(xpoint)
          addXCtrl(xpoint, xstate)

        case _ =>
      }
    }

    protected def startInputState(b: Block) = {
      new State(Array.fill(localValues.order.size)(Invalid), entryMemory,
        if (env.enabled(ContextTypesInParsing)) new ContextTypesMap() else null)
    }

    protected def interpret(block: Block, state: State): Block = {
      require(state != null)
      val anchor = block.outCtrl match {
        case anchor: HandlerAnchor => anchor
        case x => assert(x == block.blockEnd); null
      }

      currentScope.inState(state) {
        withIdempotentDominance {
          // In IR built from bytecode there may be only one exception handler for a block.
          val xhandler = if (anchor != null) anchor.xHandler else null
          onCommit.withCallback(registerXCtrl(_, state, xhandler)) {
            state.add(block)
            if (anchor != null) {
              state.add(anchor)
            }
            block.blockEnd.inCtrl = null
            block.spineBackwardIsBroken = true

            // Here both memory and control dependencies become broken in this block.
            // Note that overall CFG remains valid, because block and its block end are still linked.
            // TODO: do not break IR and rework state so that it can keep this block together,
            //       while allowing insertion of new nodes in parsing and in insert code.

            parseBlock(block, state)

            block.blockEnd.inCtrl = currentCtrl.asInstanceOf[UpperPoint]
            block.blockEnd.inMemory = currentMemory

            block.spineBackwardIsBroken = false
            checkConsistency(CheckLevels.Desirable)(Block.verifyBlockControlNums(block))
            if (state.contextTypes != null) {
              ContextTypesMap.setMapAt(block.blockEnd, state.contextTypes)
            }
          }
        }
      }
      if (anchor != null) {
        strikeOut(anchor)
      }
      block
    }

    // TODO: reduce copy-paste with VMStateInterpreter
    override protected def interpretEdge(blockExit: BlockExit, state: State): ControlNode = {
      if (state.contextTypes != null) {
        val filter = state.contextTypes.makeFilter(blockExit)
        if (filter != null) {
          val branchExit = blockExit.asInstanceOf[Branch.Exit] // Otherwise it cannot be filter exit
          if (filter.isRedundant) {
            ContextTypesStats.updateOnRedundantFilterRemove(filter)
            return replaceByGoto(branchExit)
          } else {
            state.contextTypes.appendFilter(filter)
            ContextTypesMap.setMapAt(branchExit, state.contextTypes)
          }
        }
      }
      blockExit
    }

    private def parseBlock(block: Block, state: State): Unit = {
      if (block == entryBlock) {
        // Prologue block
        assert(!blockMap.contains(block))

        // Fill params
        val (rcvOpt, args) = if (method.hasReceiverParameter || method.hasMutRecordParameter) {
          (Some(params.head), params.tail)
        } else {
          (None, params)
        }
        for (rcv <- rcvOpt) {
          val idx = if (method.hasReceiverParameter) method.getReceiverArgIdx else method.getMutRecordArgIdx
          state(rcv) = rootMethodParam(idx)
        }
        val chirParamStart = method.getMethodType.startSpecialParamsCount
        for ((p, i) <- args.zipWithIndex) {
          state(p) = rootMethodParam(i + chirParamStart)
        }

        if (env.enabled(PackageInitFromMain) && method.isMain) {
          for (id <- Seq(pkg.pkg.packageLiteralInitFunc, pkg.pkg.packageInitFunc)) {
            val func = pkg.getValue[Function](id)
            val refType = resolver.findClass(func.base.packageName).get

            val name = resolver.symName(func)
            val target = calcMethodRef(refType, SignatureType.fromSymType(refType), name, func, func.funcKind, func.base.base.base.attributes)

            callMethod(target, None, SignatureType.Void, Seq.empty, Seq.empty)
          }
        }

        if (pkg.getValue[Function](pkg.pkg.packageLiteralInitFunc) == func) {
          // TODO: consider moving it under @has_invoked_pkg_init_literal check
          for (id <- 1L to pkg.pkg.valuesLength) pkg.getValue[Table](id) match {
            case g: PackageFormat.GlobalVar if !resolver.isImported(g.base) =>
              val declType = if (g.base.declaredParent == 0) {
                resolver.findClass(pkg.pkg.name).get
              } else {
                resolver.symType(pkg.getDef[Table](g.base.declaredParent)).get
              }
              val field = asClassType(declType).findDeclaredFieldOrNull(xstr(resolver.symName(g)))
              assert(field.isStatic, field)
              val value = pkg.getValue[Table](g.initializer) match {
                case null | _: PackageFormat.NullLiteral | _: PackageFormat.UnitLiteral | _: PackageFormat.Function => null
                case v: PackageFormat.IntLiteral => IntegralConst(ValueType.fromSig(field.getType))(v.`val`)
                case v: PackageFormat.FloatLiteral => field.getType match {
                  case SignatureType.Float32 => FConst(v.`val`.toFloat)
                  case SignatureType.Float64 => DConst(v.`val`)
                  case t => notImplemented(s"unexpected static field type ${t.toJETSignature} of field $field")
                }
                case v: PackageFormat.BoolLiteral => IConst(if (v.`val`) 1 else 0)
                case v: PackageFormat.RuneLiteral => IConst(v.`val`.toInt)
              }
              if (value != null) {
                StoreStaticFieldSeq(Seq(CangjieFieldReference(field.getFieldIndex, Some(field), SignatureType.fromSymType(declType), field.getType)))(value)
              }
            case _ =>
          }
        }

      } else { // Regular block
        val blockVal = blockMap(block)

        exprs(blockVal) foreach {
          case e: PackageFormat.UnaryExpressionBase =>
            import SignatureType.*
            val arg = operands(e.base) match {
              case Seq(argVar, /*Exception targets*/ _*) => state(argVar)
            }

            val sig = resolver.typeSig(e.base.resultTy, func)
            val tpe = if (sig == Float16) FloatType else arg.tpe

            def adjustBool(n: Node): Node = {
              BitFieldExtract.BFX(tpe, 0, sig.toAsm.sizeInBits, signExtension = false, n)
            }

            val n = e.base.kind match {
              case CHIRExprKind.Neg => Neg(tpe)(arg)
              case CHIRExprKind.Not => CondVal(negated = true)(Cmp(tpe, Condition.NE)(adjustBool(arg), IntegralConst(tpe)(0)))
              case CHIRExprKind.BitNot => Xor(arg, IntegralConst(tpe)(-1))
              case x => shouldNotReachHere(s"unexpected unary expression: ${PackageFormat.CHIRExprKind.name(x)}")
            }
            state(e) = n

          case e: PackageFormat.BinaryExpressionBase =>
            import SignatureType.*
            val (sig, lraw, rraw) = operands(e.base) match {
              case Seq(lvar: PackageFormat.LocalVar, rvar, /*Exception targets*/ _*) =>
                (resolver.typeSig(lvar.base.`type`, func), state(lvar), state(rvar))
              case Seq(lpar: PackageFormat.Parameter, rvar, /*Exception targets*/ _*) =>
                (resolver.typeSig(lpar.base.`type`, func), state(lpar), state(rvar))
            }

            val resSig = resolver.typeSig(e.base.resultTy, func)

            def adjustRes(n: Node) = if (resSig == Float16) ValueConvert(AsmType.F32, AsmType.F16)(n) else n

            def adjustArg(n: Node) = sig match {
              case Float16 =>
                ValueConvert(AsmType.F16, AsmType.F32)(n)
              case sig: SignatureType.Integral if sig.isShortIntegral =>
                BitFieldExtract(IntType, 0, sig.bits, signExtension = sig.signed, n)
              case _ =>
                n
            }

            val l = adjustArg(lraw)
            val r = adjustArg(rraw)

            val tpe = if (sig == Float16) FloatType else l.tpe

            val n = (resSig: @unchecked) match {
              case Boolean =>
                val unsigned = sig match {
                  case sig: Integral => !sig.signed
                  case _ => false
                }
                // Note: Cangjie does not have "unordered" floating point comparisons
                val op = e.base.kind match {
                  case CHIRExprKind.LT => if (unsigned) Condition.ULT else Condition.LT
                  case CHIRExprKind.GT => if (unsigned) Condition.UGT else Condition.GT
                  case CHIRExprKind.LE => if (unsigned) Condition.ULE else Condition.LE
                  case CHIRExprKind.GE => if (unsigned) Condition.UGE else Condition.GE
                  case CHIRExprKind.Equal => Condition.EQ
                  case CHIRExprKind.NotEqual => Condition.NE
                  case x => shouldNotReachHere(s"unexpected boolean binary expression: ${PackageFormat.CHIRExprKind.name(x)}")
                }
                CondVal(Cmp(tpe, op)(l, r))

              case resSig: FloatingPoint =>
                e.base.kind match {
                  case CHIRExprKind.Add => Add(l, r)
                  case CHIRExprKind.Sub => Sub(l, r)
                  case CHIRExprKind.Mul => Mul(l, r)
                  case CHIRExprKind.Div => FDiv(tpe)(l, r)
                  case x => shouldNotReachHere(s"unexpected floating point binary expression: ${PackageFormat.CHIRExprKind.name(x)}")
                }

              case resSig: Integral =>
                assert(resSig == sig)
                val size = resSig.bits
                val signed = resSig.signed

                val strategy = e.overflowStrategy match {
                  case PackageFormat.OverflowStrategy.THROWING if !env.enabled(RealCheckedOps) => PackageFormat.OverflowStrategy.WRAPPING
                  case s => s
                }

                strategy match {
                  case PackageFormat.OverflowStrategy.WRAPPING =>
                    e.base.kind match {
                      case CHIRExprKind.Add => Add(l, r)
                      case CHIRExprKind.Sub => Sub(l, r)
                      case CHIRExprKind.Mul => Mul(l, r)
                      case CHIRExprKind.Div | CHIRExprKind.TryDiv => DivisorCheck()(r); IDivRemOp(tpe, isUnsigned = !signed, isDiv = true)(l, r)
                      case CHIRExprKind.Mod => DivisorCheck()(r); IDivRemOp(tpe, isUnsigned = !signed, isDiv = false)(l, r)
                      case CHIRExprKind.LShift => Shift(ArithOp.LSL, l, BitFieldExtract.Truncate(r)) // TODO explicit overshift check in compiler or runtime
                      case CHIRExprKind.RShift =>
                        val op = if (sig.toAsm.isSigned) ArithOp.ASR else ArithOp.LSR // Cangjie has arithmetic shift in case of signed left operand
                        Shift(op, l, BitFieldExtract.Truncate(r)) // TODO explicit overshift check in compiler or runtime
                      case CHIRExprKind.BitAnd => And(l, r)
                      case CHIRExprKind.BitOr => Or(l, r)
                      case CHIRExprKind.BitXor => Xor(l, r)
                      case x => shouldNotReachHere(s"unexpected wrapping binary expression: ${PackageFormat.CHIRExprKind.name(x)}")
                    }
                  case PackageFormat.OverflowStrategy.THROWING =>
                    val width = sig.toAsm.width
                    val normalizedArgs = Seq(l, r) map { n =>
                      CheckedOp.normalizeArg(n.tpe, width, signed, n)
                    }
                    e.base.kind match {
                      case CHIRExprKind.Add => CheckedOp(tpe, width, CheckedOp.Kind.ADD, signed, method.isManaged)(normalizedArgs: _*)
                      case CHIRExprKind.Sub => CheckedOp(tpe, width, CheckedOp.Kind.SUB, signed, method.isManaged)(normalizedArgs: _*)
                      case CHIRExprKind.Mul => CheckedOp(tpe, width, CheckedOp.Kind.MUL, signed, method.isManaged)(normalizedArgs: _*)
                      case CHIRExprKind.Div => CheckedOp(tpe, width, CheckedOp.Kind.DIV, signed, method.isManaged)(normalizedArgs: _*)
                      case CHIRExprKind.Mod => DivisorCheck()(r); IDivRemOp(tpe, isUnsigned = !signed, isDiv = false)(normalizedArgs: _*)
                      case CHIRExprKind.Exp => RTSCall(RTSProc.CJ_throwingPowI64)(lraw, rraw)
                      case x => shouldNotReachHere(s"unexpected throwing binary expression: ${PackageFormat.CHIRExprKind.name(x)}")
                    }

                  case PackageFormat.OverflowStrategy.SATURATING =>
                    val proc = e.base.kind match {
                      case CHIRExprKind.Exp => assert(size == 64); RTSProc.CJ_saturatingPowI64
                      case CHIRExprKind.Add => size match {
                        case 8  => if (signed) RTSProc.CJ_saturatingAddI8  else RTSProc.CJ_saturatingAddU8
                        case 16 => if (signed) RTSProc.CJ_saturatingAddI16 else RTSProc.CJ_saturatingAddU16
                        case 32 => if (signed) RTSProc.CJ_saturatingAddI32 else RTSProc.CJ_saturatingAddU32
                        case 64 => if (signed) RTSProc.CJ_saturatingAddI64 else RTSProc.CJ_saturatingAddU64
                      }
                      case CHIRExprKind.Sub => size match {
                        case 8  => if (signed) RTSProc.CJ_saturatingSubI8  else RTSProc.CJ_saturatingSubU8
                        case 16 => if (signed) RTSProc.CJ_saturatingSubI16 else RTSProc.CJ_saturatingSubU16
                        case 32 => if (signed) RTSProc.CJ_saturatingSubI32 else RTSProc.CJ_saturatingSubU32
                        case 64 => if (signed) RTSProc.CJ_saturatingSubI64 else RTSProc.CJ_saturatingSubU64
                      }
                      case CHIRExprKind.Mul => size match {
                        case 8  => if (signed) RTSProc.CJ_saturatingMulI8  else RTSProc.CJ_saturatingMulU8
                        case 16 => if (signed) RTSProc.CJ_saturatingMulI16 else RTSProc.CJ_saturatingMulU16
                        case 32 => if (signed) RTSProc.CJ_saturatingMulI32 else RTSProc.CJ_saturatingMulU32
                        case 64 => if (signed) RTSProc.CJ_saturatingMulI64 else RTSProc.CJ_saturatingMulU64
                      }
                      case CHIRExprKind.Div => size match {
                        case 8  => if (signed) RTSProc.CJ_saturatingDivI8  else RTSProc.CJ_saturatingDivU8
                        case 16 => if (signed) RTSProc.CJ_saturatingDivI16 else RTSProc.CJ_saturatingDivU16
                        case 32 => if (signed) RTSProc.CJ_saturatingDivI32 else RTSProc.CJ_saturatingDivU32
                        case 64 => if (signed) RTSProc.CJ_saturatingDivI64 else RTSProc.CJ_saturatingDivU64
                      }
                      case CHIRExprKind.Mod => size match {
                        case 8  => if (signed) RTSProc.CJ_saturatingModI8  else RTSProc.CJ_saturatingModU8
                        case 16 => if (signed) RTSProc.CJ_saturatingModI16 else RTSProc.CJ_saturatingModU16
                        case 32 => if (signed) RTSProc.CJ_saturatingModI32 else RTSProc.CJ_saturatingModU32
                        case 64 => if (signed) RTSProc.CJ_saturatingModI64 else RTSProc.CJ_saturatingModU64
                      }
                      case x => shouldNotReachHere(s"unexpected saturating binary expression: ${PackageFormat.CHIRExprKind.name(x)}")
                    }
                    RTSCall(proc)(lraw, rraw)

                  case x => shouldNotReachHere(s"unexpected overflow strategy: ${PackageFormat.OverflowStrategy.name(x)}")
                }
            }
            state(e) = adjustRes(n)

          case e: PackageFormat.AllocateBase =>
            val sig = resolver.typeSig(e.allocatedType, func)
            val n = if (sig.isTraceableReference) {
              pkg.getType[Table](e.allocatedType) match {
                case t: PackageFormat.Type if t.kind == CHIRTypeKind.REFTYPE =>
                  // Uninitialized
                  NoValue()

                case _ =>
                  if (sig.isAbstractClass) {
                    Null()
                  } else {
                    New(sig)()
                  }
              }

            } else if (sig.isZST) {
              NoValue()

            } else if (sig.isPrimitive) {
              ZeroValueNode(ValueType.fromSig(sig))

            } else {
              assert(sig.isRecord)
              StackAlloc.Local(sig, workaroundForNonZeroedTraceableRecords = true)
            }
            state(e) = n

          case e: PackageFormat.RawArrayAllocateBase =>
            val len = operands(e.base) match {
              case Seq(len, /*Exception targets*/ _*) => state(len)
            }
            val elemType = resolver.typeSig(e.elementType, func)
            val arrayType = SignatureType.CangjieArray(elemType)
            state(e) = NewArray(arrayType)(len)

          case e: PackageFormat.GetElementRef =>
            val (mem, memBase, staticField) = operands(e.base) match {
              case Seq(memVal: PackageFormat.Parameter) => (state(memVal), memVal.base, None)
              case Seq(memVal: PackageFormat.LocalVar) => (state(memVal), memVal.base, None)
              case Seq(memVal: PackageFormat.GlobalVar) => (NoValue(), memVal.base.base, Some(staticFieldRef(memVal)))
            }

            staticField match {
              case None =>
                nullCheck(mem)
              case Some(sf) =>
                val symRefType = asClassType(sf.refType)
                ensurePrepared(symRefType)
                packageInitCheck(symRefType)
            }

            val host = resolver.typeSig(memBase.`type`, func)

            val fields = fieldChain(host, e.pathVector.toSeq)

            val lastField = fields.last
            val n = if (lastField.fieldType.isZST) {
              // do nothing
              NoValue()

            } else {
              staticField match {
                case None => GetFieldSeqRef(fields)(mem)
                case Some(sf) => GetStaticFieldSeqRef(sf +: fields)
              }
            }
            state(e) = n

          case e: PackageFormat.StoreElementRef =>
            val (arg, mem, memBase, staticField) = operands(e.base) match {
              case Seq(argVal, memVal: PackageFormat.Parameter) => (state(argVal), state(memVal), memVal.base, None)
              case Seq(argVal, memVal: PackageFormat.LocalVar) => (state(argVal), state(memVal), memVal.base, None)
              case Seq(argVal, memVal: PackageFormat.GlobalVar) => (state(argVal), NoValue(), memVal.base.base, Some(staticFieldRef(memVal)))
            }

            staticField match {
              case None =>
                nullCheck(mem)
              case Some(sf) =>
                val symRefType = asClassType(sf.refType)
                ensurePrepared(symRefType)
                packageInitCheck(symRefType)
            }

            val host = resolver.typeSig(memBase.`type`, func)

            if (host.isArray) {
              assert(e.pathLength == 1)
              val idx = e.path(0)
              arrayPut(host, mem, LConst(idx), arg)

            } else {
              val fields = fieldChain(host, e.pathVector.toSeq)

              val lastField = fields.last
              if (lastField.fieldType.isZST) {
                // do nothing
                NoValue()

              } else if (needsCopy(lastField.fieldType)) {
                val addr = GetFieldSeqRef(fields)(mem)
                copy(lastField.fieldType, addr, arg)

              } else {
                writeBarrier()
                staticField match {
                  case None => StoreFieldSeq(fields)(mem, arg)
                  case Some(sf) => StoreStaticFieldSeq(sf +: fields)(arg)
                }
              }
            }

          case e: PackageFormat.Field =>
            val (mem, memBase, staticField) = operands(e.base) match {
              case Seq(memVal: PackageFormat.Parameter) => (state(memVal), memVal.base, None)
              case Seq(memVal: PackageFormat.LocalVar) => (state(memVal), memVal.base, None)
              case Seq(memVal: PackageFormat.GlobalVar) => (NoValue(), memVal.base.base, Some(staticFieldRef(memVal)))
            }

            val host = resolver.typeSig(memBase.`type`, func)

            host match {
              case host: SignatureType.NullableWrapper =>
                pkg.getType[Table](memBase.`type`) match {
                  case t: PackageFormat.CustomType => pkg.getDef[Table](t.customTypeDef) match {
                    case d: PackageFormat.EnumDef =>
                      resolver.enumKind(d) match {
                        case EnumKind.OptionLike(base) =>
                          val nullIsFalse = pkg.getType[FuncType](d.ctors(0).funcType).base.argTysLength == 1
                          val cond = if (nullIsFalse) Condition.NE else Condition.EQ
                          state(e) = CondVal(Cmp(TRefType, cond)(mem, Null()))
                        case _ =>
                      }
                    case _ =>
                  }
                  case _ =>
                }

              case _ =>
                staticField match {
                  case None =>
                    nullCheck(mem)
                  case Some(sf) =>
                    val symRefType = asClassType(sf.refType)
                    ensurePrepared(symRefType)
                    packageInitCheck(symRefType)
                }

                val fields = fieldChain(host, e.pathVector.toSeq)

                val lastField = fields.last
                val n = if (lastField.fieldType.isZST) {
                  // do nothing
                  NoValue()

                } else if (lastField.fieldType.isRecord) {
                  staticField match {
                    case None => GetFieldSeqRef(fields)(mem)
                    case Some(sf) => GetStaticFieldSeqRef(sf +: fields)
                  }

                } else {
                  staticField match {
                    case None => LoadFieldSeq(fields)(mem)
                    case Some(sf) => LoadStaticFieldSeq(sf +: fields)
                  }
                }
                state(e) = n
            }

          case e: PackageFormat.ApplyBase =>
            val (target, argVals, refType) = operands(e.base.base) match {
              case Seq(func: Function, argVals: _*) =>
                val declType = Option(pkg.getDef[Table](func.base.declaredParent))
                  .flatMap(resolver.symType)
                  .map(asClassType)
                  .getOrElse(resolver.findClass(func.base.packageName).get)

                val refType = Option.when(e.base.objType != 0)(e.base.objType)
                  .map(resolver.typeSig(_, interpreter.func))

                val refClass = refType
                  .filter(t => t.isReference || t.isRecord)
                  .getOrElse(SignatureType.fromSymType(declType))

                val name = resolver.symName(func)
                val target = calcMethodRef(declType, refClass, name, func, func.funcKind, func.base.base.base.attributes)

                // TODO: add instantiated type parameters to base MethodReference
                val lparams = e.base.instantiatedTypeArgsVector.toSeq.map(resolver.typeSig(_, interpreter.func))
                val targetWithUGContext = if (lparams.nonEmpty) {
                  target.toInstantiatedMethodReference(lparams, refClass)
                } else {
                  target
                }
                val nonBlockArgVals = e.base.base.kind match {
                  case CHIRExprKind.TryApply => argVals.dropRight(2)
                  case CHIRExprKind.Apply => argVals
                }
                (targetWithUGContext, nonBlockArgVals, refType)
            }

            val skipCall = env.enabled(SkipCHIRGarbageCalls) && target.methodName.toString.startsWith("_CGFatU")
            if (!skipCall) {
              val args = argVals match {
                case Seq(gv: PackageFormat.GlobalVar, rest: _*) =>
                  GetStaticFieldSeqRef(Seq(staticFieldRef(gv))) +: rest.map(state.apply)
                case _ =>
                  argVals.map(state.apply)
              }
              val paramTypes = argVals.map {
                case x: PackageFormat.LocalVar => x.base.`type`
                case x: PackageFormat.GlobalVar => x.base.base.`type`
                case x: PackageFormat.Parameter => x.base.`type`
              } map (resolver.typeSig(_, interpreter.func))
              val retType = resolver.typeSig(e.base.base.resultTy, func)
              val call = callMethod(target, refType, retType, paramTypes, args)
              state(e) = call
            }

          case e: PackageFormat.InvokeBase =>
            val refType = resolver.typeSig(e.base.objType, interpreter.func)
            val func = e.virMethodCtx
            val name = resolver.symName(func)
            val (sig, _, _) = resolver.functionSig(func.funcType, e, hasReceiver = true)

            // TODO: unify logic with calcMethodRef?
            val refClass = asClassType(refType)
            val method = refClass.findDeclaredMethodOrNull(xstr(name), sig)

            val vtable = refClass.getCHIRVTable
            val cparams = genericParams(refType)
            val vnum = vtable.extDefs.find(_.extType.instantiate(cparams, Seq.empty) == refType).toSeq.flatMap(_.funcTable)
              .indexWhere(m => m.name == name && m.originalSig == sig)
            assert(vnum >= 0, s"could not find VTable slot for $name${sig.toJETSignature} in $vtable")

            assert(!method.isStatic, method.getFullName)
            val target = new MethodReference(method, MAK.VIRTUAL, CompiledType(refType), vnum)

            val argVals = operands(e.base.base)
            val args = e.base.base.kind match {
              case CHIRExprKind.TryInvoke => argVals.dropRight(2).map(state.apply)
              case CHIRExprKind.Invoke => argVals.map(state.apply)
            }
            val paramTypes = operands(e.base.base).map {
              case x: PackageFormat.LocalVar => x.base.`type`
              case x: PackageFormat.GlobalVar => x.base.base.`type`
              case x: PackageFormat.Parameter => x.base.`type`
            } map (resolver.typeSig(_, interpreter.func))
            val retType = resolver.typeSig(e.base.base.resultTy, e)
            val call = callMethod(target, Some(refType), retType, paramTypes, args)
            state(e) = call

          //case e: PackageFormat.InvokeBase =>
          case e: PackageFormat.InstanceOf =>
            val tpe = resolver.typeSig(e.targetType, func)
            state(e) = operands(e.base).map(state.apply) match {
              case Seq(obj) => InstanceOf(tpe)(obj)
            }

          case e: PackageFormat.NumericCastBase =>
            import BitFieldExtract.*
            import SignatureType.*
            import AsmType.*

            val (from, value) = operands(e.base) match {
              case Seq(fromVar: PackageFormat.LocalVar, /*Exception targets*/ _*) =>
                (resolver.typeSig(fromVar.base.`type`, func), state(fromVar))
              case Seq(fromVar: PackageFormat.Parameter, /*Exception targets*/ _*) =>
                (resolver.typeSig(fromVar.base.`type`, func), state(fromVar))
            }
            val to = resolver.typeSig(e.base.resultTy, func)

            val fromAsm = from.toAsm
            val toAsm = to.toAsm

            val fromTpe = ValueType.fromSig(from)
            val toTpe = ValueType.fromSig(to)

            e.overflowStrategy match {
              case PackageFormat.OverflowStrategy.WRAPPING | PackageFormat.OverflowStrategy.NA => // ok
              case PackageFormat.OverflowStrategy.THROWING => // TODO: do we need to support it?
              case PackageFormat.OverflowStrategy.SATURATING => notImplemented("saturating type cast")
              case PackageFormat.OverflowStrategy.CHECKED => shouldNotReachHere("checked type cast")
            }

            val n = (from, to) match {
              case (from: (Reference | InstantiatedReference), to: (Reference | InstantiatedReference)) =>
                // TODO: remove this case when numeric cast will handle only *numeric* types
                CheckCast(to, trusted = true)(value)
                value

              case (from: (Record | InstantiatedRecord), to: (Record | InstantiatedRecord)) =>
                // TODO: check something
                ReinterpretCast(fromTpe, toTpe)(value)

              case (from: (Record | InstantiatedRecord), to: Tuple) =>
                // TODO: assert from is enum!
                ReinterpretCast(fromTpe, toTpe)(value)

              case (from: Integral, to: Integral) =>
                BFX(toTpe, 0, (from.bits min to.bits), signExtension = from.signed, value)

              case (from: Integral, UnicodeChar32) =>
                BFX(toTpe, 0, (from.bits min 32), signExtension = from.signed, value)

              case (UnicodeChar32, to: Integral) =>
                BFX(toTpe, 0, (32 min to.bits), signExtension = true, value)

              case (from: Integral, to: FloatingPoint) =>
                val fpToAsm = if (toAsm == F16) F32 else toAsm
                val res = fromAsm match {
                  case U64 =>
                    val proc = fpToAsm match {
                      case F32 => RTSProc.JR_ul2f
                      case F64 => RTSProc.JR_ul2d
                      case _ => shouldNotReachHere(toAsm)
                    }
                    RTSCall(proc)(value)

                  case U32 =>
                    // Non-long values could be zero-extended to bigger signed type and then converted to float.
                    val i64 = BFX(LongType, 0, from.bits, signExtension = false, value)
                    ValueConvert(I64, fpToAsm)(i64)

                  case _ =>
                    val (adjFromAsm, adjValue) = if (fromAsm.isShortIntegral) {
                      (I32, BFX(IntType, 0, fromAsm.sizeInBits, signExtension = fromAsm.signed, value))
                    } else {
                      (fromAsm, value)
                    }
                    ValueConvert(adjFromAsm, fpToAsm)(adjValue)
                }

                if (toAsm == F16) ValueConvert(F32, F16)(res) else res

              case (from: FloatingPoint, to: Integral) =>
                val (fpFromAsm, fpValue) = if (fromAsm == F16) {
                  (F32, ValueConvert(F16, F32)(value))
                } else {
                  (fromAsm, value)
                }

                toAsm match {
                  case U64 =>
                    val proc = fpFromAsm match {
                      case F32 => RTSProc.JR_f2ul
                      case F64 => RTSProc.JR_d2ul
                      case _ => shouldNotReachHere(fromAsm)
                    }
                    RTSCall(proc)(fpValue)

                  case U32 =>
                    // Value could be converted to bigger signed type and then zero-extended to target type.
                    val i64 = ValueConvert(fpFromAsm, I64)(fpValue)
                    BitFieldExtract.Truncate(i64)

                  case _ =>
                    val adjToAsm = if (toAsm.isShortIntegral) I32 else toAsm
                    ValueConvert(fpFromAsm, adjToAsm)(fpValue)
                }

              case (from: FloatingPoint, to: FloatingPoint) =>
                ValueConvert(fromAsm, toAsm)(value)

              case (NullableWrapper(x), to @ Tuple(Seq(Boolean, y))) if x == y =>
                // FIXME: inverted?
                allocTuple(to, Seq(IConst(1), value))

              case _ => notImplemented(s"cast from ${from.toJETSignature} to ${to.toJETSignature}")
            }
            state(e) = n

          case e: PackageFormat.Branch =>
            operands(e.base) match {
              case Seq(selectorVar: PackageFormat.LocalVar, trueBlock: PackageFormat.Block, falseBlock: PackageFormat.Block) =>
                val sig = resolver.typeSig(selectorVar.base.`type`, func)
                val selector = BitFieldExtract.BFX(IntType, 0, sig.toAsm.sizeInBits, signExtension = false, state(selectorVar))
                val cond = Cmp(selector.tpe, Condition.NE)(selector, IConst(0))
                val branch = block.blockEnd.asInstanceOf[If]
                val proxy = branch.selector
                assert(proxy.isInstanceOf[Proxy] && proxy.singleUse == branch)
                proxy.replaceBy(cond)
            }

          //case e: PackageFormat.MultiBranch =>

          case e: PackageFormat.IntrinsicBase =>
            e.intrinsicKind match {
              case PackageFormat.IntrinsicKind.PREINITIALIZE =>
              // no-op

              case PackageFormat.IntrinsicKind.BEGIN_CATCH =>
                operands(e.base.base) match {
                  case Seq(ex: PackageFormat.LocalVar) =>
                    state(e) = state(ex)
                }

              case PackageFormat.IntrinsicKind.ARRAY_GET_UNCHECKED |
                   PackageFormat.IntrinsicKind.ARRAY_GET_REF_UNCHECKED |
                   PackageFormat.IntrinsicKind.ARRAY_GET =>
                // TODO: ArrayIndexCheck
                val (arrayType, obj, idx) = operands(e.base.base) match {
                  case Seq(obj: PackageFormat.LocalVar, idx) =>
                    (resolver.typeSig(obj.base.`type`, func), state(obj), state(idx))
                  case Seq(obj: PackageFormat.Parameter, idx) =>
                    (resolver.typeSig(obj.base.`type`, func), state(obj), state(idx))
                }
                val elemType = arrayType.getArrayElemType
                val n = if (elemType.isZST) {
                  NoValue()
                } else {
                  val get = ArrayGet(arrayType)(obj, idx)
                  if (needsCopy(elemType)) {
                    val local = StackAlloc.Local(elemType)
                    copy(elemType, local, get)
                    local
                  } else {
                    get
                  }
                }
                state(e) = n

              case PackageFormat.IntrinsicKind.ARRAY_SET_UNCHECKED |
                   PackageFormat.IntrinsicKind.ARRAY_SET =>
                // TODO: ArrayIndexCheck
                val (arrayType, obj, idx, value) = operands(e.base.base) match {
                  case Seq(obj: PackageFormat.LocalVar, idx, value) =>
                    (resolver.typeSig(obj.base.`type`, func), state(obj), state(idx), state(value))
                  case Seq(obj: PackageFormat.Parameter, idx, value) =>
                    (resolver.typeSig(obj.base.`type`, func), state(obj), state(idx), state(value))
                }
                arrayPut(arrayType, obj, idx, value)

              case PackageFormat.IntrinsicKind.ARRAY_SIZE =>
                operands(e.base.base).map(state.apply) match {
                  case Seq(obj) => state(e) = CangjieArrayLength(obj)
                }

              case PackageFormat.IntrinsicKind.ATOMIC_LOAD =>
                val (refType, obj) = operands(e.base.base) match {
                  case Seq(localVar: PackageFormat.LocalVar, memoryOrder) =>
                    (resolver.typeSig(localVar.base.`type`, func), state(localVar))
                }
                val Seq(value) = declaredFields(refType)
                // TODO: atomic?
                state(e) = LoadFieldSeq(Seq(value))(obj)

              case PackageFormat.IntrinsicKind.ATOMIC_FETCH_ADD =>
                val (refType, obj, addend) = operands(e.base.base) match {
                  case Seq(localVar: PackageFormat.LocalVar, addend, memoryOrder) =>
                    (resolver.typeSig(localVar.base.`type`, func), state(localVar), state(addend))
                }
                val Seq(value) = declaredFields(refType)
                // TODO: atomic?
                val prevValue = LoadFieldSeq(Seq(value))(obj)
                val newValue = Add(prevValue, addend)
                StoreFieldSeq(Seq(value))(obj, newValue)
                state(e) = prevValue

              case PackageFormat.IntrinsicKind.ATOMIC_SWAP =>
                val (refType, obj, newValue) = operands(e.base.base) match {
                  case Seq(localVar: PackageFormat.LocalVar, newValue, memoryOrder) =>
                    (resolver.typeSig(localVar.base.`type`, func), state(localVar), state(newValue))
                }
                val Seq(value) = declaredFields(refType)
                // TODO: atomic?
                val prevValue = LoadFieldSeq(Seq(value))(obj)
                StoreFieldSeq(Seq(value))(obj, newValue)
                state(e) = prevValue

              case PackageFormat.IntrinsicKind.SQRT =>
                operands(e.base.base).map(state.apply) match {
                  case Seq(x) =>
                    val kind = if (x.tpe == DoubleType) Java.Lang.MathIntrinsic.D_SQRT else Java.Lang.MathIntrinsic.F_SQRT
                    state(e) = MathIntrinsic(kind)(x)
                }
            }

          case e: PackageFormat.SpawnBase =>
            operands(e.base).map(state.apply) match {
              case Seq(future) =>
                pkg.getValue[Table](e.executeClosure) match {
                  case null =>
                    val retType = resolver.typeSig(e.base.resultTy, func)
                    state(e) = SpawnFuture(retType)(future)
                  case _ =>
                    notImplemented("SpawnClosure")
                }
            }


          case e: PackageFormat.Debug =>
            // TODO: support debug

          case e: PackageFormat.Expression => e.kind match {

            case CHIRExprKind.Constant =>
              val value = singleElement(operands(e)) match {
                case v: PackageFormat.BoolLiteral => IConst(if (v.`val`) 1 else 0)
                case v: PackageFormat.UnitLiteral => NoValue()
                case v: PackageFormat.NullLiteral => IntegralConst(AddrType)(0) // Not sure if this is always raw address
                case v: PackageFormat.RuneLiteral => IConst(v.`val`.toInt)
                case v: PackageFormat.IntLiteral =>
                  import PackageFormat.CHIRTypeKind.*
                  val t = pkg.getType[PackageFormat.Type](v.base.base.`type`)
                  val tpe = t.kind match {
                    case INT8 | INT16 | INT32 | UINT8 | UINT16 | UINT32 => IntType
                    case INT64 | INT_NATIVE | UINT64 | UINT_NATIVE => LongType
                  }
                  IntegralConst(tpe)(v.`val`)
                case v: PackageFormat.FloatLiteral =>
                  import PackageFormat.CHIRTypeKind.*
                  val t = pkg.getType[PackageFormat.Type](v.base.base.`type`)
                  t.kind match {
                    case FLOAT16 => notImplemented(s"FLOAT16: ${v.`val`}")
                    case FLOAT32 => FConst(v.`val`.toFloat)
                    case FLOAT64 => DConst(v.`val`)
                  }
                case v: PackageFormat.StringLiteral =>
                  val sigType = fromSymType(resolver.findClass("std.core:String").get)
                  val xstring = currentInlineContext.method.getDeclaringClass.getConstString(xstr(v.`val`))

                  val sa = StackAlloc.Local(sigType)
                  InitStringRecord(sigType, isStatic = false, xstring)(sa)
                  sa
              }
              state(e) = value

            case CHIRExprKind.Load =>
              operands(e) match {
                case Seq(localVar: PackageFormat.LocalVar) =>
                  val sig = resolver.typeSig(localVar.base.`type`, func)
                  if (sig.isZST) {
                    // nothing to do

                  } else {
                    val n = state(localVar) match {
                      case GetFieldSeqRef(fields, base) =>
                        LoadFieldSeq(fields)(maybeDerivedPtr(base))
                      case GetStaticFieldSeqRef(fields) =>
                        LoadStaticFieldSeq(fields)
                      case mem =>
                        if (sig.isRecord || sig.isTraceableReference || sig.isPrimitive) {
                          mem
                        } else {
                          LoadMemory(sig.toAsm, sig, atomic = false)(mem)
                        }
                    }
                    state(e) = n
                  }
                case Seq(globalVar: PackageFormat.GlobalVar) =>
                  val field = staticFieldRef(globalVar)
                  val n = if (needsCopy(field.fieldType)) {
                    val local = StackAlloc.Local(field.fieldType)
                    val addr = GetStaticFieldSeqRef(Seq(field))
                    copy(field.fieldType, local, addr)
                    local
                  } else {
                    LoadStaticFieldSeq(Seq(field))
                  }
                  state(e) = n
              }

            case CHIRExprKind.Store =>
              operands(e) match {
                case Seq(valueVar: (PackageFormat.LocalVar | PackageFormat.Parameter), localVar: PackageFormat.LocalVar) =>
                  val sig = resolver.typeSig(localVar.base.`type`, func)
                  if (sig.isZST) {
                    // nothing to do

                  } else {
                    val value = state(valueVar)
                    val mem = state(localVar)
                    writeBarrier()
                    if (needsCopy(sig)) {
                      value match {
                        case ZeroValueNode() =>
                          // TODO: zeroing?
                        case _ =>
                          copy(sig, mem, value)
                      }

                    } else {
                      mem match {
                        case GetFieldSeqRef(fields, base) =>
                          StoreFieldSeq(fields)(maybeDerivedPtr(base), value)
                        case GetStaticFieldSeqRef(fields) =>
                          StoreStaticFieldSeq(fields)(value)
                        case mem =>
                          if (sig.isTraceableReference || sig.isPrimitive) {
                            state(localVar) = value
                          } else {
                            val value0 = value match {
                              case value: LoadMemory =>
                                // TODO: come up with better assertions
                                UniversalGeneric.convertHolder(value.signature, sig)(value)
                              case v => v
                            }
                            StoreMemory(sig.toAsm, sig, atomic = false)(mem, value0)
                          }
                      }
                    }
                  }
                case Seq(valueVar: (PackageFormat.LocalVar | PackageFormat.Parameter), globalVar: PackageFormat.GlobalVar) =>
                  val staticField = staticFieldRef(globalVar)
                  val sig = staticField.fieldType
                  if (sig.isZST) {
                    // nothing to do

                  } else {
                    val symRefType = asClassType(staticField.refType)

                    ensurePrepared(symRefType)
                    packageInitCheck(symRefType)

                    val value = state(valueVar)
                    if (needsCopy(staticField.fieldType)) {
                      val addr = GetStaticFieldSeqRef(Seq(staticField))
                      copy(sig, addr, value)
                    } else {
                      writeBarrier()
                      StoreStaticFieldSeq(Seq(staticField))(value)
                    }
                  }
              }

            case CHIRExprKind.Goto =>
              assert(block.blockEnd.isInstanceOf[Goto])

            case CHIRExprKind.Exit =>
              val retType = rootMethod.getReturnType
              val retVal = if (retType.isZST) {
                Void()
              } else {
                val r = pkg.getValue[Table](func.retVal)
                if (r == null) {
                  Void()
                } else if (rootMethod.hasRetByValParameter) {
                  val retByVal = rootMethodParam(rootMethod.getRetByValArgIdx)
                  if (needsCopy(retType)) {
                    copy(retType, retByVal, state(r))
                    retByVal
                  } else {
                    val value = state(r)
                    StoreMemory(retType.toAsm, retType, atomic = false)(retByVal, value)
                    value
                  }
                } else {
                  r match {
                    case r: PackageFormat.LocalVar =>
                      val t = resolver.typeSig(r.base.`type`, func)
                      if (t.isTraceableReference || t.isPrimitive) {
                        state(r)
                      } else {
                        assert(!t.isRecord)
                        LoadMemory(t.toAsm, t, atomic = false)(state(r))
                      }
                  }
                }
              }
              val ret = block.blockEnd.asInstanceOf[Return]
              val proxy = ret.inValue
              assert(proxy.isInstanceOf[Proxy] && proxy.singleUse == ret)
              proxy.replaceBy(retVal)

            case CHIRExprKind.RaiseException =>
              assert(block.blockEnd.isInstanceOf[Halt])
              operands(e) match {
                case Seq(ex, _*) => Throw(state(ex))
              }

            case CHIRExprKind.Tuple =>
              pkg.getType[Table](e.resultTy) match {
                case t: PackageFormat.Type =>
                  t.kind match {
                    case CHIRTypeKind.TUPLE =>
                      val tupleType = resolver.typeSig(e.resultTy, func).asInstanceOf[SignatureType.Tuple]
                      state(e) = allocTuple(tupleType, operands(e).map(state.apply))
                  }

                case t: PackageFormat.CustomType =>
                  t.base.kind match {
                    case CHIRTypeKind.ENUM =>
                      val enumDef = pkg.getDef[EnumDef](t.customTypeDef)
                      resolver.enumKind(enumDef) match {
                        case EnumKind.ZeroSized => // nothing to do
                        case EnumKind.PrimitiveBased => state(e) = operands(e).map(state.apply) match {
                          case Seq(c: IConst) => c
                        }
                        case EnumKind.OptionLike(base) =>
                          resolver.typeSig(e.resultTy, func) match {
                            case _: SignatureType.NullableWrapper =>
                              state(e) = operands(e).map(state.apply) match {
                                case Seq(IConst(c)) => assert(c == 0 || c == 1, c); Null()
                                case Seq(IConst(c), x) => assert(c == 0 || c == 1, c); x
                              }
                            case sig =>
                              val mem = StackAlloc.Local(sig, workaroundForNonZeroedTraceableRecords = true)
                              val Seq(tag, payload) = declaredFields(sig)
                              operands(e).map(state.apply) match {
                                case Seq(IConst(c)) =>
                                  assert(c == 0 || c == 1, c)
                                  StoreFieldSeq(Seq(tag))(mem, IConst(c))

                                case Seq(IConst(c), x) =>
                                  assert(c == 0 || c == 1, c)
                                  StoreFieldSeq(Seq(tag))(mem, IConst(c))
                                  if (payload.fieldType.isZST) {
                                    // nothing to do

                                  } else if (needsCopy(payload.fieldType)) {
                                    val addr = GetFieldSeqRef(Seq(payload))(mem)
                                    copy(payload.fieldType, addr, x)

                                  } else {
                                    StoreFieldSeq(Seq(payload))(mem, x)
                                  }
                              }
                              state(e) = mem
                          }
                        case EnumKind.UnionBased =>
                          operands(e).map(state.apply) match {
                            case args @ Seq(IConst(c), _*) =>
                              assert(c >= 0, c)
                              val constrFuncType = pkg.getType[FuncType](enumDef.ctors(c).funcType)
                              val constrTypes = constrFuncType.base.argTysVector.toSeq.init.map(resolver.typeSig(_, func))
                              val tupleType = SignatureType.Tuple(SignatureType.UInt32 +: constrTypes)
                              val enumType = resolver.typeSig(e.resultTy, func)
                              state(e) = ReinterpretCast(ValueType.fromSig(tupleType), ValueType.fromSig(enumType))(
                                allocTuple(tupleType, args)
                              )
                          }
                        case EnumKind.ClassBased => notImplemented(s"class-based enum ${enumDef.base.identifier}")
                      }
                  }
              }

            case CHIRExprKind.GetException =>
              state(e) = block.asInstanceOf[XBlock].catchNode

            case CHIRExprKind.RawArrayInitByValue =>
              val (array, len, value) = operands(e).map(state.apply) match {
                case Seq(array: NewArray, len, value) => (array, len, value)
              }
              assert(array.lengths == Seq(len), s"RawArrayInitByValue($array, $len, $value)")
              val arrayType = array.allocType
              if (arrayType.getArrayElemType.isZST) {
                stats.count(StatsKind.ArrayZeroingElimination, "Unit array zeroing eliminated on parsing", array)
              } else {
                AJArrayFill(arrayType, arrayType.getArrayElemType)(array, value)
              }

            case CHIRExprKind.RawArrayLiteralInit =>
              val (array, values) = operands(e).map(state.apply) match {
                case Seq(array: NewArray, values: _*) => (array, values)
              }
              assert(array.lengths == Seq(LConst(values.size)), s"RawArrayLiteralInit($array, $values)")
              val arrayType = array.allocType
              if (arrayType.getArrayElemType.isZST) {
                // Nothing to do

              } else {
                // TODO: Support ArrayFill
                for ((value, idx) <- values.zipWithIndex) {
                  arrayPut(arrayType, array, LConst(idx), value)
                }
              }

            case k => notImplemented(CHIRExprKind.name(k))
          }
        }
      }
    }

    private def staticFieldRef(globalVar: PackageFormat.GlobalVar): CangjieFieldReference = {
      val decl = pkg.getDef[Table](globalVar.base.declaredParent)
      val symRefType = if (decl == null) {
        resolver.findClass(globalVar.base.packageName).get
      } else {
        asClassType(resolver.symType(decl).get)
      }

      val refType = fromSymType(symRefType)
      val name = resolver.symName(globalVar)
      val sig = resolver.typeSig(globalVar.base.base.`type`, func)
      val f = symRefType.findDeclaredFieldOrNull(xstr(name), sig) ensuring
        (_ != null, s"cannot find field '$name' with signature '${sig.toJETSignature}' in class '${symRefType.getName}'")

      CangjieFieldReference(f.getFieldIndex, Some(f), refType, f.getType)
    }

    private def calcMethodRef(declType: SymClassType, refType: SignatureType, _name: String,
                              func: Function, funcKind: Int, attributes: Long): MethodReference = {
      val isStatic = declType.isCangjiePackage || (Attribute.STATIC in attributes) || resolver.isExtendedBaseFunc(func)
      val (sig, isCFunc, vararg) = resolver.functionSig(func, hasReceiver = !isStatic)

      // TODO: explain
      val name = if (!isStatic && declType.isVariableSizeType) {
        resolver.mutWithoutTI(_name)
      } else {
        _name
      }

      val method = refType match {
        case refType: SignatureType.InstantiatedType if !resolver.isExtendedBaseFunc(func) =>
          val cparams = refType.instantiatedTypeParameters
          val lparams = Seq.empty[SignatureType] //FIXME
          declType.findDeclaredMethodOrNullWithSigEq(xstr(name), sig, MethodSignature.equalInstantiated(cparams, lparams))
        case _ =>
          // TODO: generic extend funcs
          declType.findDeclaredMethodOrNull(xstr(name), sig)

      }
      assert(method != null, s"cannot find method '$name' with signature '${sig.toJETSignature}' in class '${declType.getName}'")

      val mak =
        if (method.isCangjieMut) {
          MAK.MUT
        } else if (isStatic) {
          MAK.STATIC
        } else if (Attribute.VIRTUAL in attributes) {
          MAK.VIRTUAL
        } else {
          MAK.SPECIAL
        }
      val target = new MethodReference(method, mak, CompiledType(refType))
      assert(vararg == target.method.isVarArgs)
      if (vararg) {
        notImplemented("vararg")
      } else {
        target
      }
    }

    private def calcABIArgs(target: MethodReference, args: Seq[Node])(paramNode: SpecialParameter => Iterable[Node]): Seq[Node] = {
      val specialParams = target.methodType.specialParameters
      val startArgs = specialParams.specialParametersStart.flatMap(paramNode).toSeq
      val endArgs = specialParams.specialParametersEnd.flatMap(paramNode).toSeq
      // TODO: make it more systematic
      val genericParams = if (specialParams.contains(GenericFuncParams)) {
        paramNode(GenericFuncParams)
      } else {
        Seq.empty
      }
      startArgs ++ args ++ genericParams ++ endArgs
    }

    private def callMethod(target: MethodReference, refType: Option[SignatureType], retType: SignatureType, _paramTypes: Seq[SignatureType], _args: Seq[Node]): Node = {
      import SpecialParameter.*

      val (receiver, args, paramTypes) = if (!target.hasMethod || target.method.isStatic) {
        (None, _args, _paramTypes)
      } else {
        (_args.headOption, _args.tail, _paramTypes.tail)
      }

      val ugArgs = for ((a, (from, to)) <- args zip (paramTypes zip target.method.getSignature.parameterTypes.map(ABI.makeABISigType)))
        yield (from, to) match {
          case (_: SignatureType.Box, _: SignatureType.Box) => a // no adjustment necessary
          case (from: SignatureType.Box, _) => shouldNotReachHere((from, to))

          case (from: SignatureType.TypeVariable, to: SignatureType.Box) => notImplemented((from, to))
          case (from, to: SignatureType.Box) if from.isTraceableReference => a
          case (from, to: SignatureType.Box) => Box(from)(LoadTypeInfo(from), a)

          case (_, _) => a
        }

      lazy val abiRetValType = target.methodType.parameterType(target.methodType.getRetByValArgIdx)

      var abiRetVal: Node = null
      val abiArgs = calcABIArgs(target, ugArgs) {
        case RetByVal | CFuncRetByVal =>
          abiRetVal = abiRetValType match {
            case _ if abiRetValType.isZST =>
              assert(!target.methodType.hasCFuncRetByValParameter)
              Void()

            case t: SignatureType.Box =>
              // Variable-sized type
              val baseType = retType match {
                case rt: SignatureType.NullableWrapper =>
                  notImplemented(s"Option<T> of reference type sret: ${retType.toJETSignature}")
                case _ => retType
              }
              New(SignatureType.Box(baseType))()

            case SignatureType.Address =>
              // Type variable
              // TODO: prepareSRet
              val memType = ReferenceType.cangjieStdCoreObject.sigType
              val mem = StackAlloc.Local(memType, workaroundForNonZeroedTraceableRecords = true)
              assert(!retType.isInstanceOf[SignatureType.NullableWrapper], s"unsupported return type T as Option<T>: $retType")
              val value = if (retType.isTraceableReference) {
                Null()
              } else {
                New(SignatureType.Box(retType))()
              }
              StoreMemory(memType.toAsm, memType, atomic = false)(mem, value)
              mem

            case _ =>
              assert(abiRetValType.isRecord)
              assert(retType.isRecord)
              StackAlloc.Local(retType, workaroundForNonZeroedTraceableRecords = true)
          }

          Seq(abiRetVal)
        case Receiver => receiver ensuring (_.nonEmpty)
        case SMutRecord => receiver map maybeDerivedPtr ensuring (_.nonEmpty)
        case SMutObject => receiver.map {
          case rcv: InstanceFieldSeqOperation => rcv.obj
          case rcv: FieldSeqOperation => DerivedPtr.Global()
          case rcv: StackAlloc => DerivedPtr.Local()
          case rcv: Param =>
            assert(rcv.num == rootMethod.getMutRecordArgIdx)
            rootMethodParam(rootMethod.getMutObjectArgIdx)
        } ensuring (_.nonEmpty)
        case OuterTypeInfo =>
          Seq(LoadTypeInfo(refType.get))
        case SpecialParameter.ThisTypeInfo =>
          Seq(LoadTypeInfo(refType.get))
        case GenericFuncParams =>
          target.asInstanceOf[InstantiatedMethodReference].instantiatedTypeParameters.map { t =>
            LoadTypeInfo(t)
          }
        case x @ (MutRecord | MutObject | UGDesc | GenericFuncParams) =>
          shouldNotReachHere(x)
      }

      ensurePrepared(PreparationRequired.forInvoke(target))

      packageInitCheck(target.refClass)

      val call = if (target.methodType.hasThisTypeInfoParameter && target.accessKind == MAK.STATIC_VIRTUAL) {
        InvokeVirtualStatic(target)(abiArgs: _*)
      } else {
        Invoke(target)(abiArgs: _*)
      }

      if (abiRetVal != null) {
        abiRetValType match {
          case _ if abiRetValType.isZST =>
            abiRetVal

          case t: SignatureType.Box =>
            // Variable-sized type
            val baseType = retType match {
              case rt: SignatureType.NullableWrapper =>
                notImplemented(s"Option<T> of reference type sret: ${retType.toJETSignature}")
              case _ => retType
            }
            New(SignatureType.Box(baseType))()

          case SignatureType.Address =>
            // Type variable
            // TODO: prepareSRet
            val memType = ReferenceType.cangjieStdCoreObject.sigType
            val obj = LoadMemory(memType.toAsm, memType, atomic = false)(abiRetVal)
            if (retType.isTraceableReference) {
              obj
            } else {
              Unbox(retType)(LoadTypeInfo(retType), obj)
            }

          case _ =>
            assert(abiRetValType.isRecord)
            abiRetVal
        }
      } else {
        call
      }
    }

    private def inlinedCall(target: MethodReference)(args: Node*) = {
      assert(target.method.isAJInline, s"${target}")
      assert(!target.isInterfCall)
      Invoke(target)(args: _*)
    }

    private def genericParams(x: SignatureType): Seq[SignatureType] = x match {
      case x: SignatureType.InstantiatedType => x.instantiatedTypeParameters
      case _ => Seq.empty
    }

    private def fieldChain(host: SignatureType, path: Seq[Long]): Seq[CangjieFieldReference] = {
      path.scanLeft[CangjieFieldReference](null) { case (fr, idx) =>
        val refType = if (fr == null) host else fr.fieldType
        refType match {
          case refType: SignatureType.Tuple =>
            val fieldType = refType.params(idx.toInt)
            CangjieFieldReference(idx, None, refType, fieldType)
          case refType =>
            val next = asClassType(refType).getFields.toArray.apply(idx.toInt)
            val fieldType = next.getType.instantiate(genericParams(refType), Seq.empty)
            CangjieFieldReference(idx, Some(next), refType, fieldType)
        }
      }.drop(1) // drop first null value
    }

    private def declaredFields(host: SignatureType): Seq[CangjieFieldReference] = {
      asClassType(host).getDeclaredFields.toSeq map { f =>
        CangjieFieldReference(f.getFieldIndex, Some(f), host, f.getType.instantiate(genericParams(host), Seq.empty))
      }
    }

    private def nullCheck(n: Node): Unit = {
      if (n.tpe.isTraceableRefType) {
        NullCheck(trusted = true)(n)
      }
    }

    /** Inserts package initialization check before load of global or static variable from another package. */
    private def packageInitCheck(klass: SymClassType): Unit = {
      val anotherPackage = klass.getCangjiePackage
      val thisPackage = method.getDeclaringClass.getCangjiePackage
      if (anotherPackage != null && anotherPackage != thisPackage) {
        PackageInitCheck(anotherPackage)()
      }
    }

    private def arrayPut(arrayType: SignatureType, obj: Node, idx: Node, value: Node): Unit = {
      val elemType = arrayType.getArrayElemType
      if (elemType.isZST) {
        // nop

      } else if (needsCopy(elemType)) {
        val addr = ArrayGet(arrayType)(obj, idx)
        copy(elemType, addr, value)

      } else {
        ArrayPut(arrayType)(obj, idx, value)
      }
    }

    private def maybeDerivedPtr(rcv: Node): Node = rcv match {
      case rcv: Param if rootMethod.hasMutRecordParameter && rcv.num == rootMethod.getMutRecordArgIdx =>
        DerivedPtr(rootMethod.getMutRecordType)(rootMethodParam(rootMethod.getMutObjectArgIdx), rcv)
      case rcv => rcv
    }

    private def allocTuple(tupleType: SignatureType.Tuple, args: Seq[Node]): Node = {
      val mem = StackAlloc.Local(tupleType)
      for (((arg, i), sig) <- args.zipWithIndex zip tupleType.params) {
        val fieldRef = CangjieFieldReference(i, None, tupleType, sig)
        if (sig.isZST) {
          // Nothing to do

        } else if (needsCopy(sig)) {
          val tupleField = GetFieldSeqRef(Seq(fieldRef))(mem)
          copy(sig, tupleField, arg)

        } else {
          StoreFieldSeq(Seq(fieldRef))(mem, arg)
        }
      }
      mem
    }

    private def needsCopy(sig: SignatureType): Boolean = {
      sig.isRecord
    }

    private def copy(sig: SignatureType, to: Node, from: Node): Node = {
      assert(sig.isRecord, sig)
      CopyStructure(sig)(to, from)
    }

    private def writeBarrier(): Unit = {
      // FIXME
    }
  }
}
