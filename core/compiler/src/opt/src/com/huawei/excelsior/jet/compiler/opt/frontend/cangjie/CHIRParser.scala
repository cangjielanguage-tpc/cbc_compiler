/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.frontend.cangjie

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.compiler.{PreparationRequired, RTSProc, Stage, StatsKind}
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.cangjie.CHIRVTable
import com.huawei.excelsior.jet.compiler.chir.CHIR.Attribute
import com.huawei.excelsior.jet.compiler.chir.{CHIR, CHIRLoader, CHIRResolver}
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, ConstBranchElimination, Universe}
import com.huawei.excelsior.jet.compiler.opt.ir.nodes.HLIRNodes
import com.huawei.excelsior.jet.compiler.opt.middle.patterns.Arrays
import com.huawei.excelsior.jet.compiler.opt.middle.{ContextTypesRecalculation, DCEComponent, UCEComponent}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{ContextTypesInParsing, DetailedParsingLogs, FailArrayAcquireRawData, FailSaturatingArithmetic, PackageInitFromMain}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParameter
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{CangjieEnumWrapper, fromSymType}
import com.huawei.excelsior.jet.compiler.symlevel.{CangjieFieldReference, Field, InstantiatedMethodReference, Method, MethodReference, MethodSignature, MethodType, SignatureType, ClassType as SymClassType, MethodReferenceAccessKind as MAK, Type as SymType}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.util.ScalaCollections.*
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.{Closure, Numbering, ScalaCollections}

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

  def loadCHIRMethod(method: Method, args0: Seq[Node]): RTPartsInfo = stage(Stage.LoadCHIR) {
    val args = args0
    val (ret, message) = method match {
      case _ if method.isMutWrapper => (loadMutWrapper(method, args), "after load mut wrapper")
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
      assert(from.sigType.isInstanceOf[SignatureType.Tuple] || to.sigType.isInstanceOf[SignatureType.Tuple] ||
        from.sigType.getRawObjectSize == to.sigType.getRawObjectSize, s"inconsistent record type size: cast $from -> $to")
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

  private def loadMutWrapper(method: Method, args: Seq[Node]): Return = {
    val Some(source, idx) = method.getCHIRDef
    implicit val resolver: CHIRResolver = CHIRLoader.getCHIRResolver(source.toString)(env)
    implicit val pkg: CHIR.Package = resolver.pkg

    // Mut wrapper C.foo is a wrapper around method C.foo$withoutTI
    // but with boxed receiver argument instead of mut pair or single record
    // receiver in C.foo$withoutTI.
    //
    // Note: wrapped method may not be "mut" with mut pair as receiver,
    //       despite what the name "mut wrapper" might suggest.
    currentScope.inState(entryBlock, entryBlock) {
      val declType = method.getDeclaringClass
      val wrappedMethod = declType.findDeclaredMethodOrNull(xstr(resolver.mutWithoutTI(method.getName)), method.getSignature)
      assert(wrappedMethod != null)

      val rcvBoxed = args(method.getReceiverArgIdx)
      val nonRcvArgs = args.drop(method.getReceiverArgIdx + 1)

      val sret = if (method.hasRetByValParameter) {
        args.headOption
      } else {
        None
      }

      val (mutObject, rcvType, mak) = if (wrappedMethod.isCangjieMut) {
        assert(wrappedMethod.getMutRecordArgIdx == method.getReceiverArgIdx)
        (Some(rcvBoxed), wrappedMethod.getMutRecordType, MAK.MUT)
      } else {
        assert(wrappedMethod.getReceiverArgIdx == method.getReceiverArgIdx)
        (None, wrappedMethod.getParamType(wrappedMethod.getReceiverArgIdx), MAK.SPECIAL)
      }

      val payload = UnboxLea(rcvType)(rcvBoxed)

      val refType = rcvType
      val target = new MethodReference(wrappedMethod, mak, CompiledType(refType))
      val call = Invoke(target)(sret.toSeq ++ Seq(payload) ++ mutObject.toSeq ++ nonRcvArgs: _*)

      val retType = method.getReturnType
      val retVal = if (retType.isZST) {
        Void()
      } else {
        call
      }
      val ret = Return.proto(ValueType(retType))(retVal)

      dbgPrinter.debugCFG("CFG after parsing")
      dbgPrinter.debugGraphs("CFG after parsing")

      checkGraphConsistency(CheckLevels.Important, cfg)
      checkIRConsistency(CheckLevels.Important)

      ret
    }
  }

  private def loadNormal(method: Method, args: Seq[Node]): Return = {
    val Some(source, idx) = method.getCHIRDef
    implicit val resolver: CHIRResolver = CHIRLoader.getCHIRResolver(source.toString)(env)
    implicit val pkg: CHIR.Package = resolver.pkg

    val func = pkg.function(idx)

    stage(Stage.CangjieFunctionParsing) {
      val blockMap = withFreeUnreachableBlocks {
        makeCFG(method, func)
      }
      dbgPrinter.debugCFG("CFG after parsing")
      dbgPrinter.debugGraphs("CFG after parsing")

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

    resolveProxiesInArgs()

    assert(all[SMutRecArg].isEmpty, "there should be no alive SMutRecArg nodes after parsing")
    assert(all[SMutObjectArg].isEmpty, "there should be no alive SMutObjectArg nodes after parsing")

    if (eliminateUnreachableCode()) {
      dbgPrinter.debugNodes("All graph after UCE")
    }
    // Cleanup proxies
    if (eliminateDeadCode()) {
      dbgPrinter.debugNodes("All graph after DCE")
    }

    for (n <- all[Abs]) {
      replaceByCode(n) {
        val arg = n.value
        val tpe = arg.tpe
        val isNegative = If(Cmp(tpe, Condition.LT)(arg, IntegralConst(tpe)(0)))
        val b = BBlock(isNegative.trueExit, isNegative.falseExit)
        Phi(tpe)(b, Neg(tpe)(arg), arg)
      }
    }

    for (n <- all[ArrayBuiltInCopyTo]) {
      replaceByCode(n) {
        val arrayType = n.arrayType
        val idxTpe = n.srcStart.tpe

        val entryGoto = Goto()

        val loopBlock = BBlock(entryGoto)
        val idx = Phi.raw(idxTpe)(loopBlock, IntegralConst(idxTpe)(0))
        val loopContinue = If(Cmp(idxTpe, Condition.LT)(idx, n.len))

        val bodyBlock = BBlock(loopContinue.trueExit)
        val srcIdx = Add(n.srcStart, idx)
        val dstIdx = Add(n.dstStart, idx)
        if (arrayType.getArrayElemType.isRecord) {
          val srcMem = ArrayGet(arrayType)(n.src, srcIdx)
          val dstMem = ArrayGet(arrayType)(n.dst, dstIdx)
          CopyStructure(arrayType.getArrayElemType)(srcMem, dstMem)
        } else {
          val value = ArrayGet(arrayType)(n.src, srcIdx)
          ArrayPut(arrayType)(n.dst, dstIdx, value)
        }
        val bodyGoto = Goto()

        loopBlock.addArg(bodyGoto)
        idx.addArg(Add(idx, IntegralConst(idxTpe)(1)))
        commit(idx)

        BBlock(loopContinue.falseExit)
      }
    }

    assert(all[EnumCast].isEmpty, "there should be no alive EnumCast nodes after parsing")

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
    private val _blocks = mutable.LinkedHashMap.empty[CHIR.Block, Block]
    private val _blockVals = mutable.LinkedHashMap.empty[Block, CHIR.Block]

    def link(value: CHIR.Block, block: Block): Unit = {
      _blocks(value) = block
      _blockVals(block) = value
    }

    def apply(value: CHIR.Block): Block = _blocks(value)
    def apply(block: Block): CHIR.Block = _blockVals(block)

    def contains(block: Block): Boolean = _blockVals.contains(block)

    def iterator: Iterator[(CHIR.Block, Block)] = _blocks.iterator
    def blocks: Iterator[Block] = _blocks.valuesIterator
    def blockVals: Iterator[CHIR.Block] = _blocks.keysIterator
  }

  private def makeCFG(method: Method, func: CHIR.Func)(implicit pkg: CHIR.Package): BlockMap = {
    val bg = func.body.get

    // Create blocks
    val blockMap = new BlockMap
    for (v <- bg.blocks) {
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

      def succBlocks(t: CHIR.HasSuccessors): Seq[Block] = {
        t.successors.map(blockMap.apply)
      }

      def goto(t: CHIR.Goto): Unit = {
        val end = Goto(b, b)
        succBlocks(t) foreach (_ addArg end)
      }

      def branch(t: CHIR.Branch): Unit = {
        val end = If(b, b, Proxy(ConditionType)(b))
        val Seq(trueBlock, falseBlock) = succBlocks(t)
        trueBlock addArg end.trueExit
        falseBlock addArg end.falseExit
      }

      def multiBranch(v: CHIR.MultiBranch): Unit = {
        val end = Switch(v.caseValues.map(_.toInt))(b, b, Proxy(IntType)(b))
        for ((exit, target) <- end.exits zip succBlocks(v)) {
          target addArg exit
        }
      }

      def exit(): Unit = {
        val retType = ValueType.fromSig(method.getReturnType) // FIXME: EOPs
        Return(b, b, Proxy(retType)(b))
      }

      def exceptGoto(t: CHIR.HasSuccessors): Unit = {
        val anchor = HandlerAnchor(b)
        val end = Goto(anchor, b)
        val Seq(target: BBlock, handler: XBlock) = succBlocks(t)
        target addArg end
        handler addArg anchor.xpoint
      }

      def throwOp(t: CHIR.RaiseException): Unit = {
        var ctrl: ControlNode = b
        for (handler <- succBlocks(t)) {
          val anchor = HandlerAnchor(b)
          assert(handler.isInstanceOf[XBlock])
          handler addArg anchor.xpoint
          ctrl = anchor
        }
        Halt.empty()(ctrl, b)
      }

      bv.terminator match {
        case t: CHIR.Goto => goto(t)
        case _: CHIR.Exit => exit()
        case t: CHIR.RaiseException => throwOp(t)
        case t: (CHIR.TryApply |
          CHIR.TryInvoke |
          CHIR.TryIntrinsic |
          CHIR.TrySpawn |
          CHIR.TryNumericCast |
          CHIR.TryAllocate |
          CHIR.TryRawArrayAllocate |
          CHIR.TryUnary |
          CHIR.TryBinary) => exceptGoto(t)
        case t: CHIR.Branch => branch(t)
        case t: CHIR.MultiBranch => multiBranch(t)
      }
    }

    // Link prologue with entry block
    val chirEntryBlock = blockMap(bg.entryBlock)
    chirEntryBlock addArg Goto(entryBlock, entryBlock)

    blockMap
  }

  private class CHIRInterpreter(method: Method, func: CHIR.Func, blockMap: BlockMap)(implicit pkg: CHIR.Package, resolver: CHIRResolver) extends AbstractInterpreter {
    interpreter =>

    case class CatchProxy()
    private type StateValue = CHIR.Expression | CHIR.Parameter | CHIR.Value | CatchProxy

    private val entryBlockVal = func.body.get.entryBlock

    private val params = func.params

    private val catchProxy = CatchProxy()

    private val localValues = Numbering[StateValue](catchProxy +: (params ++ blockMap.blockVals.flatMap(_.expressions)))

    class State(private var locals: Array[NodeRef], _memory: NodeRef, _contextTypes: ContextTypesMap)
      extends Scope.State(null, _memory, _contextTypes) {

      protected type This = State

      override protected def forkImpl() =
        new State(locals, memory, if (contextTypes != null) contextTypes.clone() else null)

      override def makeUnreachableCopy() =
        new State(locals map (_ => NoValue()), memory, ContextTypesMap.Unreachable)

      def apply(x: StateValue): Node = {
        val v = x match {
          case x: CHIR.LocalVar => x.associatedExpr
          case x => x
        }
        val i = localValues.number(v)
        locals(i).deref ensuring (_ != null)
      }

      def update(x: StateValue, value: Node): Unit = {
        val v = x match {
          case x: CHIR.LocalVar => x.associatedExpr
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
      new State(
        Array.fill(localValues.order.size)(Invalid)
          // Set catchProxy to null...
          // This is the ugliest hack, but CHIR might have paths leading to GetException expression
          // on which there are literally no Catch nodes. However, it is somehow guaranteed
          // that dynamically uninitialized value never reaches these points using some pattern
          // generated by FE or some other bullshit like that.
          .updated(localValues.number(catchProxy), Null()),
        entryMemory,
        if (env.enabled(ContextTypesInParsing)) new ContextTypesMap() else null)
    }

    protected def interpret(block: Block, state: State): Block = {
      require(state != null)

      def processBlock(b: Block)(action: => Unit): Unit = {
        currentScope.inState(state) {
          withIdempotentDominance {
            state.add(b)
            b.blockEnd.inCtrl = null
            b.spineBackwardIsBroken = true

            // Here both memory and control dependencies become broken in this block.
            // Note that overall CFG remains valid, because block and its block end are still linked.
            // TODO: do not break IR and rework state so that it can keep this block together,
            //       while allowing insertion of new nodes in parsing and in insert code.

            action

            b.blockEnd.inCtrl = currentCtrl.asInstanceOf[UpperPoint]
            b.blockEnd.inMemory = currentMemory

            b.spineBackwardIsBroken = false
            checkConsistency(CheckLevels.Desirable)(Block.verifyBlockControlNums(b))

            if (state.contextTypes != null) {
              ContextTypesMap.setMapAt(block.blockEnd, state.contextTypes)
            }
          }
        }
      }

      val anchor = block.outCtrl match {
        case anchor: HandlerAnchor => anchor
        case x => assert(x == block.blockEnd); null
      }

      block match {
        case block: XBlock => state(catchProxy) = block.catchNode
        case _ =>
      }

      if (block == entryBlock) { // Entry block
        processBlock(block) {
          parseEntryBlock(block, state)
        }
        block

      } else if (anchor != null) { // Block with handler
        val b = blockMap(block)
        val spine = b.nonTerminatorExpressions
        val terminator = b.terminator

        // In CHIR only terminator can throw exception and requires handler,
        // all expressions before it must not go to the same handler even if they can throw
        // from our IR perspective.
        //
        // Because of this we have to parse regular expressions in separate block
        // which does not have handler anchor, and parse terminator by itself in
        // dedicated handled block.
        val goto = Block.splitBefore(anchor)

        // Parse regular expressions
        processBlock(block) {
          for (e <- spine) {
            parseExpression(e, block, state)
          }
        }

        // Parse terminator in its own block
        val terminatorBlock = goto.target
        processBlock(terminatorBlock) {
          onCommit.withCallback(registerXCtrl(_, state, anchor.xHandler)) {
            state.add(anchor)
            parseExpression(terminator, terminatorBlock, state)
          }
        }

        // Remove anchor
        strikeOut(anchor)
        terminatorBlock

      } else { // Regular block
        processBlock(block) {
          for (e <- blockMap(block).expressions) {
            parseExpression(e, block, state)
          }
        }
        block
      }
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

    private def parseEntryBlock(block: Block, state: State): Unit = {
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
        for (func <- Seq(pkg.packageInitLiteralFunc, pkg.packageInitFunc)) {
          val refType = resolver.findClass(func.packageName).get

          val name = resolver.symName(func)
          val target = calcMethodRef(refType, SignatureType.fromSymType(refType), name, func)

          callMethod(target, None, None, SignatureType.Void, Seq.empty, Seq.empty, None)
        }
      }

      if (pkg.packageInitLiteralFunc == func) {
        // TODO: consider moving it under @has_invoked_pkg_init_literal check
        for (v <- pkg.values) v match {
          case g: CHIR.GlobalVar if !resolver.isImported(g) =>
            val declType = g.declaringDef.flatMap(resolver.symType).getOrElse(resolver.findClass(pkg.name).get)
            val field = asClassType(declType).findDeclaredFieldOrNull(xstr(resolver.symName(g)))
            assert(field.isStatic, field)
            val value = g.initializer.map {
              case CHIR.UnitLiteral => null
              case _: CHIR.NullLiteral => IntegralConst(ValueType.fromSig(field.getType))(0)
              case v: CHIR.IntLiteral => IntegralConst(ValueType.fromSig(field.getType))(v.value)
              case v: CHIR.FloatLiteral => field.getType match {
                case SignatureType.Float32 => FConst(v.value.toFloat)
                case SignatureType.Float64 => DConst(v.value)
                case t => notImplemented(s"unexpected static field type ${t.toJETSignature} of field $field")
              }
              case v: CHIR.BoolLiteral => IConst(if (v.value) 1 else 0)
              case v: CHIR.RuneLiteral => IConst(v.value.toInt)
              case v: CHIR.StringLiteral => constString(v.value)
              case v: CHIR.Func =>
                val refType = resolver.findClass(v.packageName).get

                val name = resolver.symName(v)
                val target = calcMethodRef(refType, SignatureType.fromSymType(refType), name, v)

                callMethod(target, None, None, SignatureType.Void, Seq.empty, Seq.empty, None)
                null
            }.orNull
            value match {
              case null =>
                // nothing to do
              case StackAlloc.Local(allocType) =>
                // string
                val mem = GetStaticFieldSeqRef(Seq(CangjieFieldReference(field.getFieldIndex, Some(field), SignatureType.fromSymType(declType), field.getType)))(DerivedPtr.Global())
                copy(allocType, mem, value)
              case _ =>
                StoreStaticFieldSeq(Seq(CangjieFieldReference(field.getFieldIndex, Some(field), SignatureType.fromSymType(declType), field.getType)))(DerivedPtr.Global(), value)
            }
          case _ =>
        }
      }
    }

    object ValueSig {
      def unapply(v: CHIR.Value): Option[SignatureType] = Some(resolver.typeSig(v match {
          case v: CHIR.GlobalVar => v.tpe
          case v: CHIR.LocalVar => v.tpe
          case v: CHIR.Parameter => v.tpe
      }))
    }

    private def parseExpression(e: CHIR.Expression, block: Block, state: State): Unit = e match {
      case e: CHIR.Unary =>
        import SignatureType.*
        val arg = state(e.operand)
        val sig = resolver.typeSig(e.resultTpe)
        val tpe = if (sig == Float16) FloatType else arg.tpe
        assert(tpe != ValueType, arg)

        def adjustBool(n: Node): Node = {
          BitFieldExtract.BFX(tpe, 0, sig.toAsm.sizeInBits, signExtension = false, n)
        }

        val n = e.kind match {
          case CHIR.Unary.Kind.Neg => Neg(tpe)(arg)
          case CHIR.Unary.Kind.Not => CondVal(negated = true)(Cmp(tpe, Condition.NE)(adjustBool(arg), IntegralConst(tpe)(0)))
          case CHIR.Unary.Kind.BitNot => Xor(arg, IntegralConst(tpe)(-1))
        }
        state(e) = n

      case e: CHIR.Binary =>
        import SignatureType.*

        val ValueSig(sig) = e.leftOperand
        val lraw = state(e.leftOperand)
        val rraw = state(e.rightOperand)

        val resSig = resolver.typeSig(e.resultTpe)

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
            val op = e.kind match {
              case CHIR.Binary.Kind.Lt => if (unsigned) Condition.ULT else Condition.LT
              case CHIR.Binary.Kind.Gt => if (unsigned) Condition.UGT else Condition.GT
              case CHIR.Binary.Kind.Le => if (unsigned) Condition.ULE else Condition.LE
              case CHIR.Binary.Kind.Ge => if (unsigned) Condition.UGE else Condition.GE
              case CHIR.Binary.Kind.Eq => Condition.EQ
              case CHIR.Binary.Kind.NotEq => Condition.NE
              case x => shouldNotReachHere(s"unexpected boolean binary expression: ${e.kind}")
            }
            CondVal(Cmp(tpe, op)(l, r))

          case resSig: FloatingPoint =>
            e.kind match {
              case CHIR.Binary.Kind.Add => Add(l, r)
              case CHIR.Binary.Kind.Sub => Sub(l, r)
              case CHIR.Binary.Kind.Mul => Mul(l, r)
              case CHIR.Binary.Kind.Div => FDiv(tpe)(l, r)
              case x => shouldNotReachHere(s"unexpected floating point binary expression: ${e.kind}")
            }

          case resSig: Integral =>
            assert(resSig == sig)
            val size = resSig.bits
            val signed = resSig.signed

            e.overflowStrategy match {
              case CHIR.OverflowStrategy.Wrapping | CHIR.OverflowStrategy.Na =>
                e.kind match {
                  case CHIR.Binary.Kind.Add => Add(l, r)
                  case CHIR.Binary.Kind.Sub => Sub(l, r)
                  case CHIR.Binary.Kind.Mul => Mul(l, r)
                  case CHIR.Binary.Kind.Div => DivisorCheck()(r); IDivRemOp(tpe, isUnsigned = !signed, isDiv = true)(l, r)
                  case CHIR.Binary.Kind.Mod => DivisorCheck()(r); IDivRemOp(tpe, isUnsigned = !signed, isDiv = false)(l, r)
                  case CHIR.Binary.Kind.LShift => Shift(ArithOp.LSL, l, BitFieldExtract.Truncate(r)) // TODO explicit overshift check in compiler or runtime
                  case CHIR.Binary.Kind.RShift =>
                    val op = if (sig.toAsm.isSigned) ArithOp.ASR else ArithOp.LSR // Cangjie has arithmetic shift in case of signed left operand
                    Shift(op, l, BitFieldExtract.Truncate(r)) // TODO explicit overshift check in compiler or runtime
                  case CHIR.Binary.Kind.And => And(l, r)
                  case CHIR.Binary.Kind.Or => Or(l, r)
                  case CHIR.Binary.Kind.Xor => Xor(l, r)
                  case CHIR.Binary.Kind.Exp => Pow(l, r)
                  case x => shouldNotReachHere(s"unexpected wrapping binary expression: ${e.kind}")
                }
              case CHIR.OverflowStrategy.Throwing =>
                val width = sig.toAsm.width
                val normalizedArgs = Seq(l, r) map { n =>
                  CheckedOp.normalizeArg(n.tpe, width, signed, n)
                }
                e.kind match {
                  case CHIR.Binary.Kind.Add => CheckedOp(tpe, width, CheckedOp.Kind.ADD, signed, method.isManaged)(normalizedArgs: _*)
                  case CHIR.Binary.Kind.Sub => CheckedOp(tpe, width, CheckedOp.Kind.SUB, signed, method.isManaged)(normalizedArgs: _*)
                  case CHIR.Binary.Kind.Mul => CheckedOp(tpe, width, CheckedOp.Kind.MUL, signed, method.isManaged)(normalizedArgs: _*)
                  case CHIR.Binary.Kind.Div => CheckedOp(tpe, width, CheckedOp.Kind.DIV, signed, method.isManaged)(normalizedArgs: _*)
                  case CHIR.Binary.Kind.Mod => DivisorCheck()(r); IDivRemOp(tpe, isUnsigned = !signed, isDiv = false)(normalizedArgs: _*)
                  case CHIR.Binary.Kind.Exp => CheckedOp(tpe, width, CheckedOp.Kind.POW, signed, method.isManaged)(normalizedArgs: _*)
                  case x => shouldNotReachHere(s"unexpected throwing binary expression: ${e.kind}")
                }

              case CHIR.OverflowStrategy.Saturating =>
                val proc = e.kind match {
                  case CHIR.Binary.Kind.Exp => assert(size == 64); RTSProc.CJ_saturatingPowI64
                  case CHIR.Binary.Kind.Add => size match {
                    case 8  => if (signed) RTSProc.CJ_saturatingAddI8  else RTSProc.CJ_saturatingAddU8
                    case 16 => if (signed) RTSProc.CJ_saturatingAddI16 else RTSProc.CJ_saturatingAddU16
                    case 32 => if (signed) RTSProc.CJ_saturatingAddI32 else RTSProc.CJ_saturatingAddU32
                    case 64 => if (signed) RTSProc.CJ_saturatingAddI64 else RTSProc.CJ_saturatingAddU64
                  }
                  case CHIR.Binary.Kind.Sub => size match {
                    case 8  => if (signed) RTSProc.CJ_saturatingSubI8  else RTSProc.CJ_saturatingSubU8
                    case 16 => if (signed) RTSProc.CJ_saturatingSubI16 else RTSProc.CJ_saturatingSubU16
                    case 32 => if (signed) RTSProc.CJ_saturatingSubI32 else RTSProc.CJ_saturatingSubU32
                    case 64 => if (signed) RTSProc.CJ_saturatingSubI64 else RTSProc.CJ_saturatingSubU64
                  }
                  case CHIR.Binary.Kind.Mul => size match {
                    case 8  => if (signed) RTSProc.CJ_saturatingMulI8  else RTSProc.CJ_saturatingMulU8
                    case 16 => if (signed) RTSProc.CJ_saturatingMulI16 else RTSProc.CJ_saturatingMulU16
                    case 32 => if (signed) RTSProc.CJ_saturatingMulI32 else RTSProc.CJ_saturatingMulU32
                    case 64 => if (signed) RTSProc.CJ_saturatingMulI64 else RTSProc.CJ_saturatingMulU64
                  }
                  case CHIR.Binary.Kind.Div => size match {
                    case 8  => if (signed) RTSProc.CJ_saturatingDivI8  else RTSProc.CJ_saturatingDivU8
                    case 16 => if (signed) RTSProc.CJ_saturatingDivI16 else RTSProc.CJ_saturatingDivU16
                    case 32 => if (signed) RTSProc.CJ_saturatingDivI32 else RTSProc.CJ_saturatingDivU32
                    case 64 => if (signed) RTSProc.CJ_saturatingDivI64 else RTSProc.CJ_saturatingDivU64
                  }
                  case CHIR.Binary.Kind.Mod => size match {
                    case 8  => if (signed) RTSProc.CJ_saturatingModI8  else RTSProc.CJ_saturatingModU8
                    case 16 => if (signed) RTSProc.CJ_saturatingModI16 else RTSProc.CJ_saturatingModU16
                    case 32 => if (signed) RTSProc.CJ_saturatingModI32 else RTSProc.CJ_saturatingModU32
                    case 64 => if (signed) RTSProc.CJ_saturatingModI64 else RTSProc.CJ_saturatingModU64
                  }
                  case x => shouldNotReachHere(s"unexpected saturating binary expression: ${e.kind}")
                }
                if (env.enabled(FailSaturatingArithmetic)) {
                  notImplemented("Saturating arithmetic")
                  //RTSCall(proc)(lraw, rraw)
                }
                IntegralConst(tpe)(123456789)
            }
        }
        state(e) = adjustRes(n)

      case e: CHIR.Allocate =>
        val allocType = e.allocatedType
        val sig = resolver.typeSig(allocType)
        val n = if (sig.isTraceableReference) {
          allocType match {
            case t: CHIR.RefType =>
              // Uninitialized
              NoValue()

            case _ =>
              sig match {
                case _ if sig.isAbstractClass =>
                  Null()
                case sig: SignatureType.InstantiatedReference if sig.isCangjieLambda =>
                  NewGeneric(sig)(loadTypeInfo(sig))
                case _ if sig.containsTypeVariables =>
                  NewGeneric(sig)(loadTypeInfo(sig))
                case _ =>
                  New(sig)()
              }
          }

        } else if (sig.isZST) {
          NoValue()

        } else if (sig.isTypeVariable) {
          // Uninitialized
          NoValue()

        } else if (sig.isPrimitive) {
          ZeroValueNode(ValueType.fromSig(sig))

        } else if (sig.isVariableSizeType) {
          NewGeneric(sig)(loadTypeInfo(sig))

        } else {
          assert(sig.isRecord)
          StackAlloc.Local(sig, workaroundForNonZeroedTraceableRecords = true)
        }
        state(e) = n

      case e: CHIR.RawArrayAllocate =>
        val len = state(e.size)
        val elemType = resolver.typeSig(e.elementType)
        val arrayType = SignatureType.CangjieArray(elemType)
        state(e) = NewArray(arrayType)(len)

      case e: CHIR.GetElementRef =>
        val memBase = e.base
        val (mem, staticField) = memBase match {
          case v: CHIR.GlobalVar => (NoValue(), Some(staticFieldRef(v)))
          case _ => (state(memBase), None)
        }

        staticField match {
          case None =>
            nullCheck(mem)
          case Some(sf) =>
            val symRefType = asClassType(sf.refType)
            ensurePrepared(symRefType)
            packageInitCheck(symRefType)
        }

        val ValueSig(host) = memBase

        val fields = fieldChain(host, e.path)

        val lastField = fields.last
        val n = if (lastField.fieldType.isZST) {
          // do nothing
          Void()

        } else {
          staticField match {
            case None =>
              if (host.isVariableLayoutType || fields.exists(_.fieldType.isVariableSizeType))  {
                GetFieldSeqRefGeneric(fields)(maybeDerivedPtrBase(mem), mem, typeInfos(fields))
              } else {
                GetFieldSeqRef(fields)(maybeDerivedPtrBase(mem), mem)
              }
            case Some(sf) => GetStaticFieldSeqRef(sf +: fields)(DerivedPtr.Global())
          }
        }
        state(e) = n

      case e: CHIR.StoreElementRef =>
        val memBase = e.location
        val arg = state(e.value)
        val (mem, staticField) = memBase match {
          case v: CHIR.GlobalVar => (NoValue(), Some(staticFieldRef(v)))
          case _ => (state(memBase), None)
        }

        staticField match {
          case None =>
            nullCheck(mem)
          case Some(sf) =>
            val symRefType = asClassType(sf.refType)
            ensurePrepared(symRefType)
            packageInitCheck(symRefType)
        }

        val ValueSig(host) = memBase

        if (host.isArray) {
          assert(e.path.size == 1)
          val idx = e.path.head
          arrayPut(host, mem, LConst(idx), arg)

        } else {
          val fields = fieldChain(host, e.path)

          val lastField = fields.last
          if (lastField.fieldType.isZST) {
            // do nothing
            NoValue()

          } else {
            writeBarrier()
            staticField match {
              case None =>
                if (host.isVariableLayoutType || fields.exists(_.fieldType.isVariableSizeType)) {
                  StoreFieldSeqGeneric(fields)(maybeDerivedPtrBase(mem), mem, arg, typeInfos(fields))
                } else if (needsCopy(lastField.fieldType)) {
                  val addr = GetFieldSeqRef(fields)(maybeDerivedPtrBase(mem), mem)
                  copy(lastField.fieldType, addr, arg)
                } else {
                  StoreFieldSeq(fields)(maybeDerivedPtrBase(mem), mem, arg)
                }
              case Some(sf) =>
                if (needsCopy(lastField.fieldType)) {
                  val addr = GetStaticFieldSeqRef(fields)(DerivedPtr.Global())
                  copy(lastField.fieldType, addr, arg)
                } else {
                  StoreStaticFieldSeq(sf +: fields)(DerivedPtr.Global(), arg)
                }
            }
          }
        }

      case e: CHIR.Field =>
        val memBase = e.base
        val _mem = state(memBase)
        val staticField = memBase match {
          case v: CHIR.GlobalVar => Some(staticFieldRef(v))
          case _ => None
        }

        val (mem, host) = _mem match {
          case x: EnumCast => (x.base, x.enumType)
          case x =>
            val ValueSig(host) = memBase
            (x, host)
        }

        val chirPath = e.path

        host match {
          case _: SignatureType.ZeroSizedEnum | _: SignatureType.PrimitiveBasedEnum => shouldNotReachHere(host)
          case host: SignatureType.OptionLikeEnum if host.isNullableOption =>
            val res = chirPath match {
              case Seq(0) =>
                val nullIsZeroTag = host.info.constructors.head.params.isEmpty
                val cond = if (nullIsZeroTag) Condition.NE else Condition.EQ
                CondVal(Cmp(TRefType, cond)(mem, Null()))
              case Seq(1) =>
                mem
            }
            state(e) = res

          case host: SignatureType.OptionLikeEnum if host.someType.isTypeVariable =>
            val res = chirPath match {
              case Seq(0) => OptionTagGeneric(host)(loadTypeInfo(host.someType), mem)
              case Seq(1) => OptionPayloadGeneric(host)(loadTypeInfo(host.someType), loadTypeInfo(host), mem)
            }
            state(e) = res

          case host: SignatureType.UnionBasedEnum =>
            val res = chirPath match {
              case Seq(0) =>
                val tupleType = SignatureType.Tuple(Seq(SignatureType.UInt32))
                LoadFieldSeq(Seq(CangjieFieldReference(0, None, tupleType, SignatureType.UInt32)))(
                  maybeDerivedPtrBase(mem),
                  ReinterpretCast(ValueType(host), ValueType(tupleType))(mem)
                )
            }
            state(e) = res

          case _ =>
            staticField match {
              case None =>
                nullCheck(mem)
              case Some(sf) =>
                val symRefType = asClassType(sf.refType)
                ensurePrepared(symRefType)
                packageInitCheck(symRefType)
            }

            val fields = fieldChain(host, chirPath)

            val lastField = fields.last
            val n = if (lastField.fieldType.isZST) {
              // do nothing
              Void()

            } else {
              val shouldCopy = needsCopy(lastField.fieldType)
              val valueOrMem = staticField match {
                case None =>
                  if (host.isVariableLayoutType || fields.exists(_.fieldType.isVariableSizeType)) {
                    val tis = typeInfos(fields)
                    if (shouldCopy) {
                      GetFieldSeqRefGeneric(fields)(maybeDerivedPtrBase(mem), mem, tis)
                    } else {
                      LoadFieldSeqGeneric(fields)(maybeDerivedPtrBase(mem), mem, tis)
                    }
                  } else {
                    if (shouldCopy) {
                      GetFieldSeqRef(fields)(maybeDerivedPtrBase(mem), mem)
                    } else {
                      LoadFieldSeq(fields)(maybeDerivedPtrBase(mem), mem)
                    }
                  }
                case Some(sf) =>
                  if (shouldCopy) {
                    GetStaticFieldSeqRef(sf +: fields)(DerivedPtr.Global())
                  } else {
                    LoadStaticFieldSeq(sf +: fields)(DerivedPtr.Global())
                  }
              }
              if (shouldCopy) {
                val local = StackAlloc.Local(lastField.fieldType)
                copy(lastField.fieldType, local, valueOrMem)
                local
              } else {
                valueOrMem
              }
            }
            state(e) = n
        }

      case e: CHIR.Apply =>
        val func = e.callee

        val thisType = e.thisType.map(resolver.typeSig)

        def refineSuperTypes(refType: SignatureType): Iterator[SignatureType] = {
          val cparams = refType match {
            case refType: SignatureType.InstantiatedType => refType.instantiatedTypeParameters
            case _ => Seq.empty
          }
          val lparams = Seq.empty

          val refClass = asClassType(refType)
          Option(refClass.getSuperClassSig).map(_.instantiate(cparams, lparams)).iterator ++
            refClass.getDeclaredSuperInterfacesSig.map(_.instantiate(cparams, lparams))
        }

        val declClass = func.declaringDef
          .flatMap(resolver.symType)
          .map(asClassType)
          .getOrElse(resolver.findClass(func.packageName).get)

        val thisTypeForRefining = thisType.filter(t => t.isRecord || t.isReference)
        val declType = Closure(thisTypeForRefining)(refineSuperTypes).find(asClassType(_) == declClass)
          .getOrElse(SignatureType.fromSymType(declClass))

        val name = resolver.symName(func)
        val _target = calcMethodRef(declClass, declType, name, func)

        // TODO: add instantiated type parameters to base MethodReference
        val lparams = e.instantiatedTypeArgs.map(resolver.typeSig)
        val target = if (lparams.nonEmpty) {
          _target.toInstantiatedMethodReference(lparams, declType)
        } else {
          _target
        }
        val argVals = e.args
        val outerType = if (asClassType(declType).isCangjieExtend) thisType else Some(declType)

        val args = argVals match {
          case Seq(gv: CHIR.GlobalVar, rest: _*) =>
            GetStaticFieldSeqRef(Seq(staticFieldRef(gv)))(DerivedPtr.Global()) +: rest.map(state.apply)
          case _ =>
            argVals.map(state.apply)
        }
        val paramTypes = argVals.map { v =>
          val ValueSig(sig) = v
          sig
        }
        val retType = resolver.typeSig(e.resultTpe)
        val call = callMethod(target, outerType, thisType, retType, paramTypes, args, None)
        state(e) = call

      case e: CHIR.Invoke =>
        val methodArgVal = e.callee
        val argVals = e.args

        val isStatic = e.thisArg match {
          case x: CHIR.LocalVar =>
            x.associatedExpr match {
              case _: (CHIR.GetRTTI | CHIR.GetRTTIStatic) => true
              case _ => false
            }
          case _ => false
        }

        val (thisTypeArgVal, sourceArgVals) = if (isStatic) {
          (argVals.headOption, argVals.tail)
        } else {
          (None, argVals)
        }
        val thisTypeInfo = thisTypeArgVal.map(state.apply)

        val thisType = resolver.typeSig(e.thisType) match {
          // FIXME
          case SignatureType.ThisTypeInfo => SignatureType.fromSymType(rootMethod.getDeclaringClass)
          case _: SignatureType.TypeVariable =>
            val v = if (isStatic) thisTypeArgVal.get else sourceArgVals.head
            val ValueSig(sig) = v
            sig
          case t => t
        }

        val retType = resolver.typeSig(e.resultTpe)
        val isigParams = sourceArgVals map { v =>
          val ValueSig(sig) = v
          sig
        }

        val func = methodArgVal
        val name = resolver.symName(func)
        val (gsig, _, _, _) = resolver.functionSig(func.tpe, hasReceiver = !isStatic)

        def boxTypeVar(g: SignatureType, i: SignatureType): SignatureType = {
          if (g.isTypeVariable && !i.isTypeVariable && !i.isInstanceOf[SignatureType.Box]) SignatureType.Box(i) else i
        }

        val isig = MethodSignature(retType, isigParams.drop(if (isStatic) 0 else 1))
        val sig = MethodSignature(boxTypeVar(gsig.returnType, isig.returnType), gsig.parameterTypes.zip(isig.parameterTypes).map(boxTypeVar.tupled))

        val lparams = e.instantiatedTypeArgs.map(resolver.typeSig)

        val vtable = asClassType(thisType).getCHIRVTable
        assert(vtable != null, thisType)

        def findSlot(extDef: CHIRVTable.ExtDef): Option[(CHIRVTable.ExtDef, Int)] = {
          val vnum = extDef.funcTable.indexWhere(m => m.name == name && m.originalSig.instantiate(genericParams(thisType), Seq.empty) == sig)
          Option.when(vnum >= 0)((extDef, vnum))
        }

        val (extDef, vnum) = vtable.extDefs.collectFirst(findSlot.unlift).getOrElse {
          shouldNotReachHere(s"could not find VTable slot for $name${sig.toJETSignature} in $vtable")
        }
        val entry = extDef.funcTable(vnum)

        val refType = extDef.extType.instantiate(genericParams(thisType), Seq.empty)

        // TODO: unify logic with calcMethodRef?
        val refClass = asClassType(refType)
        val method = entry.impl.getOrElse(refClass.findDeclaredMethodOrNull(xstr(name), gsig))

        val mak = if (isStatic) {
          // TODO: static virtual!
          MAK.STATIC
        } else {
          MAK.VIRTUAL
        }
        val target = new MethodReference(method, mak, CompiledType(refType), vnum)

        val args = sourceArgVals.map(state.apply)
        val call = callMethod(target, Some(refType), Some(thisType), retType, isigParams, args, thisTypeInfo)
        state(e) = call

      case e: CHIR.InstanceOf =>
        val tpe = resolver.typeSig(e.testType)
        val obj = state(e.obj)
        val refTpe = if (tpe.isTraceableReference) tpe else SignatureType.Box(tpe)
        state(e) = if (tpe.containsTypeVariables) {
          InstanceOfGeneric(refTpe)(obj, loadTypeInfo(tpe))
        } else {
          InstanceOf(refTpe)(obj)
        }

      case e: (CHIR.NumericCast | CHIR.StaticCast) =>
        import BitFieldExtract.*
        import SignatureType.*
        import AsmType.*

        val ValueSig(from) = e.value
        val value = state(e.value)
        val to = resolver.typeSig(e.targetTpe)

        val fromAsm = from.toAsm
        val toAsm = to.toAsm

        def fromTpe = ValueType.fromSig(from)

        def toTpe = ValueType.fromSig(to)

        e match {
          case e: CHIR.NumericCast => e.overflowStrategy match {
            case CHIR.OverflowStrategy.Wrapping | CHIR.OverflowStrategy.Na => // ok
            case CHIR.OverflowStrategy.Throwing => // TODO: do we need to support it?
            case CHIR.OverflowStrategy.Saturating => notImplemented("saturating type cast")
          }
          case _ => // do nothing
        }

        val n = (from, to) match {
          case (from: (Reference | InstantiatedReference | TypeVariable), to: (Reference | InstantiatedReference)) =>
            // TODO: remove this case when numeric cast will handle only *numeric* types
            CheckCast(to, trusted = true)(value)
            value

          case (from: (Record | InstantiatedRecord), to: (Record | InstantiatedRecord)) =>
            // TODO: check something
            ReinterpretCast(fromTpe, toTpe)(value)

          case (from: (Record | InstantiatedRecord), to: Tuple) =>
            // TODO: assert from is enum!
            ReinterpretCast(fromTpe, toTpe)(value)

          case (from: TypeVariable, to: TypeVariable) =>
            value

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

          case (Int32 | UInt32 | _: PrimitiveBasedEnum, Int32 | UInt32 | _: PrimitiveBasedEnum) =>
            value

          case (from@OptionLikeEnum(_, _, x), to@Tuple(Seq(Boolean, y))) =>
            assert(x == y, s"cast from $from to $to")
            if (from.isNullableOption || x.isTypeVariable) {
              EnumCast(from)(value)
            } else {
              ReinterpretCast(fromTpe, toTpe)(value)
            }

          case (from: ClassBasedEnum, to: Tuple) =>
            val ctors = from.info.constructors
            val targetCtor = to.params.tail // first element is tag
            val idx = ctors.indexWhere(_.params == targetCtor) // TODO: instantiate
            assert(idx >= 0)
            val enumName = resolver.classBasedEnumConstructorName(from.name, idx)
            val enumType = if (from.params.isEmpty) {
              CangjieReference(enumName)
            } else {
              InstantiatedReference(enumName, from.params)
            }
            EnumCast(enumType)(value)

          case (from: UnionBasedEnum, to: Tuple) =>
            ReinterpretCast(fromTpe, toTpe)(value)

          case (from: ZeroSizedEnum, UInt32) =>
            IConst(0)

          case (UInt32, to: ZeroSizedEnum) =>
            self.Void()

          case _ => notImplemented(s"cast from ${from.toJETSignature} to ${to.toJETSignature}")
        }

        state(e) = n

      case e: CHIR.Branch =>
        val selectorVar = e.condition
        val ValueSig(sig) = selectorVar
        val selectorValue = state(selectorVar)

        val selector = BitFieldExtract.BFX(IntType, 0, sig.toAsm.sizeInBits, signExtension = false, selectorValue)
        val cond = Cmp(selector.tpe, Condition.NE)(selector, IConst(0))
        val branch = block.blockEnd.asInstanceOf[If]
        val proxy = branch.selector
        assert(proxy.isInstanceOf[Proxy] && proxy.singleUse == branch)
        proxy.replaceBy(cond)

      case e: CHIR.MultiBranch =>
        val branch = block.blockEnd.asInstanceOf[Switch]
        val proxy = branch.selector
        assert(proxy.isInstanceOf[Proxy] && proxy.singleUse == branch)
        proxy.replaceBy(BitFieldExtract.Truncate(state(e.condition)))

      case e: CHIR.Intrinsic =>
        e.kind match {
          case CHIR.Intrinsic.Kind.Preinitialize =>
          // no-op

          case CHIR.Intrinsic.Kind.BeginCatch =>
            e.args match {
              case Seq(ex: CHIR.LocalVar) =>
                state(e) = state(ex)
            }

          case CHIR.Intrinsic.Kind.ArrayGetUnchecked |
               CHIR.Intrinsic.Kind.ArrayGetRefUnchecked |
               CHIR.Intrinsic.Kind.ArrayGet =>
            // TODO: ArrayIndexCheck
            val args = e.args
            val ValueSig(arrayType) = args.head
            val (obj, idx) = args match {
              case Seq(obj, idx) => (state(obj), state(idx))
            }
            val elemType = arrayType.getArrayElemType
            val n = if (elemType.isZST) {
              Void()
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

          case CHIR.Intrinsic.Kind.ArraySetUnchecked |
               CHIR.Intrinsic.Kind.ArraySet =>
            // TODO: ArrayIndexCheck
            val args = e.args
            val ValueSig(arrayType) = args.head
            val (obj, idx, value) = args match {
              case Seq(obj, idx, value) => (state(obj), state(idx), state(value))
            }
            arrayPut(arrayType, obj, idx, value)

          case CHIR.Intrinsic.Kind.ArraySize =>
            val obj = state(e.args.head)
            state(e) = CangjieArrayLength(obj)

          case CHIR.Intrinsic.Kind.ArrayBuiltinCopyTo =>
            val args = e.args
            val ValueSig(arrayType) = args.head
            val (src, dst, srcStart, dstStart, len) = args match {
              case Seq(src, dst, srcStart, dstStart, len) => (state(src), state(dst), state(srcStart), state(dstStart), state(len))
            }
            ArrayBuiltInCopyTo(arrayType)(src, dst, srcStart, dstStart, len)

          case CHIR.Intrinsic.Kind.AtomicLoad =>
            val args = e.args
            val ValueSig(refType) = args.head
            val obj = args match {
              case Seq(obj, _) => state(obj)
            }
            val Seq(field) = declaredFields(refType)
            state(e) = AtomicOps.Load(obj.tpe, field)(obj)

          case CHIR.Intrinsic.Kind.AtomicStore =>
            val args = e.args
            val ValueSig(refType) = args.head
            val (obj, value) = args match {
              case Seq(obj, value, _) => (state(obj), state(value))
            }
            val Seq(field) = declaredFields(refType)
            AtomicOps.Store(obj.tpe, field)(obj, PutMemoryOperation.adjustValue(field.fieldType.toAsm, value))

          case CHIR.Intrinsic.Kind.AtomicCAS =>
            val args = e.args
            val ValueSig(refType) = args.head
            val (obj, compareVal, swapVal) = args match {
              case Seq(obj, compareVal, swapVal, _, _) => (state(obj), state(compareVal), state(swapVal))
            }
            val Seq(field) = declaredFields(refType)
            state(e) = AtomicOps.CAS(obj.tpe, field)(obj, compareVal, swapVal)

          case CHIR.Intrinsic.Kind.AtomicSwap =>
            val args = e.args
            val ValueSig(refType) = args.head
            val (obj, value) = args match {
              case Seq(obj, value, _) => (state(obj), state(value))
            }
            val Seq(field) = declaredFields(refType)
            AtomicOps.Simple.swap(obj.tpe, field)(obj, value)

          case CHIR.Intrinsic.Kind.AtomicFetchAdd =>
            val args = e.args
            val ValueSig(refType) = args.head
            val (obj, value) = args match {
              case Seq(obj, value, _) => (state(obj), state(value))
            }
            val Seq(field) = declaredFields(refType)
            AtomicOps.Simple.fetchAdd(obj.tpe, field)(obj, value)

          case CHIR.Intrinsic.Kind.AtomicFetchSub =>
            val args = e.args
            val ValueSig(refType) = args.head
            val (obj, value) = args match {
              case Seq(obj, value, _) => (state(obj), state(value))
            }
            val Seq(field) = declaredFields(refType)
            AtomicOps.Simple.fetchSub(obj.tpe, field)(obj, value)

          case CHIR.Intrinsic.Kind.AtomicFetchAnd =>
            val args = e.args
            val ValueSig(refType) = args.head
            val (obj, value) = args match {
              case Seq(obj, value, _) => (state(obj), state(value))
            }
            val Seq(field) = declaredFields(refType)
            AtomicOps.Simple.fetchAnd(obj.tpe, field)(obj, value)

          case CHIR.Intrinsic.Kind.AtomicFetchOr =>
            val args = e.args
            val ValueSig(refType) = args.head
            val (obj, value) = args match {
              case Seq(obj, value, _) => (state(obj), state(value))
            }
            val Seq(field) = declaredFields(refType)
            AtomicOps.Simple.fetchOr(obj.tpe, field)(obj, value)

          case CHIR.Intrinsic.Kind.AtomicFetchXor =>
            val args = e.args
            val ValueSig(refType) = args.head
            val (obj, value) = args match {
              case Seq(obj, value, _) => (state(obj), state(value))
            }
            val Seq(field) = declaredFields(refType)
            AtomicOps.Simple.fetchXor(obj.tpe, field)(obj, value)

          case CHIR.Intrinsic.Kind.Sqrt =>
            e.args.map(state.apply) match {
              case Seq(x) =>
                val kind = if (x.tpe == DoubleType) Java.Lang.MathIntrinsic.D_SQRT else Java.Lang.MathIntrinsic.F_SQRT
                state(e) = MathIntrinsic(kind)(x)
            }

          case CHIR.Intrinsic.Kind.Abs =>
            e.args.map(state.apply) match {
              case Seq(x) =>
                state(e) = Abs(x)
            }

          // TODO write cangjie tests (use -cbcaotdeps)
          case CHIR.Intrinsic.Kind.CPointerRead =>
            val args = e.args
            val ValueSig(cpointerType: SignatureType.CPointer) = args.head
            val (base, idx) = args match {
              case Seq(base, idx) => (state(base), state(idx))
            }
            cpointerType.pointee match {
              case baseType: SignatureType =>
                val mem = Add(base, Mul(LConst(baseType.toAsm.sizeInBytes), idx))
                state(e) = if (baseType.isPrimitive) {
                  LoadMemory(baseType.toAsm, baseType, atomic = false)(mem)
                } else {
                  assert(baseType.isRecord)
                  Add(base, Mul(LConst(baseType.toAsm.sizeInBytes), idx))
                }
              case _ =>
                // TODO pointer to a method?
                shouldNotReachHere(cpointerType)
            }

          // TODO write cangjie tests (use -cbcaotdeps)
          case CHIR.Intrinsic.Kind.CPointerWrite =>
            val args = e.args
            val ValueSig(cpointerType: SignatureType.CPointer) = args.head
            val (base, idx, value) = args match {
              case Seq(base, idx, value) => (state(base), state(idx), state(value))
            }
            cpointerType.pointee match {
              case baseType: SignatureType =>
                val mem = Add(base, Mul(LConst(baseType.toAsm.sizeInBytes), idx))
                if (baseType.isPrimitive) {
                  StoreMemory(baseType.toAsm, baseType, atomic = false)(mem, value)
                } else {
                  assert(baseType.isRecord)
                  copy(baseType, mem, value)
                }
              case _ =>
                // TODO pointer to a method?
                shouldNotReachHere(cpointerType)
            }

          case CHIR.Intrinsic.Kind.ArrayAcquireRawData =>
            if (env.enabled(FailArrayAcquireRawData)) {
              notImplemented("ARRAY_ACQUIRE_RAW_DATA intrinsic")
            }
            state(e) = LConst(123456789)

          case CHIR.Intrinsic.Kind.ArrayReleaseRawData =>
            if (env.enabled(FailArrayAcquireRawData)) {
              notImplemented("ARRAY_RELEASE_RAW_DATA intrinsic")
            }

          case CHIR.Intrinsic.Kind.ObjectZeroValue =>
            val sig = resolver.typeSig(e.resultTpe)
            val res = if (sig.isZST) {
              Void()
            } else if (sig.isRecord) {
              StackAlloc.Local(sig, workaroundForNonZeroedTraceableRecords = true)
            } else {
              ZeroValueNode(ValueType.fromSig(sig))
            }
            state(e) = res
        }

      case e: CHIR.Spawn =>
        val objVar = e.obj
        val obj = state(objVar)
        val ValueSig(objSig) = objVar
        e.executeClosure match {
          case None =>
            val retType = resolver.typeSig(e.resultTpe)
            state(e) = SpawnFuture(retType)(obj)
          case Some(_) =>
            SpawnClosure(objSig)(obj)
        }

      case e: CHIR.Debug =>
        // TODO: support debug

      case e: CHIR.GetRTTIStatic =>
        assert(rootMethod.hasThisTypeInfoParameter)
        state(e) = rootMethodParam(rootMethod.getThisTypeInfoArgIdx)

      case e: CHIR.Constant =>
        def intConst(v: Long, l: CHIR.Literal): Node = {
          l.tpe match {
            case CHIR.BuiltinType.Int8 | CHIR.BuiltinType.Int16 | CHIR.BuiltinType.Int32 |
                 CHIR.BuiltinType.UInt8 | CHIR.BuiltinType.UInt16 | CHIR.BuiltinType.UInt32 | CHIR.BuiltinType.Boolean => IConst(v.toInt)
            case CHIR.BuiltinType.Int64 | CHIR.BuiltinType.IntNative |
                 CHIR.BuiltinType.UInt64 | CHIR.BuiltinType.UIntNative => LConst(v)
            case CHIR.BuiltinType.Float16 => notImplemented(s"FLOAT16: $v")
            case CHIR.BuiltinType.Float32 => FConst(v.toFloat)
            case CHIR.BuiltinType.Float64 => DConst(v.toDouble)
            case CHIR.BuiltinType.Unit => IntegralConst(AddrType)(v)
            case t: CHIR.CustomType => IntegralConst(AddrType)(v)
          }
        }
        val value = e.literal match {
          case v: CHIR.BoolLiteral => IConst(if (v.value) 1 else 0)
          case CHIR.UnitLiteral => Void()
          case v: CHIR.RuneLiteral => IConst(v.value.toInt)
          case v: CHIR.NullLiteral => if (resolver.typeSig(e.resultTpe).isTraceableReference) Null() else intConst(0, v)
          case v: CHIR.IntLiteral => intConst(v.value, v)
          case v: CHIR.FloatLiteral => v.tpe match {
            case CHIR.BuiltinType.Float16 => notImplemented(s"FLOAT16: ${v.value}")
            case CHIR.BuiltinType.Float32 => FConst(v.value.toFloat)
            case CHIR.BuiltinType.Float64 => DConst(v.value)
          }
          case v: CHIR.StringLiteral => constString(v.value)
        }
        state(e) = value

      case e: CHIR.Load =>
        e.location match {
          case localVar: CHIR.LocalVar =>
            val sig = resolver.typeSig(localVar.tpe)
            if (sig.isZST) {
              // nothing to do
              state(e) = Void()

            } else {
              val n = state(localVar) match {
                case mem @ GetFieldSeqRef(fields, _, base) =>
                  if (needsCopy(mem.resType)) {
                    val res = StackAlloc.Local(mem.resType)
                    copy(mem.resType, res, mem)
                    res
                  } else {
                    LoadFieldSeq(fields)(maybeDerivedPtrBase(mem), base)
                  }
                case mem @ GetFieldSeqRefGeneric(fields, _, base, tis) =>
                  if (mem.resType.isVariableSizeType || !needsCopy(mem.resType)) {
                    LoadFieldSeqGeneric(fields)(maybeDerivedPtrBase(mem), base, tis)
                  } else {
                    val res = StackAlloc.Local(mem.resType)
                    copy(mem.resType, res, mem)
                    res
                  }
                case mem @ GetStaticFieldSeqRef(fields) =>
                  if (needsCopy(mem.resType)) {
                    val res = StackAlloc.Local(mem.resType)
                    copy(mem.resType, res, mem)
                    res
                  } else {
                    LoadStaticFieldSeq(fields)(DerivedPtr.Global())
                  }
                case mem =>
                  if (sig.isRecord || sig.isTraceableReference || sig.isPrimitive) {
                    mem
                  } else {
                    LoadMemory(sig.toAsm, sig, atomic = false)(mem)
                  }
              }
              state(e) = n
            }
          case globalVar: CHIR.GlobalVar =>
            val field = staticFieldRef(globalVar)
            val n = if (field.fieldType.isZST) {
              Void()
            } else if (needsCopy(field.fieldType)) {
              val local = StackAlloc.Local(field.fieldType)
              val addr = GetStaticFieldSeqRef(Seq(field))(DerivedPtr.Global())
              copy(field.fieldType, local, addr)
              local
            } else {
              LoadStaticFieldSeq(Seq(field))(DerivedPtr.Global())
            }
            state(e) = n
        }

      case e: CHIR.Store =>
        val valueVar = e.value
        val ValueSig(sig) = valueVar
        val value = state(valueVar)
        e.location match {
          case localVar: CHIR.LocalVar =>
            if (sig.isZST) {
              // nothing to do

            } else {
              val mem = state(localVar)
              writeBarrier()
              if (!sig.isVariableSizeType && needsCopy(sig)) {
                value match {
                  case ZeroValueNode() =>
                  // TODO: zeroing?
                  case _ =>
                    copy(sig, mem, value)
                }

              } else {
                mem match {
                  case GetFieldSeqRef(fields, _, base) =>
                    StoreFieldSeq(fields)(maybeDerivedPtrBase(mem), base, value)
                  case GetFieldSeqRefGeneric(fields, _, base, tis) =>
                    StoreFieldSeqGeneric(fields)(maybeDerivedPtrBase(mem), base, value, tis)
                  case GetStaticFieldSeqRef(fields) =>
                    StoreStaticFieldSeq(fields)(DerivedPtr.Global(), value)
                  case mem =>
                    if (sig.isTraceableReference || sig.isPrimitive || sig.isVariableSizeType) {
                      state(localVar) = value
                    } else {
                      shouldNotReachHere(sig.toJETSignature)
                    }
                }
              }
            }
          case globalVar: CHIR.GlobalVar =>
            val staticField = staticFieldRef(globalVar)
            if (sig.isZST) {
              // nothing to do

            } else {
              val symRefType = asClassType(staticField.refType)

              ensurePrepared(symRefType)
              packageInitCheck(symRefType)

              val value = state(valueVar)
              if (needsCopy(staticField.fieldType)) {
                val addr = GetStaticFieldSeqRef(Seq(staticField))(DerivedPtr.Global())
                copy(sig, addr, value)
              } else {
                writeBarrier()
                StoreStaticFieldSeq(Seq(staticField))(DerivedPtr.Global(), value)
              }
            }
        }

      case _: CHIR.Goto =>
        assert(block.blockEnd.isInstanceOf[Goto])

      case _: CHIR.Exit =>
        val retType = rootMethod.getReturnType
        val retVal = if (retType.isZST) {
          Void()
        } else {
          val r = func.retVal
          if (r.isEmpty) {
            Void()
          } else if (rootMethod.hasRetByValParameter) {
            val retByVal = rootMethodParam(rootMethod.getRetByValArgIdx)
            val abiRetValType = rootMethod.getMethodType.parameterType(rootMethod.getMethodType.getRetByValArgIdx)
            abiRetValType match {
              case _ if abiRetValType.isZST =>
                retByVal

              case t: SignatureType.Box =>
                // Variable-sized type
                val value = state(r.get)
                val ti = loadTypeInfo(t.base)
                val box = if (value.tpe.isTraceableRefType) value else Box(t.base)(ti, value)
                AssignGeneric(t.base)(ti, retByVal, box)
                retByVal

              case SignatureType.Address =>
                // Type variable
                val value = state(r.get)
                val memType = if (retType.isInstanceOf[SignatureType.Box]) retType else SignatureType.Box(retType)
                StoreMemory(memType.toAsm, memType, atomic = false)(retByVal, value)
                value

              case _ =>
                assert(abiRetValType.isRecord)
                copy(abiRetValType, retByVal, state(r.get))
                retByVal
            }
          } else {
            r.get match {
              case r: CHIR.LocalVar =>
                val t = resolver.typeSig(r.tpe)
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

      case e: CHIR.RaiseException =>
        assert(block.blockEnd.isInstanceOf[Halt])
        Throw(state(e.exceptionValue))

      case e: CHIR.Tuple =>
        val resTpe = e.resultTpe
        resTpe match {
          case t: CHIR.TupleType =>
            val tupleType = resolver.typeSig(resTpe).asInstanceOf[SignatureType.Tuple]
            state(e) = allocTuple(tupleType, e.elementValues.map(state.apply))

          case t: CHIR.EnumType =>
            import SignatureType.*
            (resolver.typeSig(resTpe): @unchecked) match {
              case _: ZeroSizedEnum =>
              // nothing to do

              case _: PrimitiveBasedEnum =>
                state(e) = e.elementValues.map(state.apply) match {
                  case Seq(c: IConst) => c
                }

              case enumType: ClassBasedEnum =>
                e.elementValues.map(state.apply) match {
                  case args@Seq(IConst(c), _*) =>
                    assert(c >= 0, c)
                    val constrName = resolver.classBasedEnumConstructorName(resolver.symName(t), c)
                    val constr = resolver.findClass(constrName).get
                    state(e) = allocEnumObject(SignatureType.fromSymType(constr), args)
                }

              case enumType: UnionBasedEnum =>
                e.elementValues.map(state.apply) match {
                  case args@Seq(IConst(c), _*) =>
                    assert(c >= 0, c)
                    val constrTypes = enumType.info.constructors(c).params
                    val tupleType = Tuple(UInt32 +: constrTypes)
                    state(e) = allocTuple(tupleType, args)
                }

              case enumType: OptionLikeEnum =>
                if (enumType.isNullableOption) {
                  state(e) = e.elementValues.map(state.apply) match {
                    case Seq(IConst(c)) => assert(c == 0 || c == 1, c); Null()
                    case Seq(IConst(c), x) => assert(c == 0 || c == 1, c); x
                  }
                } else if (enumType.someType.isTypeVariable) {
                  val baseTypeInfo = loadTypeInfo(enumType.someType)
                  val optionTypeInfo = loadTypeInfo(enumType)
                  state(e) = e.elementValues.map(state.apply) match {
                    case Seq(IConst(c)) =>
                      assert(c == 0 || c == 1, c)
                      NewNoneOptionGeneric(enumType)(baseTypeInfo, optionTypeInfo)
                    case Seq(IConst(c), x) =>
                      assert(c == 0 || c == 1, c)
                      NewSomeOptionGeneric(enumType)(baseTypeInfo, optionTypeInfo, x)
                  }
                } else {
                  val mem = StackAlloc.Local(enumType, workaroundForNonZeroedTraceableRecords = true)
                  val payloadType = enumType.someType
                  val tupleType = Tuple(Seq(Boolean, payloadType))
                  val tagChain = fieldChain(enumType, Seq(0))
                  e.elementValues.map(state.apply) match {
                    case Seq(IConst(c)) =>
                      assert(c == 0 || c == 1, c)
                      if (enumType.isVariableLayoutType) {
                        StoreFieldSeqGeneric(tagChain)(maybeDerivedPtrBase(mem), mem, IConst(c), typeInfos(tagChain))
                      } else {
                        StoreFieldSeq(tagChain)(maybeDerivedPtrBase(mem), mem, IConst(c))
                      }

                    case Seq(IConst(c), x) =>
                      assert(c == 0 || c == 1, c)
                      if (enumType.isVariableLayoutType) {
                        StoreFieldSeqGeneric(tagChain)(maybeDerivedPtrBase(mem), mem, IConst(c), typeInfos(tagChain))
                      } else {
                        StoreFieldSeq(tagChain)(maybeDerivedPtrBase(mem), mem, IConst(c))
                      }
                      val payloadChain = fieldChain(enumType, Seq(1))
                      if (payloadType.isZST) {
                        // nothing to do

                      } else if (enumType.isVariableLayoutType || payloadChain.exists(_.fieldType.isVariableSizeType)) {
                        StoreFieldSeqGeneric(payloadChain)(maybeDerivedPtrBase(mem), mem, x, typeInfos(payloadChain))

                      } else if (needsCopy(payloadType)) {
                        val addr = GetFieldSeqRef(payloadChain)(maybeDerivedPtrBase(mem), mem)
                        copy(payloadType, addr, x)

                      } else {
                        StoreFieldSeq(payloadChain)(maybeDerivedPtrBase(mem), mem, x)
                      }
                  }
                  state(e) = mem
                }
            }
        }

      case e: CHIR.Box =>
        val v = e.value
        val ValueSig(baseType) = v
        val base = state(v)
        val res = baseType match {
          case baseType: SignatureType.OptionLikeEnum if baseType.someType.isTypeVariable =>
            base
          case _ =>
            if (baseType.isTraceableReference && !baseType.isInstanceOf[SignatureType.OptionLikeEnum]) {
              base
            } else {
              Box(baseType)(loadTypeInfo(baseType), base)
            }
        }
        state(e) = res

      case e: (CHIR.UnboxToValue | CHIR.CastToConcrete) =>
        val base = state(e.value)
        val baseType = resolver.typeSig(e.targetTpe)
        val value = if (baseType.isRecord) {
          baseType match {
            case baseType: SignatureType.OptionLikeEnum if baseType.someType.isTypeVariable =>
              base
            case _ =>
              if (base.tpe.isTraceableRefType) {
                UnboxRec(baseType)(loadTypeInfo(baseType), base)
              } else {
                base
              }
          }
        } else if (baseType.isTraceableReference) {
          base
        } else {
          if (base.tpe.isTraceableRefType) {
            Unbox(baseType)(loadTypeInfo(baseType), base)
          } else {
            base
          }
        }
        state(e) = value

      case CHIR.GetException =>
        state(e) = state(catchProxy)

      case e: CHIR.RawArrayInitByValue =>
        val array = state(e.array).asInstanceOf[NewArray]
        val len = state(e.size)
        val value = state(e.initValue)
        assert(array.lengths == Seq(len), s"RawArrayInitByValue($array, $len, $value)")
        val arrayType = array.allocType
        if (arrayType.getArrayElemType.isZST) {
          stats.count(StatsKind.ArrayZeroingElimination, "Unit array zeroing eliminated on parsing", array)
        } else {
          AJArrayFill(arrayType, arrayType.getArrayElemType)(array, value)
        }

      case e: CHIR.RawArrayLiteralInit =>
        val array = state(e.array).asInstanceOf[NewArray]
        val values = e.elementValues.map(state.apply)
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

      case e: CHIR.GetRTTI =>
        state(e) = ThisTypeInfoBy(ReceiverParam())
    }

    private def staticFieldRef(globalVar: CHIR.GlobalVar): CangjieFieldReference = {
      val symRefType  = globalVar.declaringDef
        .map(d => asClassType(resolver.symType(d).get))
        .getOrElse(resolver.findClass(globalVar.packageName).get)

      val refType = fromSymType(symRefType)
      val name = resolver.symName(globalVar)
      val sig = resolver.typeSig(globalVar.tpe)
      val f = symRefType.findDeclaredFieldOrNull(xstr(name), sig) ensuring
        (_ != null, s"cannot find field '$name' with signature '${sig.toJETSignature}' in class '${symRefType.getName}'")

      CangjieFieldReference(f.getFieldIndex, Some(f), refType, f.getType)
    }

    private def calcMethodRef(declType: SymClassType, refType: SignatureType, _name: String, func: CHIR.Func): MethodReference = {
      val isStatic = declType.isCangjiePackage || func.attributes.contains(Attribute.Static)
      val (sig, _, isCFunc, vararg) = resolver.functionSig(func, hasReceiver = !isStatic)

      // TODO: explain
      val name = if (!isStatic && declType.isVariableSizeType && !refType.isVariableSizeType) {
        resolver.mutWithoutTI(_name)
      } else {
        _name
      }

      val method = refType match {
        case refType: (SignatureType.InstantiatedType | SignatureType.CangjieEnum) =>
          val cparams = genericParams(refType)
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

    private def callMethod(target: MethodReference, outerTypeInfo: Option[SignatureType], thisType: Option[SignatureType], retType: SignatureType, _paramTypes: Seq[SignatureType], _args: Seq[Node], thisTypeInfo: Option[Node]): Node = {
      import SpecialParameter.*

      val (receiver, receiverType, args, paramTypes) = if (!target.hasMethod || target.method.isStatic) {
        (None, None, _args, _paramTypes)
      } else {
        (_args.headOption, _paramTypes.headOption, _args.tail, _paramTypes.tail)
      }

      def adjustArg(a: Node, from: SignatureType, to: SignatureType): Node = {
        (from, to) match {
          case (_: SignatureType.Box, _: SignatureType.Box) => a // no adjustment necessary
          case (from: SignatureType.Box, _) => shouldNotReachHere((from, to))

          case (from: SignatureType.TypeVariable, to: SignatureType.Box) => a
          case (from: SignatureType.OptionLikeEnum, to: SignatureType.Box) if from.someType.isTypeVariable => a
          case (from: SignatureType.OptionLikeEnum, to: SignatureType.Box) if from.isNullableOption =>
            Box(from)(loadTypeInfo(from), a)
          case (from, to: SignatureType.Box) if from.isTraceableReference || from.isVariableSizeType => a
          case (from, to: SignatureType.Box) => Box(from)(loadTypeInfo(from), a)

          case (_, _) => a
        }
      }

      val ugArgs = for ((a, (from, to)) <- args zip (paramTypes zip target.method.getSignature.parameterTypes.map(ABI.makeABISigType)))
        yield adjustArg(a, from, to)

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
              val box = SignatureType.Box(retType)
              if (box.containsTypeVariables) {
                NewGeneric(box)(loadTypeInfo(retType))
              } else {
                New(box)()
              }

            case SignatureType.Address =>
              // Type variable
              // TODO: prepareSRet
              val memType = ReferenceType.cangjieStdCoreObject.sigType
              val mem = StackAlloc.Local(memType, workaroundForNonZeroedTraceableRecords = true)
              val value = if (!retType.isInstanceOf[SignatureType.OptionLikeEnum] && (retType.isTraceableReference || retType.isTypeVariable)) {
                Null()
              } else {
                val box = SignatureType.Box(retType)
                if (box.containsTypeVariables) {
                  NewGeneric(box)(loadTypeInfo(retType))
                } else {
                  New(box)()
                }
              }
              StoreMemory(memType.toAsm, memType, atomic = false)(mem, value)
              mem

            case _ =>
              assert(abiRetValType.isRecord)
              assert(retType.isRecord)
              StackAlloc.Local(retType, workaroundForNonZeroedTraceableRecords = true)
          }

          Seq(abiRetVal)
        case Receiver => receiver.map(adjustArg(_, receiverType.get, target.methodType.parameterType(target.methodType.getReceiverArgIdx))) ensuring (_.nonEmpty)
        case SMutRecord => Seq(SMutRecArg(receiver.get))
        case SMutObject => Seq(SMutObjectArg(SMutRecArg(receiver.get)))
        case OuterTypeInfo =>
          val t = outerTypeInfo.get
          if (t.isCangjieClosure) {
            Seq(ThisTypeInfoBy(receiver.get))
          } else {
            Seq(loadTypeInfo(t))
          }
        case SpecialParameter.ThisTypeInfo =>
          Seq(thisTypeInfo.getOrElse(loadTypeInfo(thisType.get)))
        case GenericFuncParams =>
          target.asInstanceOf[InstantiatedMethodReference].instantiatedTypeParameters.map { t =>
            loadTypeInfo(t)
          }
        case x @ (MutRecord | MutObject) =>
          shouldNotReachHere(x)
      }

      ensurePrepared(PreparationRequired.forInvoke(target))

      packageInitCheck(target.refClass)

      val call = if (target.methodType.hasThisTypeInfoParameter && target.accessKind == MAK.STATIC_VIRTUAL) {
        InvokeVirtualStatic(target)(abiArgs: _*)
      } else {
        target.accessKind match {
          case MAK.VIRTUAL | MAK.INTERFACE =>
            SaveCallRefTypeInfo(loadTypeInfo(target.refType.sigType))
          case _ =>
        }
        Invoke(target)(abiArgs: _*)
      }

      val res = if (abiRetVal != null) {
        abiRetValType match {
          case _ if abiRetValType.isZST =>
            abiRetVal

          case t: SignatureType.Box =>
            // Variable-sized type
            retType match {
              case rt: SignatureType.OptionLikeEnum if rt.isNullableOption =>
                Unbox(retType)(loadTypeInfo(retType), abiRetVal)
              case rt: SignatureType.OptionLikeEnum if rt.someType.isTypeVariable =>
                abiRetVal
              case _ =>
                UnboxRec(retType)(loadTypeInfo(retType), abiRetVal)
            }

          case SignatureType.Address =>
            // Type variable
            // TODO: prepareSRet
            val memType = ReferenceType.cangjieStdCoreObject.sigType
            val obj = LoadMemory(memType.toAsm, memType, atomic = false)(abiRetVal)
            if (!retType.isInstanceOf[SignatureType.OptionLikeEnum] && (retType.isTraceableReference || retType.isTypeVariable)) {
              obj
            } else {
              if (retType.isRecord) {
                UnboxRec(retType)(loadTypeInfo(retType), obj)
              } else {
                Unbox(retType)(loadTypeInfo(retType), obj)
              }
            }

          case _ =>
            assert(abiRetValType.isRecord)
            abiRetVal
        }
      } else {
        call
      }
      if (retType.isShortIntegral) {
        BitFieldExtract.BFX(IntType, 0, retType.toAsm.sizeInBits, signExtension = false, res)
      } else {
        res
      }
    }

    private def inlinedCall(target: MethodReference)(args: Node*) = {
      assert(target.method.isAJInline, s"${target}")
      assert(!target.isInterfCall)
      Invoke(target)(args: _*)
    }

    private def genericParams(x: SignatureType): Seq[SignatureType] = CangjieEnumWrapper.skip(x) match {
      case x: SignatureType.InstantiatedType => x.instantiatedTypeParameters
      case x: SignatureType.CangjieEnum => x.params
      case _ => Seq.empty
    }

    private def fieldChain(host: SignatureType, path: Seq[Long]): Seq[CangjieFieldReference] = {
      path.scanLeft[CangjieFieldReference](null) { case (fr, idx) =>
        val refType = if (fr == null) host else fr.fieldType

        def fieldRef(idx: Long): CangjieFieldReference = {
          val refClass = asClassType(refType)
          val allClassFields = (refClass +: refClass.getSuperClasses.toArray).reverse.flatMap(_.getDeclaredFields)
          val next = allClassFields.filterNot(_.isStatic).apply(idx.toInt)
          val fieldType = next.getType.instantiate(genericParams(refType), Seq.empty)
          CangjieFieldReference(idx, Some(next), refType, fieldType)
        }

        refType match {
          case refType if refType.isCangjieLambda =>
            fieldRef(idx + 2) // First two fields are synthesized for lambda function pointers
          case refType: SignatureType.Tuple =>
            val fieldType = refType.params(idx.toInt)
            CangjieFieldReference(idx, None, refType, fieldType)
          case refType: SignatureType.OptionLikeEnum =>
            assert(!refType.isNullableOption && !refType.someType.isTypeVariable, refType)
            val fieldType = idx match {
              case 0 => SignatureType.Boolean
              case 1 => refType.someType
            }
            CangjieFieldReference(idx, None, refType, fieldType)
          case refType: (SignatureType.ZeroSizedEnum | SignatureType.PrimitiveBasedEnum | SignatureType.UnionBasedEnum) =>
            shouldNotReachHere(refType)
          case refType =>
            fieldRef(idx)
        }
      }.drop(1) // drop first null value
    }

    private def declaredFields(host: SignatureType): Seq[CangjieFieldReference] = {
      val hostClass = asClassType(host)
      val baseIdx = hostClass.getSuperClasses.map(_.getDeclaredFields.size).sum
      hostClass.getDeclaredFields.toSeq map { f =>
        CangjieFieldReference(baseIdx + f.getFieldIndex, Some(f), host, f.getType.instantiate(genericParams(host), Seq.empty))
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

    private def allocEnumObject(objType: SignatureType, args: Seq[Node]): Node = {
      val obj = New(objType)()
      val superType = asClassType(objType).getSuperClassSig
      for ((arg, fieldRef) <- args zip (declaredFields(superType) ++ declaredFields(objType))) {
        val sig = fieldRef.fieldType
        if (sig.isZST) {
          // Nothing to do

        } else if (needsCopy(sig)) {
          val mem = GetFieldSeqRef(Seq(fieldRef))(maybeDerivedPtrBase(obj), obj)
          copy(sig, mem, arg)

        } else {
          StoreFieldSeq(Seq(fieldRef))(maybeDerivedPtrBase(obj), obj, arg)
        }
      }
      obj
    }

    private def allocTuple(tupleType: SignatureType.Tuple, args: Seq[Node]): Node = {
      val mem = StackAlloc.Local(tupleType)
      for (((arg, i), sig) <- args.zipWithIndex zip tupleType.params) {
        val fieldRef = CangjieFieldReference(i, None, tupleType, sig)
        if (sig.isZST) {
          // Nothing to do

        } else if (needsCopy(sig)) {
          val tupleField = GetFieldSeqRef(Seq(fieldRef))(maybeDerivedPtrBase(mem), mem)
          copy(sig, tupleField, arg)

        } else {
          StoreFieldSeq(Seq(fieldRef))(maybeDerivedPtrBase(mem), mem, arg)
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

    private def typeInfoSigs(fields: Seq[CangjieFieldReference]): Seq[SignatureType] = {
      fields.head.refType +: fields.map(_.fieldType)
    }

    private def typeInfos(fields: Seq[CangjieFieldReference]): Seq[Node] = {
      typeInfoSigs(fields) map loadTypeInfo
    }

    private def writeBarrier(): Unit = {
      // FIXME
    }

    private def constString(str: String): Node = {
      val sigType = fromSymType(resolver.findClass("std.core:String").get)
      val xstring = currentInlineContext.method.getDeclaringClass.getConstString(xstr(str))

      val sa = StackAlloc.Local(sigType)
      InitStringRecord(sigType, isStatic = false, xstring)(sa)
      sa
    }
  }

  private def maybeDerivedPtrBase(rcv: Node): Node = rcv match {
    case rcv if rcv.tpe.isTraceableRefType => rcv
    case rcv: Param if rootMethod.hasMutRecordParameter && rcv.num == rootMethod.getMutRecordArgIdx =>
      rootMethodParam(rootMethod.getMutObjectArgIdx)
    case rcv: FieldSeqOperation => rcv.baseRef
    case rcv: Phi =>
      val args = Phi.transitiveValueArgs(rcv).filterNot(_.isInstanceOf[NoValue])
      singleton(args) match {
        case Some(arg) => maybeDerivedPtrBase(arg)
        case None => notImplemented(s"non-trivial phi function: $rcv, leaf args: $args")
      }
    case rcv => DerivedPtr.Local()
  }

  private def resolveProxiesInArgs(): Unit = {
    for (n <- all[SMutObjectArg]) {
      val actual = n.recArg.receiver match {
        case rcv: InstanceFieldSeqOperation => rcv.base
        case rcv: FieldSeqOperation => DerivedPtr.Global()
        case rcv: StackAlloc => DerivedPtr.Local()
        case rcv: Param =>
          assert(rcv.num == rootMethod.getMutRecordArgIdx)
          rootMethodParam(rootMethod.getMutObjectArgIdx)
      }
      n.replaceBy(actual)
    }
    for (n <- all[SMutRecArg]) {
      n.replaceBy(n.receiver)
    }
  }

  private def loadTypeInfo(t: SignatureType): Node = {
    if (t.containsTypeVariables) {
      import SignatureType.*
      t match {
        case t: ClassTypeVariable =>
          if (rootMethod.getDeclaringClass.isCangjiePackage) {
            genericTypeInfoParam(t.idx)
          } else {
            GenericTypeArg(t.idx)(outerTypeInfoParam())
          }
        case t: LocalTypeVariable => genericTypeInfoParam(t.idx)
        case t: InstantiatedType  => LoadTypeInfoGeneric(t)(t.instantiatedTypeParameters.map(loadTypeInfo): _*)
        case t: ArraySlice        => LoadTypeInfoGeneric(t)(loadTypeInfo(t.elemType))
        case t: CangjieArray      => LoadTypeInfoGeneric(t)(loadTypeInfo(t.elemType))
        case t: VArray            => LoadTypeInfoGeneric(t)(loadTypeInfo(t.elemType))
        case t: Tuple             => LoadTypeInfoGeneric(t)(t.params.map(loadTypeInfo): _*)
        case t: CangjieEnum       => LoadTypeInfoGeneric(t)(t.params.map(loadTypeInfo): _*)
        case t => shouldNotReachHere(t)
      }
    } else {
      LoadTypeInfo(t)
    }
  }

  private def genericTypeInfoParam(idx: Int): Node = {
    assert(rootMethod.getMethodType.hasGenericFuncParams)
    rootMethodParam(rootMethod.getMethodType.getGenericFuncParamsStartIdx(rootMethod.getGenericInfo.constraints.size) + idx)
  }

  private def outerTypeInfoParam(): Node = {
    assert(rootMethod.getMethodType.hasOuterTypeInfoParameter)
    rootMethodParam(rootMethod.getMethodType.getOuterTypeInfoArgIdx)
  }

}
