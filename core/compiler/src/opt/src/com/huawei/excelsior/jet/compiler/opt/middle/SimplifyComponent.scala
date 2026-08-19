/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.util.Overflows
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.StatsKind.NoReturn
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.{STD_CORE_OPTION_PREFIX, STD_CORE_OPTION_ARRAY_PREFIX}
import com.huawei.excelsior.jet.compiler.{Env, PreparationKind, PreparationRequired, RTSProc, StatsKind}
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.explosion.Explosion
import com.huawei.excelsior.jet.compiler.types.References.Point
import com.huawei.excelsior.jet.compiler.opt.serialization.OptExtraInfo
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.util.WhileChanged.*
import com.huawei.excelsior.jet.compiler.options.BoolOption.WorkaroundForJET12354
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Field, MethodReferenceAccessKind, SignatureType, Type as SymType}
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.util.{ScalaCollections, Worklist}
import xscala.util.MathUtils.{bits, bitsSigned, isNBits, minExtended}

import scala.PartialFunction.condOpt

/**
 * Simplification of IR nodes.
 *
 * @author cypok
 * @author alexm
 * @author paul
 */
trait SimplifyComponent extends DivisionByConstantOptimizations with OptExtraInfo with ConstantStringOptimizations with Explosion { self: Universe =>
  import PartialFunction.cond

  private def findPhiCycleRootedAt(root: Phi): Option[Sets[Phi]#QSet] = {
    if (collect[Phi](root.args).isEmpty) {
      return None // fast path
    }

    // First, collect all phies reachable via use-def edges from `root`
    val above = Phi.transitivePhiArgs(root)

    if (!above.contains(root)) {
      None // `root` is not a part of any phi-cycle
    } else {
      // Then collect all phies reachable via def-use edges from `root`
      val below = Phi.transitivePhiUses(root)
      // The sought cycle consists of phies that both use-def & def-use reachable from `root`
      val phies = Sets[Phi].newQSet(above intersect below)
      assert(phies.nonEmpty)
      Some(phies)
    }
  }

  def eliminateCyclicPhies(root: Phi): Boolean = cond(findPhiCycleRootedAt(root)) {
    case Some(phies) =>
      val values = phies flatMap (_.args)
      values --= phies
      cond(values.size) {
        case 0 => shouldNotReachHere("phies without values")
        case 1 => phies foreach { x => replaceTransitively(x, values.head) }; true
      }
  }

  /** "Pulls" down [[Cmp]] with the same [[Condition]] through Phi function that uses them.
    *
    * Example transformation:
    * {{{
    *   Phi(Cmp[EQ](x1, y1), Cmp[EQ](x2, y2), ..., Cmp[EQ](xn, yn))
    * }}}
    * is replaced by
    * {{{
    *   Cmp[EQ](Phi(x1, x2, ..., xn), Phi(y1, y2, ..., yn))
    * }}}
    */
  def optimizePhiOfCmp(phi: Phi): Boolean = {
    cond(ScalaCollections.uniqueValue(phi.args map (_.proto))) {
      case Some(proto: Cmp.Proto) =>
        val (lArgs, rArgs) = phi.argsSeq.map{ case Cmp(_, l, r) => (l, r) }.unzip
        val Seq(lPhi, rPhi) = Seq(lArgs, rArgs) map { xs =>
          Phi(proto.keyType)(phi.block +: xs: _*)
        }
        phi.replaceBy(proto(lPhi, rPhi))
        true
    }
  }

  /** Optimizes web of phi-functions, all arguments of which are mutually equivalent controlled nodes except for
    * their control arguments.
    *
    * Consider phi-function `phi`(`x`, `y`), where `x` and `y` are equivalent controlled nodes except for control
    * argument. If we can move `x` and `y` to `phi` block (change control arguments of `x` and `y`), we can replace
    * `phi` and `y` by `x`.
    *
    * We do not apply this optimization to cyclic phies, because it requires more complicated analysis (searching of
    * point where we should put controlled node). Feel free to implement it and eliminate copy-paste between this
    * optimization and `eliminateCyclicPhies`.
    *
    * TODO: actually this optimization introduces new kind of nodes equality. It can be useful in other optimizations.
    */
  private def optimizePhiOfControlledNodes(root: Phi): Boolean = {
    // Point where we will put controlled node. This may be conservative (e.g. node can be placed in some dominator).
    // TODO: implement more complicated analysis, based on dominators (and apply this optimization to cyclic phies)
    val point = root.block

    val ws = Worklist(root)
    for (phi <- ws.accumulate) ws ++= collect[Phi](phi.args)
    val phies = Sets[Phi].newQSet(ws.iterator)

    lazy val phiesNotUsedOutsideOfWebExceptRoot = phies forall {
      case `root` => true
      case x => x.uses forall {
        case p: Phi => phies contains p
        case _ => false
      }
    }

    // Returns true, iff `x` can be moved down to `point`.
    def canBeMovedToPoint(x: Node): Boolean = x match {
      case _: PinnedNode =>
        false

      case _: HasInMemory =>
        false // NOTE: we can move memory nodes too, but it requires more complicated analysis.

      case cn: ControlledNode => cn.uses forall {
        case x: Phi if phies contains x => true
        case x: Phi if x.block == point => false
        case x: ControlledNode => point dominates x.inCtrl
        case x: ControlNode => point dominates x
        case _ => false
      }

      case _ =>
        false
    }

    // Returns true, iff `x` and `y` are equivalent except for their control arguments.
    def sameNonControlArgs(x: Node, y: Node): Boolean = (x, y) match {
      case (x: ControlledNode, y: ControlledNode) => (x.proto == y.proto) && (x.notControlArgs == y.notControlArgs)
      case _ => false
    }

    val args = phies flatMap (_.args)
    args --= phies

    val head = args.head

    if ((args forall canBeMovedToPoint) &&
      (args.tail forall (a => sameNonControlArgs(a, head))) &&
      phiesNotUsedOutsideOfWebExceptRoot) {

      val replacement = Node.clonePartially(head) { case e if e.isControl => point }
      head.asInstanceOf[ControlledNode].inCtrl = root.block
      bulkReplace {
        replaceTransitively(root, replacement) // Other phies will die in DCE
        args foreach { arg => replaceTransitively(arg.asInstanceOf[FloatingNode], replacement) }
      }
      true

    } else {
      false
    }
  }

  private def optimizeArrayLength(arrayLength: ArrayLength): Boolean = {
    cond(arrayLength.array) {
      case getStatic: GetStatic =>
        cond(globallyAnalyzeClinitForArrayFieldLength(getStatic.field)) {
          case Some((_, length)) =>
            replaceTransitively(arrayLength, IntegralConst(arrayLength.tpe)(length))
            true
        }
      case GetField(Java.Lang.String.value, _, _, ConstString(XStr(str))) =>
        stats.count(StatsKind.ConstStrings, "ArrayLength reduced to IConst", pos = arrayLength.pos)
        replaceTransitively(arrayLength, IConst(str.length))
        true
    }
  }

  private def optimizeGetField(getField: GetField): Boolean = {
    // TODO: add more GetField optimizations if needed
    optimizeCangjieOptionalRecord(getField)
  }

  /** Optimization of Cangjie `Option<T>` emptiness test, where `T` is record (see JET-15688).
    *
    * In Cangjie `Option<T>` is defined as follows:
    * {{{
    *   enum Option<T> {
    *       Some(T)
    *       | None
    *
    *       func isNone(): Bool {
    *           match (this) {
    *               case None => true
    *               case Some(_) => false
    *           }
    *       }
    *
    *       ...
    *   }
    * }}}
    *
    * Which is transformed into following structure in HLIR when `Option<T>` is instantiated with record type `T`:
    * {{{
    *   struct Option_T(
    *       let constructor: Bool,
    *       let val: T
    *
    *       func isNone(): Bool {
    *           if (this.constructor == 1) true
    *           else false
    *       }
    *
    *       ...
    *   )
    * }}}
    *
    * where
    *   - `None` is represented as `Option_T( 1, uninitialized_T )`;
    *   - `Some(x)` is represented as `Option_T( 0, x )`.
    *
    * If `T` has any non-nullable reference field `f` (or series of flat fields leading to `f`),
    * then we can replace the test from `isNone` with
    * {{{
    *       func isNone(): Bool {
    *           if (this.val.f == null) true
    *           else false
    *       }
    * }}}
    *
    * Note that we actually do it indirectly by replacing
    * {{{
    *   this.constructor
    * }}}
    * with
    * {{{
    *   CondVal(this.val.f == null)
    * }}}
    * and then [[Identities]] will handle the rest of the transformation.
    *
    */
  private def optimizeCangjieOptionalRecord(getField: GetField): Boolean = {
    val field = getField.field
    val clazz = field.getDeclaringClass

    def isOptionalRecord(c: SymType): Boolean = c.getName.startsWith(STD_CORE_OPTION_PREFIX)
    def isOptionalSlice(c: SymType): Boolean = c.getName.startsWith(STD_CORE_OPTION_ARRAY_PREFIX)

    def isOptimizableOptionalRecord: Boolean =
      (env.enabled(BoolOption.SimplifyCangjieOptionalRecords) && isOptionalRecord(clazz)) ||
        (env.enabled(BoolOption.SimplifyCangjieOptionalSlices) && isOptionalSlice(clazz))

    // Reference field can be located at arbitrary depth of intermediate flat fields.
    // So we collect this sequence of fields from Option<T> to the field.
    lazy val pathToRefField = {
      def findRefField(c: ClassType): List[Field] = {
        import SignatureType.*
        for (f <- c.getDeclaredFields) f.getType match {
          case t: (NonNullableWrapper | NullableWrapper.Base) =>
            return List(f)

          case t: Record if isOptionalRecord(t.symType) =>
            // Skip nested optional records.
            //
            // We unable to optimize `Option<Option<T>>` where `T` is record,
            // because we can't distinguish `None` and `Some(None)` cases
            // using null-test of some reference field.

          case t: (Record | ArraySlice) =>
            val res = findRefField(asClassType(t))
            if (res.nonEmpty) {
              return f :: res
            }

          case _ => // Skip value fields.
        }
        List.empty
      }
      findRefField(clazz)
    }

    if (clazz.isRecord && isOptimizableOptionalRecord && field.getName == "constructor" && pathToRefField.nonEmpty) {
      val getRefField = withIncrementalGCM {
        insertCodeAfter(getField.upperPoint) {
          pathToRefField.foldLeft(getField.obj) { (obj, f) =>
            GetField(f)(obj)
          }
        }
      }
      val repl = CondVal(Cmp(getRefField.tpe, Condition.EQ)(AnyNull(getRefField.tpe), getRefField))
      replaceTransitively(getField, repl)
      true

    } else {
      false
    }
  }

  private def optimizeGetStaticPrimitive(getStatic: GetStatic): Boolean = {
    val field = getStatic.field
    field.getType.isPrimitive && cond(globallyAnalyzeClinitForPrimitiveFieldValue(field)) {
      case Some((_, constValue)) =>
        replaceTransitively(getStatic, NumericalConst(constValue))
        true
    }
  }

  private def optimizeDelayedOp(x: DelayedOp): Boolean = x match {
    case x: DelayedGet => optimizeGetDelayed(x)
    case x: DelayedPut => optimizePutDelayed(x)
    case x: DelayedInstanceMethodVNum => optimizeGetInstanceMethodVNumDelayed(x)
    case x: DelayedInstanceFieldAddress => optimizeGetInstanceFieldAddress(x)
    case x: DelayedMethodAddr => optimizeGetMethodAddr(x)
  }

  private def optimizeGetDelayed(x: DelayedGet): Boolean = {
    // TODO: optimize duplication
    val refClass = typeProvider.findClass(x.className)
    if (refClass == null) {
      return false
    }

    val field = refClass.findDeclaredFieldOrNull(x.fieldName)
    if (field == null) {
      return false
    }
    val result = insertCodeBefore(x) {
      // FIXME: nullCheck, clinitCheck, typeCheck
      GetField(field)(x.obj)
    }
    strikeOutWithValueUses(x, result)
    true
  }

  private def optimizePutDelayed(x: DelayedPut): Boolean = {
    // TODO: optimize duplication
    val refClass = typeProvider.findClass(x.className)
    if (refClass == null) {
      return false
    }

    val field = refClass.findDeclaredFieldOrNull(x.fieldName)
    if (field == null) {
      return false
    }
    insertCodeBefore(x) {
      // FIXME: nullCheck, clinitCheck, typeCheck, writeBarrier
      PutField(field)(x.obj, x.value)
    }
    strikeOut(x)
    true
  }

  private def optimizeGetInstanceMethodVNumDelayed(x: DelayedInstanceMethodVNum): Boolean = {
    // TODO: optimize duplication
    val refClass = typeProvider.findClass(x.className)
    if (refClass == null) {
      return false
    }

    assert(!refClass.isInterface, "not yet supported")
    // TODO: use signature
    val methodRef = refClass.getMethodRefToLocalOrNull(x.methodName, null, MethodReferenceAccessKind.VIRTUAL)
    if (methodRef == null) {
      return false
    }

    val result = insertCodeBefore(x) {
      assert(methodRef.hasVirtualMethodSlot)
      IConst(methodRef.virtualMethodSlot)
    }
    strikeOutWithValueUses(x, result)
    true
  }

  private def optimizeGetInstanceFieldAddress(x: DelayedInstanceFieldAddress): Boolean = {
    // TODO: optimize duplication
    val refClass = typeProvider.findClass(x.className)
    if (refClass == null) {
      return false
    }

    assert(!refClass.isInterface, "not yet supported")
    val field = refClass.findDeclaredFieldOrNull(x.fieldName)
    if (field == null) {
      return false
    }

    val result = insertCodeBefore(x) {
      Add(ConcealRef(x.obj), IntegralConst(AddrType)(field.getInstanceFieldOffset))
    }

    strikeOutWithValueUses(x, result)
    true
  }

  private def optimizeGetMethodAddr(x: DelayedMethodAddr): Boolean = {
    // TODO: optimize duplication
    val refClass = typeProvider.findClass(x.className)
    if (refClass == null) {
      return false
    }

    // TODO: use signature
    val target = refClass.findDeclaredMethodOrNull(x.methodName, null)
    if (target == null) {
      return false
    }

    val result = insertCodeBefore(x) {
      val managedContext = x.inlineContext.method.isManaged
      val preparationKind = PreparationKind(managedContext, `lazy` = true)
      ensurePrepared(PreparationRequired.forMethodAddr(target), preparationKind)
      SymbolAddress(target)
    }

    strikeOutWithValueUses(x, result)
    true
  }


  private def optimizeSwitchDefaultExits[S](switch: AnySwitch[S]): Boolean = {
    val defaultBlock = switch.defaultExit.target

    def sameAsDefault(exit: AnySwitch.Exit[S]) = {
      // same target block
      exit.target == defaultBlock &&
        // same phi arguments
        (defaultBlock.phies forall (phi => phi.phiArg(exit.outEdge) == phi.phiArg(switch.defaultExit.outEdge)))
    }

    val (defaultExits, nonDefaultExits) = switch.caseExits partition sameAsDefault

    if (nonDefaultExits.isEmpty) {
      replaceByGoto(switch.defaultExit)
      true

    } else if (defaultExits.nonEmpty) {
      AnySwitch.dropExits(defaultExits: _*)
      true

    } else {
      false
    }
  }

  private def replaceSwitchByIf(switch: AnySwitch[_]): Boolean = cond(switch.caseExits) {
    case Seq(caseExit) =>
      val i = If(switch.inCtrl, switch.inMemory, caseExit.genCaseCheck())
      caseExit replaceUsesBy i.trueExit
      switch.defaultExit replaceUsesBy i.falseExit
      decommit(switch)
      true
  }

  private def optimizeDeadValueRangeFilter(n: RawValueRangeFilter) = {
    if (n.filteredValue.valueUses forall (_.isInstanceOf[RawValueRangeFilter])) {
      strikeOut(n)
      true
    } else {
      false
    }
  }

  private def detectNoReturnCall(call: Call): Boolean = {
    def noReturnCall() = {
      if (!call.targetRef.hasMethod) {
        false
      } else {
        call.targetRef.method.isAjNoReturn || cond(call) {
          case AnyDirectCall(t) => locallyAnalyzeMethod(t).exists(_.isNoReturn)
        }
      }
    }

    if (noReturnCall() && !call.outCtrl.isInstanceOf[Halt]) {
      val goto = Block.splitAfter(call)
      stats.count(NoReturn, s"call of method ${call.name} marked as 'NoReturn'")
      replaceByHalt(goto)
      true
    } else {
      false
    }
  }

  object ErroneousArrayLike {
    import AnyNewArray._
    def throwOnNegativeConstLength(n: Node, allocType: SymType, inlineContext: InlineContext): Option[RTSProc] = {
      condOpt(n) {
        case IConst(v) if v < 0 => negativeArraySizeErrorProc(allocType, inlineContext)
      }
    }

    def unapply(n: SpinalNode): Option[RTSProc] = n match {
      case AnyNewArray.Erroneous(proc) => Some(proc)

      case n: NewString => throwOnNegativeConstLength(n.length, n.allocType.symType, n.inlineContext)
      case n: NewArrayCopy => throwOnNegativeConstLength(n.length, n.allocType.symType, n.inlineContext)
      case n: NewArrayCopyRT if !n.isCopyOfRange => throwOnNegativeConstLength(n.to, n.allocType.symType, n.inlineContext)

      case n: NewArrayCopyRT if n.isCopyOfRange => condOpt((n.from, n.to)) {
        case (IConst(from), _) if from < 0 => ArrayIndexCheck.errorProc(n.allocType.symType, n.inlineContext)
        case (IConst(from), IConst(to)) if from > to => RTSProc.JR_ThrowIllegalArgumentException
      }


      case _ => None
    }
  }

  /** Replace runtime routine with compile-time type array allocation if it became known due to optimizations. */
  private def newArrayRTKnownType(n: NewArrayRT): Boolean = {
    cond(n.klass) {
      case ClassObject(allocType) =>
        assert(allocType.isJavaArray && (!allocType.getArrayBase.isPrimitive || allocType.getArrayDimnum > 1))
        replaceByCode(n) { NewArray(SignatureType.fromSymType(allocType))(n.length) }
        true
    }
  }

  private def replaceCompletelyByThrow(node: SpinalNode, rtsProc: RTSProc, args: Node*): Unit = {
    insertErrorRTSCallBefore(node, rtsProc)(args: _*)
    replaceValueUsesByNoValueAndStrikeOut(node)
  }

  private def simplifyDivisorCheck(n: DivisorCheck, divisor: Long): Boolean = {
    if (divisor != 0) {
      strikeOut(n)
      true
    } else {
      replaceCheckByThrow(n)
      true
    }
  }

  private def simplifyCheckedOp(n: CheckedOp): Boolean = {
    import CheckedOp.Kind._
    val width = n.width.nbits

    n match {
      case n @ CheckedOp(ADD | MUL, _, _) if !areArgsSorted(n.l, n.r) => n.swapArgs()
      case _ =>
    }

    val isOverflow = n match {
      case CheckedOp(SUB, l, r) if l == r => false
      case CheckedOp(ADD | SUB | MUL, _, IntegralConst(0)) => false
      case CheckedOp(MUL, _, IntegralConst(1)) => false
      case CheckedOp(DIV, _, IntegralConst(v)) if v != -1 => false
      case CheckedOp(DIV, IntegralConst(v), _) if v != minExtended(width) => false
      case CheckedOp(DIV, l, r) if l == r => false
      case n @ CheckedOp(DIV, _, _) if !n.signed => false
      case n @ CheckedOp(ADD, IntegralConst(l), IntegralConst(r)) => Overflows.add(l, r, n.asmType)
      case n @ CheckedOp(SUB, IntegralConst(l), IntegralConst(r)) => Overflows.sub(l, r, n.asmType)
      case n @ CheckedOp(MUL, IntegralConst(l), IntegralConst(r)) => Overflows.mul(l, r, n.asmType)
      case _ => return false
    }

    if (isOverflow) {
      insertErrorRTSCallBefore(n, n.throwProc)()
      replaceValueUsesByNoValueAndStrikeOut(n)
    } else {
      CheckedOp.replaceWithUncheckedCopy(n)
    }

    true
  }

  private def optimizeFieldSeq(n: InstanceFieldSeqOperation): Boolean = {
    cond(ReinterpretCast.skip(n.obj)) {
      case g: GetFieldSeqRef =>
        val fields = (collect[CangjieReferenceNode](g.fields) ++ n.fields).toSeq
        n match {
          case n: GetFieldSeqRef => replaceTransitively(n,
            GetFieldSeqRef.proto(FieldSeqOperation.refTpe(fields), FieldSeqOperation.resTpe(fields))(n.inCtrl +: g.obj +: fields: _*))
          case n: LoadFieldSeq => replaceByCode(n) {
            LoadFieldSeq(g.obj, fields: _*)
          }
          case n: StoreFieldSeq => replaceByCode(n) {
            StoreFieldSeq(g.obj, n.inValue, fields: _*)
          }
        }
        true
      case g: GetStaticFieldSeqRef =>
        val fields = (collect[CangjieReferenceNode](g.fields) ++ n.fields).toSeq
        n match {
          case n: GetFieldSeqRef => replaceTransitively(n, GetStaticFieldSeqRef.proto(FieldSeqOperation.resTpe(fields))(n.inCtrl +: fields: _*))
          case n: LoadFieldSeq => replaceByCode(n) {
            LoadStaticFieldSeq(fields: _*)
          }
          case n: StoreFieldSeq => replaceByCode(n) {
            StoreStaticFieldSeq(n.inValue, fields: _*)
          }
        }
        true
    }
  }

  private def optimizePutField(p: PutField): Boolean = {
    cond(p.inValue0) {
      case g: GetField if p.obj == g.obj && p.inMemory == g.inMemory && p.field == g.field =>
        strikeOut(p)
        true
    }
  }

  /** Eliminates unnecessary indirection in two consecutive copying operations:
    * {{{
    *   CopyStructure(X, Y)
    *   CopyStructure(Z, X)
    * }}}
    * to
    * {{{
    *   CopyStructure(X, Y)
    *   CopyStructure(Z, Y)
    * }}}
    *
    * Additionally this transformation removes "read" use of `X`,
    * so it can be eliminated more efficiently via
    * [[com.huawei.excelsior.jet.compiler.opt.middle.explosion.Explosion.expressExplodeAllObjects express explosion]]
    */
  private def optimizeCopyStructure(n: CopyStructure): Boolean = {
    cond(n.inMemory) {
      case m: CopyStructure if m.dst == n.src =>
        n.src = m.src
        true
    }
  }

  private def optimizeArrayGet(arrayGet: ArrayGet): Boolean = {
    cond(arrayGet) {
      case ArrayGet(_, _, GetField(Java.Lang.String.value, _, _, ConstString(XStr(str))), IConst(accessIndex)) if 0 <= accessIndex && accessIndex < str.length =>
        stats.count(StatsKind.ConstStrings, "ArrayGet reduced to IConst", pos = arrayGet.pos)
        replaceTransitively(arrayGet, IConst(str.charAt(accessIndex)))
        true
    }
  }

  private def optimizeGetClass(gc: GetClass): Boolean = {
    cond(nodeTypeAt(gc.obj, gc.inCtrl)) {
      case Point(root, false) =>
        replaceByCode(gc) {
          ClassObject(root.symType)()
        }
        true
    }
  }

  private def optimizeInstanceDescriptorBy(n: InstanceDescriptorBy): Boolean = {
    cond(nodeTypeAt(n.obj, n.inCtrl)) {
      case Point(tpe, false) =>
        replaceTransitively(n, InstanceDescriptor(tpe.symType)(n.inCtrl))
        true
    }
  }


  private def replaceThrowByGoto(thrw: Throw): Boolean = {
    cond(thrw.xHandlerOption) {
      case Some(xHandler) =>
        val templateEdge = thrw.xpoint.xEdge

        withJoinAfter(xHandler, Seq(thrw)) { thrw =>
          val goto = Block.splitBefore(thrw)
          goto.makeUsesUnreachable()
          goto

        } { join =>
          join(xHandler.catchNode, _.inValue)

          for (phi <- xHandler.phies) {
            val templateArg = phi.phiArg(templateEdge)
            join(phi, _ => templateArg)
          }
        }

        // prevent subsequent transformations
        strikeOut(thrw)

        true
    }
  }

  private def replaceAndByBFX(and: And): Boolean = cond(and.r) {
    case c @ ULConst(value) =>
      val lz = java.lang.Long.numberOfLeadingZeros(value)
      val bc = java.lang.Long.bitCount(value)
      if (lz + bc == typeSizeInBits(LongType)) {
        replaceTransitively(and, BitFieldExtract(c.tpe, 0, bc, signExtension = false, and.l))
        true
      } else {
        false
      }
  }

  private def optimizeCmpConstWithBFX(cmp: Cmp): Boolean = {
    import Condition.*
    cond(cmp) {
      case Cmp(EQ | NE, bfx: BitFieldExtract, IntegralConst(c)) =>
        optimizeBFXWithConstants(bfx, Seq(c)) match {
          case Some(Seq(newConst)) =>
            replaceTransitively(cmp, Cmp(bfx.argType, cmp.op)(bfx.arg, IntegralConst(bfx.argType)(newConst)))
            true
          case _ => false
        }
    }
  }

  private def optimizeSwitchWithBFX(switch: Switch): Boolean = {
    cond(switch.selector) {
      case bfx: BitFieldExtract =>
        val cases = switch.cases.map(_.toLong)
        optimizeBFXWithConstants(bfx, cases) match {
          // Optimizing only when cases are unchanged to avoid replacing whole switch
          case Some(`cases`) =>
            switch.selector = bfx.arg
            true
          case _ => false
        }
    }
  }

  private def optimizeBFXWithConstants(bfx: BitFieldExtract, constants: Seq[Long]): Option[Seq[Long]] = {
    import BitFieldExtract.*

    def asmTypeApproximation(n: Node): AsmType = n match {
      case n: AnyMemoryAccess => n.accessType
      case n => ValueType.toAsm(n.tpe)
    }

    bfx match {
      case bfx @ BFX(0, size, signed, x)
        // It is only safe to remove BFX if constants fits into original extension size
        if constants.forall(isNBits(signed, _, size)) &&
          // Unless there are other uses of `x` besides `bfx`,
          // removing `bfx` for single use might result in
          // unnecessary extra variable being introduced
          x.valueUses.exists(_ != bfx) =>

        val asm = asmTypeApproximation(x)
        // Removing `bfx` is only possible if it was extending and not truncating
        // and if `x` size was equal to `size`.
        // Note that we intentionally avoid weird cases where size of `x` is less than `size`
        // (e.g. signExtend(U8, 0, 16)), since these weird casts must be rare in real code if present at all.
        if (asm.width.nbits == size && size <= typeSizeInBits(bfx.tpe)) {
          // Constant value needs to be sign- or zero-extended according to signedness of `x`
          val newConstants = for (c <- constants) yield if (asm.signed) bitsSigned(c, 0, size - 1) else bits(c, 0, size - 1)
          Some(newConstants)
        } else {
          None
        }

      case _ => None
    }
  }

  /** Pull up all controlled nodes, dependent from markers to allow them to be optimized by GVN.
    * Solution for problems like JET-13518.
    */
  private def pullUpNodesControlledByMarker(marker: Marker): Boolean = {
    val usesSize = marker.controlUses.size
    if (usesSize == 1) {
      false
    } else {
      assert(usesSize > 1)
      val prev = marker.inCtrl
      val next = marker.outCtrl
      marker.replaceUses { case e if e.isControl && e.target != next => prev }
      true
    }
  }

  private def eliminateOffsetFromHost(offset: MutFunc.Offset): Boolean = cond(offset) {
    case MutFunc.Offset(_, combine: MutFunc.Combine) =>
      offset.host.replaceBy(combine.host)
      offset.replaceBy(combine.offset)
      true
  }

  private def pullBeforeMutFuncCombine(elemPtr: GetElementPtr) = cond(elemPtr) {
    case GetElementPtr(field, combine @ MutFunc.Combine(host, offset)) =>
      val elemOffs = IntegralConst(AddrType)(field.getInstanceFieldOffset)
      val newCombine = MutFunc.Combine(host, Add(offset, elemOffs), combine.tpe)
      elemPtr.replaceBy(ReinterpretCast(elemPtr.base.tpe, AddrType)(newCombine))
      true
  }

  private def optimizeInitStringRecord(init: InitStringRecord): Boolean = {
    if (!isExplosionOfCangjieStringAllowed) {
      // If explosion of cangjie string stack alloc is not allowed, then either our target is CBC or
      // our target is any of amd/arm and this is not yet time to explode them.
      return false
    }

    replaceByCode(init) {
      val arrayType = SignatureType.CangjieArray(SignatureType.UInt8)
      PutField(Cangjie.Support.String.myData)(init.obj, ConstString(init.str, arrayType.symType)())
    }
    true
  }

  private def optimizeDivRemByConst(op: IDivRemOp, d: Long): Boolean = {
    val newNode = getOptimizedDivRemByConst(op, d)

    if (newNode != null) {
      replaceTransitively(op, newNode)
      true
    } else {
      false
    }
  }

  def simplifyIR(): Boolean = {
    whileChanged { changed =>
      resetValueNumbering()

      lazy val cold = findWarmAndColdBlocks()

      bulkReplace {
        for (node <- allNodes) {
          def replaceNodeWith(node: NonControlNode, dst: Node): Unit = {
            if (dst != node) {
              replaceTransitively(node, dst)
              changed()
            }
          }

          if (!isO1Compiled) {
            node match {
              case init: InitStringRecord if optimizeInitStringRecord(init) =>
                changed()

              case x: DelayedOp if optimizeDelayedOp(x) =>
                changed()

              case dc @ DivisorCheck(IntegralConst(c)) if simplifyDivisorCheck(dc, c) =>
                changed()

              case op: CheckedOp if simplifyCheckedOp(op) =>
                changed()

              case op @ IDivRemByConstOp(c) if optimizeDivRemByConst(op, c) =>
                changed()

              case mut: MutFunc.Offset if eliminateOffsetFromHost(mut) =>
                changed()

              case elemPtr: GetElementPtr if pullBeforeMutFuncCombine(elemPtr) =>
                changed()

              case phi: Phi if eliminateCyclicPhies(phi) =>
                changed()

              case phi: Phi if optimizePhiOfCmp(phi) =>
                changed()

              case phi: Phi if optimizePhiOfControlledNodes(phi) =>
                changed()

              case arrayLength: ArrayLength if optimizeArrayLength(arrayLength) =>
                changed()

              case x: GetField if optimizeGetField(x) =>
                changed()

              case getStatic: GetStatic if optimizeGetStaticPrimitive(getStatic) =>
                changed()

              case switch: AnySwitch[_] if optimizeSwitchDefaultExits(switch) =>
                changed()

              case switch: AnySwitch[_] if replaceSwitchByIf(switch) =>
                changed()

              case badNew @ ErroneousArrayLike(throwProc) =>
                replaceCompletelyByThrow(badNew, throwProc)
                changed()

              case newArrayRT: NewArrayRT if newArrayRTKnownType(newArrayRT) =>
                changed()

              case dcc @ CheckCastTrustedDelayed(obj, AJString(typeName, _)) =>
                val targetType = typeProvider.resolveJavaTypeByName(typeProvider.getAJObjectType, typeName)
                assert(!targetType.isPrimitive)
                replaceByCode(dcc) { CheckCast(SignatureType.fromSymType(targetType), trusted = true)(obj) }
                changed()

              case branch @ If(_: Not) =>
                If.invert(branch)
                changed()

              case pm @ PutMemoryOperation(memType, bfx: BitFieldExtract) if bfx.offset == 0 &&
                memType.width.nbits == bfx.size && ValueType(memType) == bfx.argType =>

                assert(memType.isIntegral || memType.isPointer, s"$pm $memType $bfx")
                // This should be done by decoupling PutValue node into Cast + PutValue and grouping back in back end.
                pm.inValue0 = bfx.arg
                changed()

              case n: RawValueRangeFilter if optimizeDeadValueRangeFilter(n) => // TODO: JET-12121
                changed()

              case n: PutField if optimizePutField(n) =>
                changed()

              case n: CopyStructure if n.src == n.dst =>
                strikeOut(n)
                changed()

              case n: CopyStructure if optimizeCopyStructure(n) =>
                changed()

              case arrayGet: ArrayGet if optimizeArrayGet(arrayGet) =>
                changed()

              case call: Call if optimizeConstantStringInvokes(call) =>
                changed()

              case call: Call if detectNoReturnCall(call) =>
                changed()

              case branch @ If(_: TauTest) if env.enabled(WorkaroundForJET12354) && profile.isPGOHost && cold(branch.block) =>
                replaceByGoto(branch.falseExit)
                changed()

              case gc: GetClass if optimizeGetClass(gc) =>
                changed()

              case n: InstanceDescriptorBy if optimizeInstanceDescriptorBy(n) =>
                changed()

              case thrw: Throw if replaceThrowByGoto(thrw) =>
                changed()

              case and: And if replaceAndByBFX(and) =>
                changed()

              case n: Cmp if optimizeCmpConstWithBFX(n) =>
                changed()

              case n: Switch if optimizeSwitchWithBFX(n) =>
                changed()

              case marker: Marker if pullUpNodesControlledByMarker(marker) =>
                changed()

              case node: NonControlNode =>
                replaceNodeWith(node, commit(node))

              case _: ControlNode =>
                assert(commit(node) == node)
            }
          } else {
            node match {
              case init: InitStringRecord if optimizeInitStringRecord(init) =>
                changed()

              case mut: MutFunc.Offset if eliminateOffsetFromHost(mut) =>
                changed()

              case elemPtr: GetElementPtr if pullBeforeMutFuncCombine(elemPtr) =>
                changed()

              case x: DelayedOp if optimizeDelayedOp(x) =>
                changed()

              case thrw: Throw if replaceThrowByGoto(thrw) =>
                changed()

              case op: CheckedOp if simplifyCheckedOp(op) =>
                changed()

              case op: InstanceFieldSeqOperation if optimizeFieldSeq(op) =>
                changed()

              case node: NonControlNode =>
                replaceNodeWith(node, commit(node))

              case _: ControlNode =>
                assert(commit(node) == node)
            }
          }
        }
      }
    }
  }
}
