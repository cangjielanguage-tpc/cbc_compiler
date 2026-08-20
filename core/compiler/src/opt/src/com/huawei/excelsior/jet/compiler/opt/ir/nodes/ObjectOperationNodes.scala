/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.common.Arch.*
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.common.LanguagePack.SCALA
import com.huawei.excelsior.common.Environment
import com.huawei.excelsior.jet.assembler
import com.huawei.excelsior.jet.assembler.AsmType
import xscala.util.StringOps.asciiCapitalize
import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Tag, Universe}
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeMethodReference, CallKind, Field, Method, MethodReference, MethodReferenceAccessKind, MethodType, SignatureType, TypeKind, ClassType as SymClassType, Type as SymType}
import com.huawei.excelsior.jet.compiler.{Domain, Env, PreparationKind, RTConst, RTSProc, StatsKind, symlevel}
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.codeemitter.BarrierKind
import com.huawei.excelsior.jet.compiler.abi.DAIGenerator.DAITarget
import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind
import com.huawei.excelsior.jet.common.XString.ascii
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.types.Guards.*
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.types.Approximation
import com.huawei.excelsior.jet.compiler.opt.middle.types.LoweredReferences.LoweredReferenceApprox
import com.huawei.excelsior.jet.compiler.opt.middle.types.LoweredReferences.LoweredReferenceApprox.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.STATIC_VIRTUAL
import com.huawei.excelsior.jet.compiler.types.RecordType
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ClassType, ReferenceType}
import com.huawei.excelsior.jet.compiler.util.Log.Kind
import com.huawei.excelsior.jet.compiler.util.{Log, Sets}
import com.huawei.excelsior.jet.util.{Closure, ScalaCollections}

import scala.PartialFunction.{cond, condOpt}

/**
 * Object operation nodes are nodes, that corresponds with bytecode object operations.
 *
 * @author paul
 * @author cypok
 * @author conwor
 * @author alexm
 */

trait ObjectOperationNodes { self: Universe with Nodes =>

  import BitFieldExtract._
  import CallKind._

  trait TypeFilterNode extends SpinalNode {
    def filteredArg: Node
    def filterType(tpe: Approximation, point: ControlNode): (Approximation, Boolean)
  }

  abstract class AbstractNullCheck(proto: PureCheckPrototype[_ <: AbstractNullCheck]) extends PureCheck(proto) {
    protected def objArgIdx: Int
    def obj: Node = arg(objArgIdx)
    def obj_=(n: Node): Unit = { updateArg(objArgIdx, n) }
  }

  class NullCheck private (proto: NullCheck.Proto) extends AbstractNullCheck(proto)
    with ThrowingPureCheck with TypeFilterNode with NotProducesValue {

    protected def objArgIdx: Int = 2

    def filteredArg: Node = obj

    def filterType(tpe: Approximation, point: ControlNode) = tpe match {
      case tpe: ReferenceApprox =>
        if (point == this) {
          tpe subtract RefNull
        } else {
          assert(point == this.xpoint)
          tpe weakIntersect RefNull
        }
      case tpe: LoweredReferenceApprox =>
        if (point == this) {
          tpe subtract LoweredRefNull
        } else {
          assert(point == this.xpoint)
          tpe weakIntersect LoweredRefNull
        }
      case _ => shouldNotReachHere(tpe)
    }

    def domain = proto.domain

    override def throwInfo = domain match {
      case Domain.JAVA => (RTSProc.JR_ThrowNullPointerException, Seq())
      case Domain.AJ => (RTSProc.JR_ThrowAJNullPointerException, Seq())
      case Domain.CANGJIE => (RTSProc.JR_ThrowCJNoneValueException, Seq())
      case Domain.SCALA => (RTSProc.JR_ThrowScalaNullPointerException, Seq())
    }
  }

  object NullCheck {
    case class Proto private[NullCheck] (trusted: Boolean, domain: Domain)
      extends PureCheckPrototype[NullCheck](ControlType, MemoryType, EopType.Any)(ControlType)() with ControlTagged[NullCheck] {

      def newInstance() = new NullCheck(this)
    }

    private def defaultDomain: Domain = {
      val domainOwner = if (currentInlineContext != null) currentInlineContext.method else rootMethod
      domainOwner.getDomain
    }

    def apply(args: Node*): NullCheck = apply(false)(args: _*)
    def apply(trusted: Boolean): Proto = apply(trusted, defaultDomain)
    def apply(trusted: Boolean, domain: Domain): Proto = Prototype.intern(Proto(trusted, domain))
    def unapply(n: NullCheck): Option[(Node, Node, Node)] = Some(n.inCtrl, n.inMemory, n.obj)
  }


  /** Special key corresponding to tau test.
    * May be used to compare semantics of tau tests.
    */
  case class GuardKey(obj: Node, guard: Guard)

  /** TauTest is used for guarded code execution (safe speculation).
    * Replacement of any TauTest by [[False]] should be correct transformation
    * (i.e. true-path is a specialized version of false-path).
    */
  // TODO: unify TypeTest and MethodTest nodes (with different protos)
  sealed abstract class TauTest protected (proto: TauTest.Proto[_ <: FloatingNode])
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode with ControlledNode {

    assert(currentPhase > CompilerPhase.Serialization, "this node must not be serialized")

    /** Some tau tests should not be used in diamond dust optimization,
      * e.g. MethodTest inserted with +InstrumentTauFastPath option.
      *
      * This flag should be set to false for such nodes upon creation.
      */
    var canBeUsedInDiamondDust = true

    def obj: Node = arg(1)

    def guard: Guard

    def info: TauInfo = proto.info
    def key = GuardKey(obj, guard)
  }

  object TauTest {
    abstract class Proto[N <: TauTest](extraArgTypes: Type*)
      extends FixedArgs[N](Seq(ControlType, TRefType) ++ extraArgTypes: _*)(ConditionType)
        with PrototypeStrictNodeClass[N, N] {

      def guard: Guard
      def info: TauInfo
    }

    def apply(key: GuardKey, info: TauInfo, ctrl: Node): TauTest =
      apply(key.guard, info, ctrl, key.obj)

    def apply(guard: Guard, info: TauInfo, ctrl: Node, obj: Node, rcvType: ReferenceType = null): TauTest = {
      guard match {
        case tg: TypeGuard =>
          TypeTest(tg, info)(ctrl, obj)

        case mg: MethodGuard =>
          MethodTest.withReceiverType(mg, info, ctrl, obj, rcvType)

        case _ => shouldNotReachHere("unexpected guard: " + guard)
      }
    }

    def unapply(x: TauTest) = Some(x.guard, x.info, x.obj)

    def log = Log(Kind.TauOpt)
  }

  class TypeTest private (proto: TypeTest.Proto) extends TauTest(proto) {
    def guard: TypeGuard = proto.guard
  }

  object TypeTest {
    case class Proto private[TypeTest] (guard: TypeGuard, info: TauInfo) extends TauTest.Proto[TypeTest]() {
      def newInstance() = new TypeTest(this)
    }
    def apply(guard: TypeGuard, info: TauInfo) = Prototype.intern(Proto(guard, info))
    def unapply(x: TypeTest) = Some(x.guard)
  }

  class MethodTest private (proto: MethodTest.Proto) extends TauTest(proto) {
    require(MethodTest.canBeGeneratedFor(target))

    def guard: MethodGuard = proto.guard

    /** This arg is required for proper interface method test lowering (see `MiscOps#lowerTauTest`).
      *
      * For virtual method test this value is set to constant `ciaoStubForInvokeVirtual`
      * and should never be used anywhere.
      *
      * For interface method test:
      * - if the test was created from InvokeInterface node (e.g. in devirtualization),
      *   it inherits `ciao` from that invoke;
      * - otherwise a new WeakCast node is created.
      */
    def ciao = arg(2)

    def originalRef = guard.originalRef
    def original = guard.original
    def target = guard.target
  }

  object MethodTest {
    case class Proto private[MethodTest] (guard: MethodGuard, info: TauInfo) extends TauTest.Proto[MethodTest](AddrIntType) {
      def newInstance() = new MethodTest(this)
    }

    private def apply(guard: MethodGuard, info: TauInfo) = Prototype.intern(Proto(guard, info))

    private def ciaoStubForInvokeVirtual = IntegralConst(AddrIntType)(-1)

    def apply(guard: MethodGuard, info: TauInfo, ctrl: Node, obj: Node): MethodTest = {
      withReceiverType(guard, info, ctrl, obj, null)
    }

    def withReceiverType(guard: MethodGuard, info: TauInfo, ctrl: Node, obj: Node, rcvType: ReferenceType = null): MethodTest = {
      MethodTest.withCIAO(guard, info, ctrl, obj) {
        // Note that it does not reuse IMT offset for MethodTest.
        val interf = guard.originalRef.refClass
        rcvType match {
          case klass: ClassType if klass implements SignatureType.fromSymType(interf) => lightInterfCast(klass.symType, interf)
          case _ => WeakCast(interf)(obj, WeakCast.NoCheck())
        }
      }
    }

    def withCIAO(guard: MethodGuard, info: TauInfo, ctrl: Node, obj: Node)(ciao: => Node): MethodTest = {
      val originalRef = guard.originalRef
      apply(guard, info)(ctrl, obj, if (originalRef.isInterfCall) ciao else ciaoStubForInvokeVirtual)
    }

    def unapply(x: MethodTest) = Some(x.obj, x.originalRef, x.target, x.ciao)

    def canBeGeneratedFor(target: Method): Boolean = {
      if (targetArch == CBC) return false // TODO: consider using method IDs instead of body addresses to allow MethodTests in CBC

      val host = target.getDeclaringClass

      if (host.isThinClass) return false

      symbolLinker.accessKind(host.getTypeHandle) match {
        case AccessKind.DIRECT => true
        case AccessKind.FAR => shouldNotReachHere()
      }
    }
  }


  /** Some check which has no side-effects and generated as nop in enduser mode. */
  trait AssertNode extends SpinalNode with HasInMemory


  /** Checks that clinit analysis results could be correctly used at the point
    * in case of aggressive clinit analysis.
    * It means that field must be already initialized.
    *
    * Sometimes we could even check that it is initialized by its "expected" value
    * however it's not the main goal of this check.
    *
    * Does nothing if clinit analysis failed to analyse this field.
    */
  final class AggressiveClinitAnalysisAssert private (proto: AggressiveClinitAnalysisAssert.Proto)
    extends NodeWithFixedArgs(proto) with FieldOperation with AssertNode with NotProducesValue {

    def field = proto.field
  }

  object AggressiveClinitAnalysisAssert {
    case class Proto private[AggressiveClinitAnalysisAssert] (field: Field)
      extends FixedArgs[AggressiveClinitAnalysisAssert](ControlType, MemoryType)(ControlType)
        with FieldOperationProto with ControlTagged[AggressiveClinitAnalysisAssert] {

      def newInstance() = new AggressiveClinitAnalysisAssert(this)
    }

    def apply(field: Field) = Prototype.intern(Proto(field))
  }


  sealed abstract class AbstractInitializationCheck protected
    (proto: AbstractInitializationCheck.Proto[_ <: AbstractInitializationCheck], allowDeferred: Boolean = false)
    extends NodeWithFixedArgs(proto) with SpinalNode with HasInMemory with CompositeNode with NotProducesValue {

    require(allowDeferred || !klass.isDeferred)
    def klass: symlevel.ClassType = proto.klass
  }

  object AbstractInitializationCheck {
    sealed abstract class Proto[N <: AbstractInitializationCheck] extends FixedArgs[N](ControlType, MemoryType)(ControlType) {
      def klass: symlevel.ClassType
    }
  }


  /** Returns whether class is initialized. */
  class InitializedTest private(proto: InitializedTest.Proto) extends NodeWithFixedArgs(proto) with FloatingNode with CompositeNode
      with ControlledNode with HasInMemory {

    require(!klass.isDeferred)
    def klass: SymClassType = proto.klass
  }

  object InitializedTest {
    case class Proto private[InitializedTest](klass: symlevel.ClassType)
      extends FixedArgs[InitializedTest](ControlType, MemoryType)(ConditionType) {

      def newInstance() = new InitializedTest(this)
    }

    def apply(klass: symlevel.ClassType) = Prototype.intern(Proto(klass))

    def unapply(n: InitializedTest) = Some(n.klass)
  }


  /** Checks that class is "clinited": initialization has finished or in progress in current thread.
    * Otherwise starts initialization process.
    */
  class Clinit private (proto: Clinit.Proto) extends AbstractInitializationCheck(proto, allowDeferred = true) with SpinalMemoryNode with CanThrow with Idempotent with TypeFilterNode with NotProducesValue {

    def filteredArg = JVMState()

    def filterType(tpe: Approximation, point: ControlNode) = tpe match {
      case tpe: VMStateApprox =>
        if (klass.isDeferred) {
          (tpe, false)
        } else if (point == this) {
          (tpe withClinit klass, true)
        } else {
          assert(point == this.xpoint)
          (tpe, false) // TODO: with failed clinit
        }
      case _ => shouldNotReachHere(tpe)
    }
  }

  object Clinit {
    case class Proto private[Clinit] (klass: symlevel.ClassType)
      extends AbstractInitializationCheck.Proto[Clinit] with ControlMemoryTagged[Clinit] {

      def newInstance() = new Clinit(this)
    }

    def apply(klass: symlevel.ClassType) = Prototype.intern(Proto(klass))
    def unapply(n: Clinit) = Some(n.klass)

    /** Returns a clinit exit (where initialized test failed). */
    def wrapUnderInitializedTest(node: Clinit): If.Exit = {
      import PredicateConstructor._
      val (Seq(branch), clinitBlock, _) = wrapUnderPredicate(node, !atom(InitializedTest(node.klass)(_, node.inMemory)))
      clinitBlock.markAsCold()
      branch.exits.find(_.target == clinitBlock).get
    }
  }


  /** Initializes given package and its dependencies if they are not initialized yet.
    *
    * Throws exception in case of circular package initialization.
    */
  class PackageInit private(proto: PackageInit.Proto) extends AbstractInitializationCheck(proto, allowDeferred = true)
    with SpinalMemoryNode with CanThrow with Idempotent with NotProducesValue

  object PackageInit {
    case class Proto private[PackageInit](klass: symlevel.ClassType)
      extends AbstractInitializationCheck.Proto[PackageInit] with ControlMemoryTagged[PackageInit] {

      def newInstance() = new PackageInit(this)
    }

    def apply(klass: symlevel.ClassType) = Prototype.intern(Proto(klass))
    def unapply(n: PackageInit) = Some(n.klass)
  }


  /** Checks whether given package is fully initialized, otherwise throws exception. */
  class PackageInitCheck private(proto: PackageInitCheck.Proto) extends AbstractInitializationCheck(proto, allowDeferred = true)
    with CanThrow with Idempotent with NotProducesValue

  object PackageInitCheck {
    case class Proto private[PackageInitCheck](klass: symlevel.ClassType)
      extends AbstractInitializationCheck.Proto[PackageInitCheck] with ControlTagged[PackageInitCheck] {

      def newInstance() = new PackageInitCheck(this)
    }

    def apply(klass: symlevel.ClassType) = Prototype.intern(Proto(klass))
    def unapply(n: PackageInitCheck) = Some(n.klass)
  }


  /** Checks that class is "prepared". Otherwise starts preparation process. */
  class PreparationCheck private(proto: PreparationCheck.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with CompositeNode with TypeFilterNode with NotProducesValue {
    def klass = proto.klass
    def kind = proto.kind

    def filteredArg = JVMState()

    def filterType(tpe: Approximation, point: ControlNode): (VMStateApprox, Boolean) = tpe match {
      case tpe: VMStateApprox =>
        if (point == this) {
          // Block optimizations for non-assertion forced PreparationChecks,
          // like PROLOGUE_PREPARATION ones in Cangjie exported methods.
          (tpe withPreparation klass, kind.assertionOnly || !kind.forced)
        } else {
          assert(point == this.xpoint)
          (tpe, false) // TODO: with failed preparation check
        }
      case _ => shouldNotReachHere(tpe)
    }
  }

  object PreparationCheck {

    case class Proto private[PreparationCheck](klass: symlevel.Type, kind: PreparationKind)
      extends FixedArgs[PreparationCheck](ControlType)(ControlType) with ControlTagged[PreparationCheck] {

      def newInstance() = new PreparationCheck(this)
    }

    def apply(klass: symlevel.Type, kind: PreparationKind): Proto = Prototype.intern(Proto(klass, kind))
    def unapply(n: PreparationCheck) = Some(n.klass)

    def markForPreparation(check: PreparationCheck): Unit = {
      // For CBC target:
      // * bootstrap non-assertion makes no sense
      // * eager non-assertion currently has no PreparationInfo representation in CBC file format
      // * all the rest are currently unsupported because we don't support SymbolAddress node in CBC
      assert(targetArch != CBC, s"$check in CBC is not supported")

      val statKind = if (check.kind.bootstrap) {
        if (!check.klass.isNonBootstrapAnnotated) env.markForBootstrapPreparation(check.klass)
        StatsKind.BootstrapPreparation
      } else if (!check.kind.`lazy`) {
        env.markForPreparation(check.klass)
        StatsKind.EagerPreparation
      } else {
        StatsKind.LazyPreparation
      }
      stats.count(statKind,
        s"type ${check.klass.getName} marked for ${check.kind} preparation from method ${rootMethod.getFullName}" +
          s" in ${hostingClass.getName}", check)
    }
  }


  /** Asserts something about class initialization state.
    * In case of failed assertion fatal error is raised.
    */
  // TODO: unify assert nodes
  sealed abstract class AbstractInitializationAssert protected (proto: AbstractInitializationAssert.Proto[_ <: AbstractInitializationAssert])
    extends AbstractInitializationCheck(proto) with AssertNode {

    assert(Env.isWorkMode)
  }

  object AbstractInitializationAssert {
    sealed abstract class Proto[N <: AbstractInitializationAssert]
      extends AbstractInitializationCheck.Proto[N] with ControlTagged[N]
  }


  /** Asserts that class is "clinited": initialization has finished or in progress in current thread.
    * Checks that [[Clinit]] might be removed at the point.
    */
  class ClinitedAssert private (proto: ClinitedAssert.Proto) extends AbstractInitializationAssert(proto) with CanThrow

  object ClinitedAssert {
    case class Proto private[ClinitedAssert] (klass: symlevel.ClassType)
      extends AbstractInitializationAssert.Proto[ClinitedAssert] {

      def newInstance() = new ClinitedAssert(this)
    }

    def apply(klass: symlevel.ClassType) = Prototype.intern(Proto(klass))
  }


  /** Asserts that class is completely initialized (not "clinited").
    */
  class InitializedAssert private (proto: InitializedAssert.Proto) extends AbstractInitializationAssert(proto)

  object InitializedAssert {
    case class Proto private[InitializedAssert] (klass: symlevel.ClassType)
      extends AbstractInitializationAssert.Proto[InitializedAssert] {

      def newInstance() = new InitializedAssert(this)
    }

    def apply(klass: symlevel.ClassType) = Prototype.intern(Proto(klass))
  }


  trait InlineableAllocator extends CompositeNode with SpinalNode {
    /** Indicator that this operation should be lowered via inlining of RT-allocator. */
    var shouldBeInlined = false

    def allocType: SignatureType
    def inlinedAllocator: symlevel.Method
  }

  trait InlineableAllocatorWithGuard extends InlineableAllocator {
    import InlineableAllocatorWithGuard._

    /** Value of the guard check under which inlining of RT-allocator should take place.
      * If no value is set the inline should be direct (without guard).
      */
    var sizeGuard: SizeGuard = NoGuard
  }

  object InlineableAllocatorWithGuard {
    sealed abstract class SizeGuard
    object NoGuard extends SizeGuard
    case class PointGuard(size: Int) extends SizeGuard
    case class LevelGuard(size: Int) extends SizeGuard
  }

  /** New operation creates objects in heap or on stack without class initialization. */
  trait AnyNew extends SpinalMemoryNode with CompositeNode with ProducesValue {
    def allocType: SignatureType
  }

  trait AnyNewStackAllocated extends AnyNew {
    assert(currentPhase > CompilerPhase.Serialization, "this node must not be serialized")

    /** Indicator that this object allocation is in loop. */
    var inLoop = false

    override def name = super.name + (if (inLoop) "(inLoop)" else "")
  }


  abstract class AnyNewClass protected (proto: AnyNewClassProto[_ <: AnyNewClass])
    extends NodeWithFixedArgs(proto) with AnyNew {

    require(!allocType.isAbstractClass && !allocType.isInterface && !allocType.isDeferred)
    override def allocType: SignatureType = proto.allocType
  }

  abstract class AnyNewClassProto[N <: AnyNewClass](val allocType: SignatureType) extends FixedArgs[N](ControlType, MemoryType)(ValueType.fromSig(allocType))
    with ControlMemoryValueTagged[N]


  /** Creates new class instance in heap. */
  class New private (proto: New.Proto) extends AnyNewClass(proto) with CanThrow with InlineableAllocator {
    def inlinedAllocator = RT.Allocator.newObjectInlined(allocType.symType.getHeapObjectSize)
  }

  object New {
    case class Proto private[New] (_allocType: SignatureType) extends AnyNewClassProto[New](_allocType) {
      assert(!allocType.isDeferred)
      def newInstance() = new New(this)
    }

    def apply(allocType: SignatureType) = Prototype.intern(Proto(allocType))
    def unapply(n: New) = Some(n.allocType.symType) // TODO: do not downgrade to symtype
  }

  /** Creates new class instance on stack. */
  class NewStackAllocated private (proto: NewStackAllocated.Proto) extends AnyNewClass(proto) with AnyNewStackAllocated {
    var stackAllocatedByEvacuateAnalysis = false
  }

  object NewStackAllocated {
    case class Proto private[NewStackAllocated] (_allocType: SignatureType) extends AnyNewClassProto[NewStackAllocated](_allocType) {
      assert(!allocType.isDeferred)
      def newInstance() = new NewStackAllocated(this)
    }

    def apply(allocType: SignatureType) = Prototype.intern(Proto(allocType))
  }


  abstract class AnyNewArray protected (proto: AnyNewArray.Proto[_ <: AnyNewArray])
    extends NodeWithVarArgs(proto) with AnyNew {

    require(allocType.isArray)
    require(!allocType.isDeferred)
    override def allocType: SignatureType = proto.allocType

    final def lengths: Seq[Node] = argsTail(proto.fixedArgsCount)

    def negativeArraySizeErrorProc: RTSProc =
      AnyNewArray.negativeArraySizeErrorProc(allocType.symType, inlineContext)
  }

  object AnyNewArray {
    abstract class Proto[N <: AnyNewArray](_allocType: SignatureType) extends VarArgs[N](ControlType, MemoryType)(TypedArrayOperation.lenType(_allocType))(TRefType)
      with ControlMemoryValueTagged[N] {
      def allocType: SignatureType
    }

    def shouldCheckNegativeLength(arrayType: SymType, inlineContext: InlineContext) = arrayType.isJBCArray || arrayType.isCangjieArray

    def negativeArraySizeErrorProc(arrayType: SymType, inlineContext: InlineContext): RTSProc = {
      if (arrayType.isCangjieArray) {
        assert(inlineContext.method.getDomain == Domain.CANGJIE)
        RTSProc.JR_ThrowCJNegativeArraySizeException
      } else {
        if (inlineContext.method.getDomain == Domain.SCALA) {
          RTSProc.JR_ThrowScalaNegativeArraySizeException
        } else {
          RTSProc.JR_ThrowNegativeArraySizeException
        }
      }
    }

    def unapply(n: AnyNewArray): Option[(symlevel.Type, Seq[Node])] =
      Some(n.allocType.symType, n.lengths)


    object Erroneous {
      def unapply(n: AnyNewArray): Option[RTSProc] = {
          for (IntegralConst(v) <- n.lengths) {
            if (shouldCheckNegativeLength(n.allocType.symType, n.inlineContext) && v < 0) {
              return Some(n.negativeArraySizeErrorProc)
            }
          }
          None
      }
    }
  }


  /** Creates new array instance in heap. */
  class NewArray private (proto: NewArray.Proto) extends AnyNewArray(proto) with CanThrow with InlineableAllocatorWithGuard {
    /** Indicator that array is filled with ZeroValue and elem type has no traced fields. */
    var uninitialized = false

    def inlinedAllocator = RT.JavaAllocator.newArrayInlined
  }

  object NewArray {
    case class Proto private [NewArray] (allocType: SignatureType) extends AnyNewArray.Proto[NewArray](allocType) {
      def newInstance(): NewArray = new NewArray(this)
    }

    def apply(allocType: SignatureType) = Prototype.intern(Proto(allocType))

    def unapply(n: NewArray): Option[(SignatureType, Seq[Node])] =
      Some(n.allocType, n.lengths)
  }


  /** Creates new array instance on stack. */
  class NewArrayStackAllocated private (proto: NewArrayStackAllocated.Proto) extends AnyNewArray(proto) with AnyNewStackAllocated

  object NewArrayStackAllocated {
    case class Proto private [NewArrayStackAllocated] (allocType: SignatureType) extends AnyNewArray.Proto[NewArrayStackAllocated](allocType) {
      def newInstance(): NewArrayStackAllocated = new NewArrayStackAllocated(this)
    }

    def apply(allocType: SignatureType) = Prototype.intern(Proto(allocType))
  }


  /** Mimics [[AnyNewArray]] checks without any allocation. Acts as placeholder in [[ArrayIndexCheck]]. */
  class NewArrayMimic private(proto: NewArrayMimic.Proto) extends AnyNewArray(proto) {
    def shouldCheckLengths = proto.shouldCheckLengths
    override def canThrow = shouldCheckLengths
  }

  object NewArrayMimic {
    case class Proto private [NewArrayMimic](allocType: SignatureType, shouldCheckLengths: Boolean) extends AnyNewArray.Proto[NewArrayMimic](allocType) {
      def newInstance(): NewArrayMimic = new NewArrayMimic(this)
    }

    def apply(allocType: SignatureType, shouldCheckLengths: Boolean) = Prototype.intern(Proto(allocType, shouldCheckLengths))
  }


  /** Combined NewArray and System.arraycopy for redundant zeroing elimination. */
  class NewArrayCopy private (proto: NewArrayCopy.Proto) extends NodeWithFixedArgs(proto) with AnyNew with CanThrow with InlineableAllocatorWithGuard {
    /** Indicator that original array is filled with ZeroValue and elem type has no traced fields. */
    var uninitialized = false

    override def allocType: SignatureType = proto.allocType
    def inlinedAllocator = RT.JavaAllocator.newArrayCopyInlined

    def length: Node = arg(2)
    def src: Node = arg(3)
    def srcPos: Node = arg(4)
    def count: Node = arg(5)
    def value: Node = arg(6)
  }

  object NewArrayCopy {
    def indexType(allocType: symlevel.Type): Type = if (allocType.isJBCArray) IntType else LongType

    case class Proto private[NewArrayCopy] (allocType: SignatureType)
      extends FixedArgs[NewArrayCopy](ControlType, MemoryType, indexType(allocType.symType), TRefType, indexType(allocType.symType), indexType(allocType.symType), LongType)(TRefType) with ControlMemoryValueTagged[NewArrayCopy] {

      assert(!allocType.isDeferred)
      def newInstance() = new NewArrayCopy(this)
    }

    def apply(allocType: SignatureType) = Prototype.intern(Proto(allocType))
  }


  /** Combined new array allocation for compile time unknown type and System.arraycopy for redundant zeroing elimination.
    * Currently it is a representation of Arrays.copyOf calls for reference types.
    *
    * TODO: refactor allocators hierarchy and make it AnyNew.
    */
  class NewArrayCopyRT private (proto: NewArrayCopyRT.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CompositeNode with CanThrow with ArgDependentTypeNode with InlineableAllocatorWithGuard with ProducesValue {
    def allocType = SignatureType.fromSymType(proto.allocType)
    def isCopyOfRange = proto.isCopyOfRange
    def allocatorProc(inlined: Boolean) = RT.JavaAllocator.newArrayCopyOf(allocType.symType, isCopyOfRange, inlined)
    def inlinedAllocator = allocatorProc(inlined = true)

    override def name: String = simpleName + "[" + (if (isCopyOfRange) "copyOfRange" else "copyOf") + "]"

    def src: Node = arg(proto.srcArgIdx)
    def from: Node = arg(3)
    def to: Node = arg(4)

    def isTypeDependency(edge: Edge): Boolean = {
      edge.targetArgIndex == proto.srcArgIdx
    }
  }

  object NewArrayCopyRT {
    case class Proto private[NewArrayCopyRT] (allocType: SymType, isCopyOfRange: Boolean)
      extends FixedArgs[NewArrayCopyRT](ControlType, MemoryType, TRefType, IntType, IntType)(TRefType) with ControlMemoryValueTagged[NewArrayCopyRT] {

      def srcArgIdx = 2
      def newInstance() = new NewArrayCopyRT(this)
    }

    def apply(allocType: SymType, isCopyOfRange: Boolean) = Prototype.intern(Proto(allocType, isCopyOfRange))
  }


  /** Allocates an array of Class<?> type, which is unknown at compile time.
    * Implementation of AllocArray.newArray intrinsic.
    *
    * TODO: refactor allocators hierarchy and make it AnyNew.
    */
  class NewArrayRT private extends NodeWithFixedArgs(NewArrayRT) with SpinalMemoryNode with CompositeNode with CanThrow with ProducesValue {
    def klass: Node = arg(2)
    def length: Node = arg(3)
  }

  object NewArrayRT extends FixedArgs[NewArrayRT](ControlType, MemoryType, TRefType, IntType)(TRefType) with ControlMemoryValueTagged[NewArrayRT] {
    def newInstance() = new NewArrayRT()
  }

  /** Only for CBC target.
    * Combined NewArray and AJArrayFill.
    * Performs creating new Cangjie array with default primitive value without using loop.
    */
  class NewArrayFill private (proto: NewArrayFill.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CanThrow with ProducesValue {
    def allocType = proto.allocType
    def length: Node = arg(2)
    def value: Node = arg(3)
  }

  object NewArrayFill {
    case class Proto private[NewArrayFill](allocType: SignatureType)
      extends FixedArgs[NewArrayFill](ControlType, MemoryType, LongType, LongType)(TRefType) with ControlMemoryValueTagged[NewArrayFill] {

      def newInstance() = new NewArrayFill(this)
    }

    def apply(allocType: SignatureType) = Prototype.intern(Proto(allocType))

    object ValueEdge extends EdgeMatcher[NewArrayFill](3)
  }


  /** Creates new key string in heap: string object and array of chars. */
  class NewString private extends NodeWithFixedArgs(NewString) with AnyNew with CanThrow {
    def length: Node = arg(2)

    override def allocType: SignatureType = SignatureType.fromSymType(typeProvider.getStringType)
  }

  object NewString extends FixedArgs[NewString](ControlType, MemoryType, IntType)(TRefType) with ControlMemoryValueTagged[NewString] {
    def newInstance() = new NewString
  }


  class WeakCast private (proto: WeakCast.Proto) extends FloatingNodeWithFixedArgs(proto) with CompositeNode {
    def targetType = proto.targetType
    def obj = arg(0)

    // TODO: IntType => check kind
    def dominatingCheck = arg(1)
    def hasDominatingCheck = dominatingCheck.isInstanceOf[AbstractTypeCheck]
    /** Check is spoiled if it was replaced by some other integer node (e.g. Phi, CondVal, IConst(0)). */
    def hasSpoiledDominatingCheck = !hasDominatingCheck && (dominatingCheck match {
      case IConst(WeakCast.NoCheckValue) => false
      case _ => true
    })
  }

  object WeakCast {
    case class Proto private[WeakCast](targetType: symlevel.Type) extends FixedArgs[WeakCast](ValueType(targetType, eopTypeForInterfaces = false, instantiateRich = false), IntType)(AddrIntType) {
      assert(!isStandalone)
      def newInstance() = new WeakCast(this)
    }

    def apply(targetType: symlevel.Type) = Prototype.intern(Proto(targetType))
    def NoCheck() = IConst(NoCheckValue)
    def unapply(wc: WeakCast): Option[(symlevel.Type, Node)] = Some((wc.targetType, wc.obj))

    private[WeakCast] val NoCheckValue = 1
  }


  sealed trait AbstractTypeCheck extends CompositeNode {
    def obj: Node
    def targetType: SignatureType

    var shouldBeInlined: Boolean = false
  }


  /** `targetType` may be absent. */
  class CheckCast private (proto: CheckCast.Proto) extends PureCheck(proto) with AbstractTypeCheck with Idempotent with TypeFilterNode with HasInMemory with ThrowingPureCheck with ProducesValue {
    def targetType: SignatureType = proto.targetType

    override def name: String = simpleName + "[" + targetType + (if (trusted) ", trusted" else "") + "]"

    override def throwInfo = if (targetType.isAJManagedType) {
      (RTSProc.JR_ThrowAJClassCastException, Seq())
    } else if (targetType.isXScalaType) {
      (RTSProc.JR_ThrowScalaClassCastExceptionByObj, Seq(obj))
    } else {
      assert(targetType.isJavaReference)
      (RTSProc.JR_ThrowClassCastExceptionByObj, Seq(obj))
    }

    def obj = arg(2)

    def filteredArg = obj

    def filterType(tpe: Approximation, point: ControlNode) = tpe match {
      case argTpe: ReferenceApprox =>
        if (targetType.isDeferred) {
          (argTpe, false)
        } else {
          val castedType = OpenCone(ReferenceType(asClassType(targetType)), mayBeNull = true)
          if (point == this) {
            argTpe weakIntersect castedType
          } else if (point == this.xpoint) {
            argTpe subtract castedType
          } else {
            shouldNotReachHere((tpe, argTpe))
          }
        }

      case argTpe: LoweredReferenceApprox =>
        val strict = argTpe match {
          case LoweredRefNull | LoweredRefEmpty => true
          case LoweredRefNonNull | LoweredRefNullable => false
        }
        (argTpe, strict)

      case _ => shouldNotReachHere(tpe)
    }

    def unlinkDependentWeakCasts(): Unit = {
      if (hasValueUses) {
        replaceUses { case ValueEdge(_, _: WeakCast) => WeakCast.NoCheck() }
      }
    }
  }

  object CheckCast {
    case class Proto private[CheckCast] (targetType: SignatureType, trusted: Boolean)
      extends PureCheckPrototype[CheckCast](ControlType, MemoryType, ValueType(targetType))(IntType)(targetType.symType) with ControlValueTagged[CheckCast] {

      def newInstance() = new CheckCast(this)
    }

    def apply(targetType: SignatureType, trusted: Boolean = false): Proto =
      Prototype.intern(Proto(targetType, trusted))

    def unapply(n: CheckCast): Option[(SignatureType, Node)] = Some(n.targetType, n.obj)
  }


  /** Special form of trusted CheckCast when target type is non trivial compile time constant specified by class object
    * (i.e. CompileTimeComputable._ intrinsic).
    * Its usage is optional and we may ignore it if target type is failed to transform into explicit ClassObject().
    */
  class CheckCastTrustedDelayed private (proto: CheckCastTrustedDelayed.Proto) extends NodeWithFixedArgs(proto)
    with SpinalNode with HasInMemory with CompositeNode with NotProducesValue {

    def obj = arg(2)
    def targetTypeSpecifier = arg(3)
  }

  object CheckCastTrustedDelayed {
    case class Proto private[CheckCastTrustedDelayed] (keyType: Type)
      extends FixedArgs[CheckCastTrustedDelayed](ControlType, MemoryType, TRefType, keyType)(ControlType)
      with SpinalNodePrototype[CheckCastTrustedDelayed] with ControlTagged[CheckCastTrustedDelayed] {

      def newInstance() = new CheckCastTrustedDelayed(this)
    }

    def proto(keyType: Type) = Prototype.intern(Proto(keyType))

    def apply(obj: Node, targetTypeSpecified: Node) = proto(targetTypeSpecified.tpe)(obj, targetTypeSpecified)
    def unapply(n: CheckCastTrustedDelayed) = Some((n.obj, n.targetTypeSpecifier))
  }

  /** Currently this node is only used in Lowering of InstanceOf and BitcodeDeferred.InstanceOf on CBC platform in order
    * to remove null tests from LoweringJIT implementation. */
  class ControlledInstanceOf private(proto: ControlledInstanceOf.Proto) extends NodeWithFixedArgs(proto) with AbstractTypeCheck
    with FloatingNode with ControlledNode {
    def targetType: SignatureType = proto.targetType
    def obj = arg(1)
  }

  object ControlledInstanceOf {
    case class Proto private[ControlledInstanceOf](targetType: SignatureType) extends FixedArgs[ControlledInstanceOf](ControlType, ValueType(targetType))(IntType) {
      def newInstance() = new ControlledInstanceOf(this)
    }
    def apply(targetType: SignatureType) = Prototype.intern(ControlledInstanceOf.Proto(targetType))
    def unapply(x: ControlledInstanceOf) = Some(x.targetType, x.obj)
  }

  class InstanceOf private (proto: InstanceOf.Proto) extends NodeWithFixedArgs(proto) with AbstractTypeCheck with FloatingNode {
    def targetType: SignatureType = proto.targetType
    def obj = arg(0)
  }

  object InstanceOf {
    case class Proto private[InstanceOf] (targetType: SignatureType) extends FixedArgs[InstanceOf](ValueType.fromSig(targetType))(IntType) {
      def newInstance() = new InstanceOf(this)
    }

    def apply(targetType: SignatureType) = Prototype.intern(Proto(targetType))
    def unapply(x: InstanceOf) = Some(x.targetType, x.obj)
  }

  object AnyInstanceOf {
    def unapply(x: Node): Option[(SignatureType, Node)] = condOpt(x) {
      case InstanceOf(tpe, obj) => (tpe, obj)
      case ControlledInstanceOf(tpe, obj) => (tpe, obj)
      case BitcodeDeferred.InstanceOf(tpe, obj) => (tpe, obj)
    }
  }

  /** Represents `obj.getClass()` call. TODO make floating */
  class GetClass extends NodeWithFixedArgs(GetClass) with SpinalNode with CompositeNode with CanThrow with ProducesValue {
    def obj = arg(2)
  }

  object GetClass extends FixedArgs[GetClass](ControlType, MemoryType, TRefType)(TRefType) with ControlValueTagged[GetClass] {
    def newInstance() = new GetClass
    def unapply(n: GetClass) = Some(n.obj)
  }

  /** Obtains `InstanceDescriptor` of its argument `obj`. */
  class InstanceDescriptorBy private extends FloatingNodeWithFixedArgs(InstanceDescriptorBy) with CompositeNode with ControlledNode {
    def obj: Node = arg(1)
  }

  object InstanceDescriptorBy extends FixedArgs[InstanceDescriptorBy](ControlType, TRefType)(AddrType) {
    def newInstance() = new InstanceDescriptorBy
  }

  /** Stores information required to perform interface call in CBC with ISA-12. */
  class LightInterfCastCBC (val rcvType: symlevel.ClassType) extends CachedLeafNode[LightInterfCastCBC](AddrType) with Constant {
    def cacheKey = rcvType
  }

  object LightInterfCastCBC {
    def apply(rcvType: symlevel.ClassType) = Prototype.intern(new LightInterfCastCBC(rcvType))()
    def unapply(n: LightInterfCastCBC) = Some(n.rcvType)
  }


  /** Node that retrieves thrown exception from execution environment of current thread.
    * This operation should always reside in the beginning of XBlock.
    */
  class Catch extends LeafNode[Catch](TRefType) with BlockParamNode {
    private var _block: XBlock = _
    final override def block: XBlock = _block
  }

  object Catch {
    private def instance(block: XBlock): Catch = {
      val inst = new Catch
      inst._block = block
      inst
    }

    def apply(block: XBlock): Catch = instance(block)()
  }

  /** CBC-specific spinal version of [[Catch]]. */
  class CatchCBC private(proto: CatchCBC.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue

  object CatchCBC {
    case class Proto private[CatchCBC]() extends FixedArgs[CatchCBC](ControlType, MemoryType)(TRefType)
      with ControlMemoryValueTagged[CatchCBC] {
      def newInstance() = new CatchCBC(this)
    }

    def apply(): CatchCBC = Prototype.intern(CatchCBC.Proto())()
  }


  trait GetMemoryOperation extends ControlledNode with HasInMemory with FloatingNode with AnyMemoryAccess

  trait PutMemoryOperation extends SpinalMemoryNode with AnyMemoryAccess {
    protected def inValueArgIdx: Int

    /** Returns original argument value, it may exceed actual storage.
      * This unrefined value is adjusted during storing into memory in case of short integral `valueTpe`.
      */
    def inValue0: Node = arg(inValueArgIdx)
    def inValue0_=(newValue: Node): Unit = updateArg(inValueArgIdx, newValue)
    def inValue0Edge: Edge = inEdge(inValueArgIdx)

    /** Returns actual value stored in memory after this operation.
      */
    final def storedValue(): Node =
      PutMemoryOperation.adjustValue(accessType, inValue0)

    final def isPutValue(e: Edge): Boolean = e.targetArgIndex == inValueArgIdx

    object StoredValue {
      def unapply(n: Node): Boolean = PutMemoryOperation.isAdjustedValue(accessType, inValue0)(n)
    }
  }

  object PutMemoryOperation {
    def adjustValue(accessType: AsmType, value: Node): Node = {
      if (accessType.isShortIntegral) {
        JavaShortIntegralExtend(accessType, value)
      } else {
        value
      }
    }

    def isAdjustedValue(accessType: AsmType, originalValue: Node)(n: Node): Boolean = {
      if (accessType.isShortIntegral) {
        cond(n) {
          case JavaShortIntegralExtend(`accessType`, `originalValue`) => true
        }
      } else {
        n == originalValue
      }
    }

    def unapply(n: PutMemoryOperation): Option[(AsmType, Node)] = Some((n.accessType, n.inValue0))
  }

  trait FieldOperation extends Node with AnyMemoryAccess with CompositeNode {
    def field: symlevel.Field
    override final def accessType: AsmType = field.getSignature.toAsm
  }

  trait GetJavaFieldOperation extends GetMemoryOperation with FieldOperation

  trait PutJavaFieldOperation extends PutMemoryOperation with FieldOperation with NotProducesValue {
    def inValueArgIdx: Int
  }

  object PutJavaFieldOperation {
    object InValueEdge {  // TODO: improve EdgeMatcher
      def unapply(e: Edge): Option[PutJavaFieldOperation] = e.target match {
        case n: PutJavaFieldOperation if e.targetArgIndex == n.inValueArgIdx => Some(n)
        case _ => None
      }
    }
  }

  trait InstanceOperation extends MayHaveImplicitCheck {
    def obj: Node
  }

  trait InstanceFieldOperation extends FieldOperation with InstanceOperation

  object InstanceFieldOperation {
    def declaringClassType(field: Field): Type = field.getDeclaringClass match {
      case _: ObjectRTStruct => TRefType
      case _: RTStruct => AddrType
      case t => ValueType(t)
    }
  }

  trait GetInstanceFieldOperation extends InstanceFieldOperation with FloatingNode with ProducesValue

  trait FieldOperationProto


  /** Node for `getstatic` bytecode instruction: retrieving the value of a static field.
   *  Currently its control input is clinit check.
   *
   *  TODO: refactor it replacing control input with special data input that produces clinit
   *        because sometimes field's class can have no clinits and cannot be linked with clinit.
   */
  class GetStatic private (proto: GetStatic.Proto) extends NodeWithFixedArgs(proto) with GetJavaFieldOperation with ContextDependentNode {
    def field = proto.field

    override def contextKey = if (!ContextTypesMap.loweredTypes) JVMState() else null
    override def requiredKeyType: Approximation = {
      val declClass = field.getDeclaringClass
      if (declClass.isDeferred) {
        new VMStateApprox()
      } else {
        new VMStateApprox() withPreparation declClass withClinit declClass
      }
    }
  }

  object GetStatic {
    case class Proto private[GetStatic] (field: Field)
      extends FixedArgs[GetStatic](ControlType, MemoryType)(ValueType.fromSig(field.getType, instantiateRich = true)) with FieldOperationProto {

      def newInstance() = new GetStatic(this)
    }

    def proto(field: Field) = Prototype.intern(Proto(field))

    def apply(field: Field): Node = proto(field)()
    def unapply(node: GetStatic) = Some((node.field, node.inCtrl, node.inMemory))
  }

  /** Node for `putstatic` bytecode instruction: modifying the static field with a new value.
   *  Currently its control input is clinit check.
   *
   *  TODO: refactor it replacing control input with special data input that produces clinit
   *        because sometimes field's class can have no clinits and cannot be linked with clinit.
   */
  class PutStatic private (proto: PutStatic.Proto)
            extends NodeWithFixedArgs(proto) with PutJavaFieldOperation {
    override def inValueArgIdx = 2
    def field = proto.field
  }

  object PutStatic {
    case class Proto private[PutStatic] (field: Field)
            extends FixedArgs[PutStatic](ControlType, MemoryType, ValueType.fromSig(field.getType, instantiateRich = true))(ControlType)
            with FieldOperationProto with ControlMemoryTagged[PutStatic] {

      assert(!field.isAJFlat)
      def newInstance() = new PutStatic(this)
    }

    def proto(field: Field) = Prototype.intern(Proto(field))

    def apply(field: Field)(obj: Node) = proto(field)(obj)
  }

  /** Represents initialization of cangjie String (which is record).
    * Used to hide implementation details of String record from CBC.
    */
  class InitStringRecord private (proto: InitStringRecord.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with NotProducesValue {
    def obj = arg(InitStringRecord.ValueArg.index)
    def allocType = proto.allocType
    def isStatic = proto.isStatic
    def str = proto.str
  }

  object InitStringRecord {
    case class Proto private[InitStringRecord](allocType: SignatureType, isStatic: Boolean, str: symlevel.ConstString)
      extends FixedArgs[InitStringRecord](ControlType, MemoryType, ValueType.fromSig(allocType))(ControlType)
        with ControlMemoryTagged[InitStringRecord] {

      def newInstance() = new InitStringRecord(this)
    }

    def proto(allocType: SignatureType, isStatic: Boolean, str: symlevel.ConstString) = Prototype.intern(Proto(allocType, isStatic, str))
    def apply(allocType: SignatureType, isStatic: Boolean, str: symlevel.ConstString)(arg: Node) = proto(allocType, isStatic, str)(arg)

    object ValueArg extends EdgeMatcher[InitStringRecord](2)
  }

  /** Node for `getfield` bytecode instruction: retrieving the value of an instance field.
   *  Currently its control input is null check.
   *
   *  TODO: refactor it replacing control input with special data input that produces null check
   *        because sometimes we know that the object is not null (just created object or this object).
   */
  class GetField private (proto: GetField.Proto) extends NodeWithFixedArgs(proto) with GetInstanceFieldOperation with GetJavaFieldOperation with ContextDependentNode {
    def objArgIdx = 2
    def obj = arg(objArgIdx)
    def field = proto.field

    override def contextKey = if (!ContextTypesMap.loweredTypes) obj else null
    override def requiredKeyType = {
      if (field.getDeclaringClass.hasDeferredSuper) {
        null
      } else if (field.getDeclaringClass.isRecord) {
        RecordType(SignatureType.fromSymType(field.getDeclaringClass))
      } else {
        OpenCone(ReferenceType(field.getDeclaringClass), mayBeNull = false)
      }
    }

    override def name = s"$simpleName[$field${if (field.isAJFlat) ", FLAT" else ""}]"
  }

  object GetField {
    case class Proto private[GetField] (field: Field)
        extends FixedArgs[GetField](ControlType, MemoryType, InstanceFieldOperation.declaringClassType(field))(ValueType.fromSig(field.getType, instantiateRich = true))
        with FieldOperationProto {
      assert(!field.getType.isZST)
      assert(!field.getDeclaringClass.isUniversalGeneric)
      def newInstance() = new GetField(this)

      override def equals(that: Any) = that match {
        case that: AnyRef if this eq that => true
        case that: Proto => this.field == that.field ||
          (this.field.getDeclaringClass.isArraySlice && that.field.getDeclaringClass.isArraySlice &&
            this.field.getName == that.field.getName)
        case _ => false
      }

      override def hashCode = if (field.getDeclaringClass.isArraySlice) field.getName.## else field.##

    }

    def proto(field: Field) = Prototype.intern(Proto(field))

    def apply(field: Field)(obj: Node): Node = field match {
      case field: RTStruct#Field if field.isRunTimeConstant =>
        field.getDeclaringClass match {
          case _: ObjectRTStruct | RT.ThinObj => GetConstField(field)(obj)
          case _ => GetConstField.controlIndependent(field)(obj)
        }
      case _ => proto(field)(obj)
    }

    def unapply(n: GetField) = Some(n.field, n.inCtrl, n.inMemory, n.obj)
  }

  /** CBC-specific node that combine sequential memory accesses into single expression. */
  class FieldChainRead private(proto: FieldChainRead.Proto) extends NodeWithFixedArgs(proto)
      with InstanceOperation with ControlledNode with HasInMemory with FloatingNode with ProducesValue {
    def obj = arg(2)
    def fields = proto.fields
    override def name = s"$simpleName${fields.mkString("[", " -> ", "]")}"
  }

  object FieldChainRead {
    case class Proto private[FieldChainRead](fields: Array[assembler.cbc.FieldReference], refClassType: Type, fieldType: ValueType)
      extends FixedArgs[FieldChainRead](ControlType, MemoryType, refClassType)(fieldType)
        with FieldOperationProto {

      def newInstance() = new FieldChainRead(this)
    }

    def proto(fields: List[assembler.cbc.FieldReference], refClassType: Type, fieldType: ValueType) =
      Prototype.intern(Proto(Array.from(fields), refClassType, fieldType))

    def apply(fields: List[assembler.cbc.FieldReference], refClassType: Type, fieldType: ValueType)(obj: Node) =
      proto(fields, refClassType, fieldType)(obj)

  }

  /** Node for run-time constant `getField` operation, where field is guaranteed to be immutable.
    *
    * TODO: this node can be represented by a normal GetField node without memory anti-dependence,
    *       when explicit memory graph is implemented.
    */
  class GetConstField private(proto: GetConstField.Proto) extends NodeWithFixedArgs(proto) with GetInstanceFieldOperation with ContextDependentNode {
    require(field.isRunTimeConstant)

    def objArgIdx = 1
    def obj = arg(objArgIdx)
    def field: RTStruct#Field = proto.field

    override def contextKey = field.getDeclaringClass match {
      case _: ObjectRTStruct | RT.ThinObj if ContextTypesMap.loweredTypes => obj
      case _ => null // TODO: introduce ControlDependent and ControlIndependent GetConstField (second one is not ContextOptimizedControlledValueNode)
    }

    override def requiredKeyType = field.getDeclaringClass match {
      case _: ObjectRTStruct | RT.ThinObj => LoweredRefNonNull
      case _ => null
    }
  }

  object GetConstField {
    case class Proto private[GetConstField](field: RTStruct#Field)
      extends FixedArgs[GetConstField](ControlType, InstanceFieldOperation.declaringClassType(field))(ValueType.fromSig(field.getType))
        with FieldOperationProto {

      def newInstance() = new GetConstField(this)
    }

    private def proto(field: RTStruct#Field) = Prototype.intern(Proto(field))

    def apply(field: RTStruct#Field)(obj: Node): Node = proto(field)(obj)

    private[ObjectOperationNodes] def controlIndependent(field: RTStruct#Field)(obj: Node) =
      proto(field).withExplicitArgs(entryBlock, obj)

    def unapply(n: GetConstField) = Some(n.field, n.obj)
  }


  /** Node for `putfield` bytecode instruction: modifying the instance field with a new value.
   *  Currently its control input is null check.
   *
   *  TODO: refactor it replacing control input with special data input that produces null check
   *        because sometimes we know that the object is not null (just created object or this object).
   */
  class PutField private (proto: PutField.Proto)
          extends NodeWithFixedArgs(proto) with InstanceFieldOperation with PutJavaFieldOperation {
    override def inValueArgIdx = 3
    def obj = arg(2)
    def field = proto.field
  }

  object PutField {
    case class Proto private[PutField] (field: Field)
            extends FixedArgs[PutField](ControlType, MemoryType, InstanceFieldOperation.declaringClassType(field), ValueType.fromSig(field.getType, instantiateRich = true))(ControlType)
            with FieldOperationProto with ControlMemoryTagged[PutField] {
      assert(!field.isAJFlat)
      assert(!field.getType.isZST)
      assert(!field.getDeclaringClass.isUniversalGeneric)
      def newInstance() = new PutField(this)
    }

    def proto(field: Field) = Prototype.intern(Proto(field))

    def apply(field: Field)(obj: Node, value: Node): PutField = field match {
      case _: RTStruct#FlatField => shouldNotCallThis("PutField of FlatField")
      case _ => proto(field)(obj, value)
    }

    def unapply(n: PutField) = Some(n.field, n.inCtrl, n.inMemory, n.obj, n.inValue0)
  }

  /** CBC-specific node that combine sequential memory accesses into single expression. */
  class FieldChainWrite private (proto: FieldChainWrite.Proto) extends NodeWithFixedArgs(proto)
      with InstanceOperation with SpinalMemoryNode with NotProducesValue {
    def obj = arg(2)
    def inValue = arg(3)
    def fields = proto.fields
    override def name = s"$simpleName${fields.mkString("[", " -> ", "]")}"
  }

  object FieldChainWrite {
    case class Proto private[FieldChainWrite] (fields: Array[assembler.cbc.FieldReference], refClassType: Type, fieldType: ValueType)
      extends FixedArgs[FieldChainWrite](ControlType, MemoryType, refClassType, fieldType)(ControlType)
        with FieldOperationProto with ControlMemoryTagged[FieldChainWrite] {
      def newInstance() = new FieldChainWrite(this)
    }

    def proto(fields: List[assembler.cbc.FieldReference], refClassType: Type, fieldType: ValueType) =
      Prototype.intern(Proto(Array.from(fields), refClassType, fieldType))

    def apply(fields: List[assembler.cbc.FieldReference], refClassType: Type, fieldType: ValueType)(obj: Node, value: Node) =
      proto(fields, refClassType, fieldType)(obj, value)

    def unapply(n: FieldChainWrite) = Some(n.obj, n.inValue, n.fields)
  }

  /////////////////////////////////////////
  // Arrays

  sealed abstract class ArrayLength protected (proto: ArrayLength.Proto[_ <: ArrayLength]) extends FloatingNodeWithFixedArgs(proto) with ContextDependentNode with CompositeNode with MayHaveImplicitCheck {
    def array = arg(1)

    // TODO: We need TypeOpenCone(AnyJavaArray, mayBeNull = false) but such approximation cannot be created in current implementation.
    //       We might use TypeOpenCone(j.l.Object, mayBeNull = false) but it can lead to reading "length" of non-array values,
    //       this does not look dangerous but we decided to be cautious.
    override final def contextKey = if (!ContextTypesMap.loweredTypes) array else null
    override final def requiredKeyType = nodeType(array).withoutNull
  }

  object ArrayLength {
    abstract class Proto[N <: ArrayLength](retType: Type) extends FixedArgs[N](ControlType, TRefType)(retType)
    def apply(arrayType: SignatureType): Proto[_] = {
      if (arrayType.isXScalaType) {
        require(Environment.LANGUAGE_PACK == SCALA)
        ScalaArrayLength
      } else if (arrayType.symType.isJavaArray) {
        JavaArrayLength
      } else if (arrayType.isCangjieArray) {
        CangjieArrayLength
      } else {
        AJArrayLength
      }
    }

    def unapply(n: ArrayLength) = Some(n.array)
  }

  /** Node for `arraylength` bytecode instruction: retrieving the length of an array. */
  class JavaArrayLength private () extends ArrayLength(JavaArrayLength)

  object JavaArrayLength extends ArrayLength.Proto[JavaArrayLength](IntType) {
    def newInstance() = new JavaArrayLength
  }

  class ScalaArrayLength private () extends ArrayLength(ScalaArrayLength)

  object ScalaArrayLength extends ArrayLength.Proto[ScalaArrayLength](IntType) {
    def newInstance() = new ScalaArrayLength
  }

  class AJArrayLength private () extends ArrayLength(AJArrayLength)

  object AJArrayLength extends ArrayLength.Proto[AJArrayLength](AddrIntType) {
    def newInstance() = new AJArrayLength
  }

  class CangjieArrayLength private () extends ArrayLength(CangjieArrayLength)

  object CangjieArrayLength extends ArrayLength.Proto[CangjieArrayLength](AddrIntType) {
    def newInstance() = new CangjieArrayLength
  }

  trait TypedArrayOperation extends Node {
    require(arrayType.isArray, s"$arrayType")

    /** Formal type of this array operation.
      * Might be erased type if this operation comes from Java bytecode.
      */
    def arrayType: SignatureType
  }

  object TypedArrayOperation {
    def idxType(arrayType: SignatureType) = if (arrayType.isAJArray || arrayType.isCangjieArray) AddrIntType else IntType
    def lenType(arrayType: SignatureType) = idxType(arrayType)

    def enrichedElemType(arrayType: SignatureType): SignatureType = {
      val elemType = arrayType.getArrayElemType
      // Only Cangjie arrays can store enriched values.
      if (elemType.isCangjieType || !elemType.isInterface) {
        elemType
      } else {
        // Erase interfaces in other languages to the root of corresponding hierarchy.
        val symType = if (elemType.isJavaReference) {
          typeProvider.getObjectType
        } else if (elemType.isXScalaType) {
          typeProvider.getXScalaAnyRef
        } else {
          assert(elemType.isAJManagedType)
          typeProvider.getAJObjectType
        }

        SignatureType.fromSymType(symType)
      }
    }
  }

  class ArrayIndexCheck private (proto: ArrayIndexCheck.Proto) extends PureCheck(proto) with TypedArrayOperation
    with ThrowingPureCheck with CompositeNode with TypeFilterNode with ValueRangeFilter with NotProducesValue {
    def array = arg(ArrayIndexCheck.ArrayEdge.index)
    def idx = arg(3)
    def length = arg(4)

    def arrayType: SignatureType = proto.arrayType

    def filteredArg = array

    def filterType(tpe: Approximation, point: ControlNode) = {
      (tpe, false) // TODO: array types
    }

    def filteredValue = idx
    def filteredValueRange = {
      val tpe = filteredValue.tpe
      length match {
        case IntegralConst(len) => if (len == 0) EmptyValueRange(tpe) else ConstValueRange(tpe, 0, len - 1, filteredValueCtrl)
        case len => HalfSymbolicValueRange(tpe, 0, len, -1, filteredValueCtrl)
      }
    }

    def filteredValueCtrl = outCtrl

    override def throwInfo = (ArrayIndexCheck.errorProc(arrayType.symType, inlineContext), Seq())

    override def idempotentValueArgs = Iterator(array, idx, length)
  }

  object ArrayIndexCheck {
    import TypedArrayOperation._
    case class Proto private[ArrayIndexCheck] (arrayType: SignatureType, trusted: Boolean)
      extends PureCheckPrototype[ArrayIndexCheck](ControlType, MemoryType, TRefType, idxType(arrayType), lenType(arrayType))(ControlType)(arrayType)
        with ControlTagged[ArrayIndexCheck] {

      def newInstance() = new ArrayIndexCheck(this)
    }

    def errorProc(arrayType: SymType, inlineContext: InlineContext): RTSProc = if (arrayType.isAJArray) {
      RTSProc.JR_ThrowAJArrayIndexOutOfBoundsException
    } else if (arrayType.isCangjieArray) {
      RTSProc.JR_ThrowCJIndexOutOfBoundsException
    } else if (arrayType.isXScalaArray) {
      RTSProc.JR_ThrowScalaArrayIndexOutOfBoundsException
    } else {
      assert(arrayType.isJavaArray)
      RTSProc.JR_ThrowArrayIndexOutOfBoundsException
    }

    def apply(arrayType: SignatureType, args: Node*): ArrayIndexCheck = apply(arrayType, trusted = false)(args: _*)
    def apply(arrayType: SignatureType, trusted: Boolean): Proto = Prototype.intern(Proto(arrayType, trusted))

    object ArrayEdge extends EdgeMatcher[ArrayIndexCheck](2)
  }


  class ArrayStoreCheck private (proto: ArrayStoreCheck.Proto) extends PureCheck(proto) with TypedArrayOperation
    with ThrowingPureCheck with CompositeNode with TypeFilterNode with NotProducesValue {

    /** Array type approximation (actual array type is greater or equal to this type).
      * This field is used only if value is assign compatible with this type. */
    var arrayTypeForFastPath: ReferenceApprox = _
    def hasFastPathInfo: Boolean = arrayTypeForFastPath != null

    /** For interface values we should know its relaxed type to generate enrichment check. */
    var valueRelaxedType: ReferenceType = _
    def valueHasRelaxedType: Boolean = {
      assert(hasFastPathInfo)
      valueRelaxedType != null
    }

    def array = arg(2)
    def value = arg(3)

    def arrayType: SignatureType = proto.arrayType

    def filteredArg = array

    def filterType(tpe: Approximation, point: ControlNode) = {
      (tpe, false) // TODO: array types
    }

    override def throwInfo = {
      val proc = if (arrayType.isXScalaArray) {
        RTSProc.JR_ThrowScalaArrayStoreException
      } else {
        RTSProc.JR_ThrowArrayStoreException
      }
      (proc, Seq())
    }

    def checkArrayStore =
      if arrayType.isXScalaArray then RTSProc.JR_ScalaCheckArrayStore else RTSProc.JR_CheckArrayStore

    def checkArrayStoreNotNull =
      if arrayType.isXScalaArray then RTSProc.JR_ScalaCheckArrayStoreNotNull else RTSProc.JR_CheckArrayStoreNotNull

    def checkArrayStoreOpt =
      if arrayType.isXScalaArray then RTSProc.JR_ScalaCheckArrayStoreOpt else RTSProc.JR_CheckArrayStoreOpt

  }

  object ArrayStoreCheck {
    case class Proto private[ArrayStoreCheck] (arrayType: SignatureType, trusted: Boolean)
      extends PureCheckPrototype[ArrayStoreCheck](ControlType, MemoryType, TRefType, TRefType)(ControlType)(arrayType.symType)
        with ControlTagged[ArrayStoreCheck] {

      def newInstance() = new ArrayStoreCheck(this)
    }

    def apply(arrayType: SignatureType, args: Node*): ArrayStoreCheck = apply(arrayType, trusted = false)(args: _*)
    def apply(arrayType: SignatureType, trusted: Boolean): Proto = Prototype.intern(Proto(arrayType, trusted))
  }


  trait ArrayElementOperation extends AnyMemoryAccess with CompositeNode with MayHaveImplicitCheck {
    def arrayArgIdx = 2
    def array = arg(arrayArgIdx)
    def idx = arg(3)
  }

  trait ArrayGetOperation extends ArrayElementOperation with GetMemoryOperation with ProducesValue

  trait ArrayPutOperation extends ArrayElementOperation with PutMemoryOperation with NotProducesValue {
    override def inValueArgIdx = 4
  }

  object ArrayPutOperation {
    object InValueEdge {  // TODO: improve EdgeMatcher
      def unapply(e: Edge): Option[ArrayPutOperation] = e.target match {
        case n: ArrayPutOperation if e.targetArgIndex == n.inValueArgIdx => Some(n)
        case _ => None
      }
    }
  }

  class ArrayGet private (proto: ArrayGet.Proto) extends NodeWithFixedArgs(proto) with ArrayGetOperation with TypedArrayOperation with ArgDependentTypeNode with ContextDependentNode {
    def isTypeDependency(edge: Edge): Boolean = {
      edge.targetArgIndex == arrayArgIdx
    }

    def arrayType: SignatureType = proto.arrayType
    override final def accessType: AsmType = arrayType.getArrayElemType.toAsm

    // TODO: improve context types to fully support these operations,
    //       now we just use last check without type calculation
    override def contextKey = if (!ContextTypesMap.loweredTypes) array else null
    override def requiredKeyType = null // TODO: why not nodeType(array) like in ArrayLength?

    def enrichedElemType = proto.enrichedElemType
  }

  object ArrayGet {
    import TypedArrayOperation._
    case class Proto private[ArrayGet] (arrayType: SignatureType, enrichedElemType: SignatureType)
        extends FixedArgs[ArrayGet](ControlType, MemoryType, TRefType, idxType(arrayType))(retType(enrichedElemType)) {

      // TODO: remove this assert with proper IR type check when Eop hierarchy is reworked.
      assert(!enrichedElemType.isInterface || enrichedElemType.symType.isCangjieType,
        s"unexpected rich array element ($enrichedElemType): only Cangjie arrays can store enriched values")

      def newInstance() = new ArrayGet(this)
    }

    private def retType(enrichedElemType: SignatureType) = {
      ValueType(enrichedElemType, eopTypeForInterfaces = enrichedElemType.isCangjieType, instantiateRich = true)
    }

    def apply(arrayType: SignatureType): Proto = apply(arrayType, enrichedElemType(arrayType))
    def apply(arrayType: SignatureType, enrichedElemType: SignatureType): Proto = Prototype.intern(Proto(arrayType, enrichedElemType))
    def unapply(node: ArrayGet) = Some((node.inCtrl, node.inMemory, node.array, node.idx))
  }

  /* CBC-specific node that starts sequential memory accesses from record array. */
  class RecordArrayGet private(proto: RecordArrayGet.Proto) extends FloatingNodeWithFixedArgs(proto) with ProducesValue {
    def arrayArgIdx = 0
    def array = arg(arrayArgIdx)

    def idxArgIdx = 1
    def idx = arg(idxArgIdx)

    def arrayType = proto.arrayType
  }

  object RecordArrayGet {
    case class Proto private[RecordArrayGet](arrayType: SignatureType) extends FixedArgs[RecordArrayGet](TRefType, AddrIntType)(ValueType(arrayType.getArrayElemType)) {
      def newInstance() = new RecordArrayGet(this)
    }

    def proto(arrayType: SignatureType) = Prototype.intern(Proto(arrayType))
    def apply(arrayType: SignatureType)(array: Node, idx: Node): Node = proto(arrayType)(array, idx)
    def unapply(n: RecordArrayGet) = Some(n.array, n.idx)
  }


  class ArrayPut private (proto: ArrayPut.Proto) extends NodeWithFixedArgs(proto) with ArrayPutOperation
    with TypedArrayOperation {

    def arrayType: SignatureType = proto.arrayType
    override final def accessType: AsmType = arrayType.getArrayElemType.toAsm

    def enrichedElemType = proto.enrichedElemType
  }

  object ArrayPut {
    import TypedArrayOperation._
    case class Proto private[ArrayPut] (arrayType: SignatureType, enrichedElemType: SignatureType)
            extends FixedArgs[ArrayPut](ControlType, MemoryType, TRefType, idxType(arrayType), ValueType(enrichedElemType, eopTypeForInterfaces = true, instantiateRich = true))(VoidType)
            with ControlMemoryTagged[ArrayPut] {

      // TODO: remove this assert with proper IR type check when Eop hierarchy is reworked.
      assert(!enrichedElemType.isInterface || enrichedElemType.isCangjieType,
        s"unexpected rich array element ($enrichedElemType): only Cangjie arrays can store enriched values")

      def newInstance() = new ArrayPut(this)
    }

    def apply(arrayType: SignatureType): Proto = apply(arrayType, enrichedElemType(arrayType))
    def apply(arrayType: SignatureType, enrichedElemType: SignatureType): Proto = Prototype.intern(Proto(arrayType, enrichedElemType))
    def unapply(n: ArrayPut) = Some(n.array, n.idx, n.inValue0)
  }

  class ArrayFill private (proto: ArrayFill.Proto) extends NodeWithFixedArgs(proto) with TypedArrayOperation with SpinalMemoryNode with NotProducesValue {
    import ArrayFill.adjustValue

    override def name: String =
      simpleName + (if (size < 10) storedValues.mkString("[", ",", "]") else s"[$size values]")

    def arrayType: SignatureType = proto.arrayType
    def elemType: AsmType = arrayType.getArrayElemType.toAsm

    /** Returns original argument values, it may exceed actual storage.
      * These unrefined values are adjusted during storing into memory in case of short integrals.
      */
    def inValues0: Seq[Long] = proto.inValues0

    lazy val storedValues: Seq[Long] = inValues0.map(adjustValue(elemType, _))

    def size = inValues0.size
    def totalBytes = size * elemType.sizeInBytes

    def arrayArgIdx = 2
    def array = arg(arrayArgIdx)
  }

  object ArrayFill {
    case class Proto private[ArrayFill] (arrayType: SignatureType, inValues0: Seq[Long])
      extends FixedArgs[ArrayFill](ControlType, MemoryType, TRefType)(ControlType)
      with ControlMemoryTagged[ArrayFill] {

      assert(arrayType.getArrayElemType.symKindErased.isIntegral)
      def newInstance() = new ArrayFill(this)
    }

    def apply(arrayType: SignatureType, values: Seq[Long]) = {
      // Note: do not intern ArrayFill prototypes because they are rarely repeated,
      // but could be created with sequentially increasing values collection causing OOM,
      // because cached prototypes remain forever.
      Proto(arrayType, values)
    }

    def adjustValue(accessType: AsmType, value: Long): Long = {
      import AsmType.*
      (accessType: @unchecked) match {
        case I8  | U8  => value.toByte
        case I16 | F16 => value.toShort
        case U16       => value.toChar
        case I32 | U32 => value.toInt
        case I64 | U64 => value
      }
    }
  }


  // TODO: rename to CangjieArrayFill or formalize abstraction to both Cangjie and AJ arrays (JET-17408)
  class AJArrayFill private(proto: AJArrayFill.Proto) extends NodeWithFixedArgs(proto) with TypedArrayOperation with SpinalMemoryNode
    with CompositeNode with NotProducesValue {

    def arrayType: SignatureType = proto.arrayType
    def elemType: AsmType = arrayType.getArrayElemType.toAsm

    def array = arg(2)
    def value = arg(valueArgIdx)

    private def valueArgIdx = 3
    def isFillValue(e: Edge) = e.targetArgIndex == valueArgIdx

    def enrichedElemType = proto.enrichedElemType
  }

  object AJArrayFill {
    case class Proto private[AJArrayFill](arrayType: SignatureType, enrichedElemType: SignatureType)
      extends FixedArgs[AJArrayFill](ControlType, MemoryType, TRefType, ValueType(enrichedElemType, eopTypeForInterfaces = true, instantiateRich = true))(ControlType)
        with ControlMemoryTagged[AJArrayFill] {

      assert(arrayType.isArray)

      // TODO: remove this assert with proper IR type check when Eop hierarchy is reworked.
      assert(!enrichedElemType.isInterface || enrichedElemType.isCangjieType,
        s"unexpected rich array element ($enrichedElemType): only Cangjie arrays can store enriched values")

      def newInstance() = new AJArrayFill(this)
    }

    def apply(arrayType: SignatureType, enrichedElemType: SignatureType): Proto = Prototype.intern(Proto(arrayType, enrichedElemType))
  }


  /////////////////////////////////////////
  // Strings

  class StrConcat protected (proto: StrConcat.Proto)
    extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CanThrow with CompositeNode with ProducesValue {

    def argTypes = proto.argTypes
    def isAJ = proto.isAJ

    def concatenatedArgs = argsTail(2)

    def formatString = (argTypes map fmtChar).mkString

    private def fmtChar(tpe: SymType): Char = {
      import TypeKind._
      tpe.getKind match {
        case BOOLEAN => 'b'
        case CHAR => 'c'
        case BYTE | SHORT | INT => 'i'
        case LONG => 'l'
        case FLOAT => 'f'
        case DOUBLE => 'd'
        case CLASS => 's'
        case x => shouldNotReachHere(x)
      }
    }

    // Is used during lowering.
    var cold: Boolean = false
  }

  object StrConcat {
    case class Proto private[StrConcat] (argTypes: Seq[SymType], isAJ: Boolean)
      extends FixedArgs[StrConcat](Seq(ControlType, MemoryType) ++ tpes(argTypes) :_*)(TRefType)
      with ControlMemoryValueTagged[StrConcat] {

      def newInstance() = new StrConcat(this)
    }

    private def tpes(argTypes: Seq[SymType]) = argTypes map { t => ValueType(t) }

    def apply(argTypes: Seq[SymType], isAJ: Boolean) = Prototype.intern(Proto(argTypes, isAJ))
  }

  /////////////////////////////////////////
  // Invokes
  // TODO: a lot of removable(?) copy-paste in this lands.

  object AbstractCall {
    /** Types corresponding to the parameters of given method. */
    def argTypes(methodRef: MethodReference): Seq[Type] = {
      val methodType = methodRef.methodType
      methodType.parameterTypes.zipWithIndex.map { case (t, idx) =>
        ValueType.fromSig(t, instantiateRich = !methodType.isReceiverParameter(idx))
      }.toSeq
    }

    def retType(methodRef: MethodReference): Type = {
      val returnType = methodRef.methodType.returnType
      ValueType.fromSig(returnType, instantiateRich = true)
    }

    abstract class Proto[N <: AbstractCall](_targetRef: MethodReference, targetType: Option[Type])
      extends FixedArgs[N](Seq(ControlType, MemoryType) ++ targetType ++ argTypes(_targetRef) : _*)(retType(_targetRef))
        with ControlMemoryValueTagged[N]
  }

  trait AbstractCall extends SpinalMemoryNode with CanThrow with ProducesValue {
    def targetRef: MethodReference
    def methodType = targetRef.methodType

    private def isNonThrowing = targetRef.hasMethod && targetRef.method.isNonThrowing

    // In cangjie foreign ccall method can throw exceptions asynchronously through EE.pendingException.
    private def canThrowAsync = targetRef.methodType.isCJForeign

    override def canThrow =
      (methodType.callConv.isManaged && !isNonThrowing) || canThrowAsync

    override def hasXSite = {
      methodType.callConv.hasManagedExecEnv ||
        // CBC instruction calling foreign method may throw exception, check of pending exception is built into the instruction.
        targetArch == CBC && canThrowAsync
    }

    protected def paramsOffsetInArgs: Int

    final def isParamEdge(inEdge: Edge): Boolean = {
      assert(inEdge.target eq this)
      inEdge.targetArgIndex >= paramsOffsetInArgs
    }

    final def invokeArgs: Seq[Node] = argsTail(paramsOffsetInArgs)

    final def invokeArgIdx(argEdge: Edge) = {
      assert(argEdge.target eq this)
      argEdge.targetArgIndex - paramsOffsetInArgs
    }

    final def updateInvokeArg(idx: Int, node: Node): Unit = {
      updateArg(idx + paramsOffsetInArgs, node)
    }

    private[ir] final override def argEnrichment(argEdge: Edge) = {
      val argIdx = invokeArgIdx(argEdge)
      if (argIdx >= 0) methodParamEnrichment(methodType, argIdx).toOption else None
    }
  }

  /** Match any direct non-deferred call (includes invokestatic, invokespecial, RT procs). */
  object AnyDirectCall {
    import MethodReferenceAccessKind._

    def unapply(x: AbstractCall): Option[Method] = condOpt(x) {
      case CallMethod(method, STATIC | SPECIAL | MUT, _) => method
      case ec: ErrorRTSCall => ec.targetRef.method
    }
  }

  /** Match any virtual call (includes invokevirtual, invokeinterface). */
  object AnyVirtualCall {
    import MethodReferenceAccessKind._

    def unapply(x: Call): Boolean = {
      x.targetRef.hasMethod && !x.targetRef.method.getDeclaringClass.isDeferred &&
        (x.akind == VIRTUAL || x.akind == INTERFACE)
    }
  }

  object VirtualStaticCall {
    def unapply(x: Call): Boolean = {
      x.targetRef.hasMethod && x.targetRef.method.hasThisTypeInfoParameter && x.akind == STATIC_VIRTUAL
    }
  }

  /** Low-level call operation */
  final class Call(proto: Call.Proto)
    extends NodeWithFixedArgs(proto) with AbstractCall with ArgDependentTypeNode with MayHaveImplicitCheck {
    // N.B.: Any attached implicit check will be reattached to PreCall node during post processing

    import Call._

    override val paramsOffsetInArgs = Call.paramsOffsetInArgs
    def target = arg(Call.Target.index)

    override def targetRef = proto.methodRef
    def akind = targetRef.accessKind

    /** Receiver in case of instance method invocation. */
    def receiver = {
      require(methodType.hasReceiverParameter)
      invokeArgs(methodType.getReceiverArgIdx)
    }

    override def name = {
      val akindInfo = Some(akind.toString)

      val refClassInfo =
        Option.when(targetRef.hasRefClass && (!targetRef.hasMethod || targetRef.method.getDeclaringClass != targetRef.refClass)) {
          s"refClass: ${targetRef.refClass.getName}"
        }

      val methodInfo = Option.when(targetRef.hasMethod) { targetRef.method.getFullName }

      val methodTypeInfo = Option.when(methodInfo.isEmpty) {
        val mt = targetRef.methodType
        Seq(mt.toMethodDescriptor.toJETSignature, mt.callConv, mt.callKind).mkString(", ")
      }

      val info = Seq(akindInfo, refClassInfo, methodInfo, methodTypeInfo).flatten.mkString(", ")

      s"Call[$info]"
    }

    lazy val abi = platform.abi(methodType)

    lazy val gcActions = methodType.callKind match {
      case AJ_LONG_SAFE => CallGCActions(inlineContext.method.hasManagedExecEnv, RT.ExecEnv.safeSectionEntranceFrameAddr,
        checkGCSafeState = env.enabled(GCSafetyChecks))

      case CJ_FOREIGN => CallGCActions(inlineContext.method.hasManagedExecEnv, RT.ExecEnv.nativeWrapperFrameAddr, checkGCSafeState = false)

      case NORMAL => emptyGCActions
    }

    def isTypeDependency(edge: Edge): Boolean = this match {
      case AnyVirtualCall() => edge.targetArgIndex == paramsOffsetInArgs + methodType.getReceiverArgIdx // receiver object
      case _ => false
    }
  }

  object Call {
    case class Proto private[Call](methodRef: MethodReference)
      extends AbstractCall.Proto[Call](methodRef, /*CallTarget*/Some(AddrType)) {

      def newInstance() = new Call(this)
    }

    val paramsOffsetInArgs = 3

    def proto(methodRef: MethodReference) = Prototype.intern(Call.Proto(methodRef))

    def apply(methodRef: MethodReference)(args: Node*): Call = proto(methodRef)(args: _*)

    case class CallGCActions(generateGCSafeRegion: Boolean, savedFrameAddrField: Field, checkGCSafeState: Boolean)

    private val emptyGCActions = CallGCActions(generateGCSafeRegion = false, null, checkGCSafeState = false)

    object Target extends EdgeMatcher[Call](2)
  }

  /** RTSCall of some error runtime procedure. Do not return control (but may throw exception),
    * so Halt block end may be inserted after this node. */
  class ErrorRTSCall private(proto: ErrorRTSCall.Proto) extends NodeWithFixedArgs(proto) with AbstractCall with CompositeNode with ColdNode {
    require(targetRef.hasMethod)

    override val paramsOffsetInArgs: Int = 2
    def targetRef = proto.targetRef
    def proc = proto.proc
  }

  object ErrorRTSCall {
    case class Proto private[ErrorRTSCall] (proc: RTSProc, targetRef: MethodReference)
      extends AbstractCall.Proto[ErrorRTSCall](targetRef, None) {

      def newInstance() = new ErrorRTSCall(this)
    }

    // for serialization
    def proto(proc: RTSProc, targetRef: MethodReference) = Prototype.intern(Proto(proc, targetRef))

    def apply(proc: RTSProc)(args: Node*) = proto(proc, new MethodReference(env.getRTSProc(proc), MethodReferenceAccessKind.STATIC))(args: _*)
    def unapply(n: ErrorRTSCall): Option[RTSProc] = Some(n.proc)
  }

  object DebugPrintf {
    def apply(format: String, args: Node*): SpinalNode = apply(Nil, format, args)
    def apply(ctrl: Node, mem: Node, format: String, args: Node*): SpinalNode = apply(Seq(ctrl, mem), format, args)

    private def apply(preArgs: Seq[Node], format: String, args: Seq[Node]): SpinalNode = {
      // RT-exports don't support varargs so here we forced to use search by name instead.
      val ref = RT.DebugPrint.safePrintf
      val varArgTypes: Seq[SignatureType] = args map (_.tpe) map { case tpe: TypeWithKind => SignatureType.Primitive(tpe.kind) }
      val realRef = ref.withMethodType(ref.realMethodType.appendVarArgs(varArgTypes))
      DirectCall(realRef)(preArgs ++ Seq(AJString.bstr(ascii(format))) ++ args: _*)
    }
  }

  abstract class CallTarget(proto: CallTarget.Proto[_ <: CallTarget]) extends FloatingNodeWithFixedArgs(proto)

  object CallTarget {
    abstract class Proto[N <: CallTarget](argTypes: Type*) extends FixedArgs[N](argTypes: _*)(AddrType)
  }

  abstract class AnyInvokeTarget protected(proto: AnyInvokeTarget.Proto[_ <: AnyInvokeTarget])
    extends CallTarget(proto) with ControlledNode with HasInMemory {
    require(targetRef.hasMethod)

    final def targetRef: MethodReference = proto.targetRef

    def akind = targetRef.accessKind
    override def name = s"Invoke${akind.toString.toLowerCase.asciiCapitalize}Target[${targetRef.method.getFullName}]"

    /** Receiver in case of instance method invocation. */
    def receiver = {
      require(targetRef.hasNonRecordReceiverParameter)
      arg(AnyInvokeTarget.ReceiverEdge.index)
    }
  }

  object AnyInvokeTarget {
    abstract class Proto[N <: AnyInvokeTarget](rcvType: Option[Type], ciao: Option[Type])
      extends CallTarget.Proto[N](Seq(ControlType, MemoryType) ++ rcvType ++ ciao: _*) {
      if (ciao.isDefined) require(rcvType.isDefined)

      def targetRef: MethodReference
    }

    def receiverType(targetRef: MethodReference): Type = {
      require(targetRef.hasNonRecordReceiverParameter)
      // Note: probably should use methodType.getParameterType(methodType.getReceiverArgIdx),
      //       but unit-tests do not properly set method type, so use refClass as a substitute.
      ValueType(targetRef.refClass)
    }

    object ReceiverEdge extends EdgeMatcher[AnyInvokeTarget](2)
  }

  class InvokeInterfaceTarget private(proto: InvokeInterfaceTarget.Proto) extends AnyInvokeTarget(proto) {
    private def ciaoArg = InvokeInterfaceTarget.CIAOEdge.index
    def ciao = arg(ciaoArg)
    def ciao_=(n: Node): Unit = { updateArg(ciaoArg, n) }
  }

  object InvokeInterfaceTarget {
    case class Proto private[InvokeInterfaceTarget](targetRef: MethodReference)
      extends AnyInvokeTarget.Proto[InvokeInterfaceTarget](Some(AnyInvokeTarget.receiverType(targetRef)), /*ciao*/ Some(AddrType)) {

      def newInstance() = new InvokeInterfaceTarget(this)
    }

    def proto(targetRef: MethodReference) = Prototype.intern(Proto(targetRef))

    def apply(targetRef: MethodReference)(receiver: Node, ciao: Node) = proto(targetRef)(receiver, ciao)
    def unapply(target: InvokeInterfaceTarget): Option[Node] = Some(target.ciao)

    object CIAOEdge extends EdgeMatcher[InvokeInterfaceTarget](3)
  }

  class InvokeTarget private(proto: InvokeTarget.Proto) extends AnyInvokeTarget(proto)

  object InvokeTarget {
    case class Proto private[InvokeTarget](targetRef: MethodReference)
      extends AnyInvokeTarget.Proto[InvokeTarget](
        Option.when(targetRef.hasNonRecordReceiverParameter)(AnyInvokeTarget.receiverType(targetRef)),
        None) {

      def newInstance() = new InvokeTarget(this)
    }

    def proto(targetRef: MethodReference) = Prototype.intern(Proto(targetRef))

    def static(targetRef: MethodReference) = proto(targetRef)()
    def instance(targetRef: MethodReference)(receiver: Node) = proto(targetRef)(receiver)
  }

  class InvokeVirtualStaticTarget private(proto: InvokeVirtualStaticTarget.Proto) extends AnyInvokeTarget(proto) {
    def thisTypeInfo = arg(InvokeVirtualStaticTarget.ThisTypeInfo.index)
  }

  object InvokeVirtualStaticTarget {
    case class Proto private[InvokeVirtualStaticTarget](targetRef: MethodReference)
      extends AnyInvokeTarget.Proto[InvokeVirtualStaticTarget](/*ThisTypeInfo*/ Some(AddrType), None) {

      def newInstance() = new InvokeVirtualStaticTarget(this)
    }

    def proto(targetRef: MethodReference) = Prototype.intern(Proto(targetRef))
    def apply(targetRef: MethodReference)(thisTypeInfo: Node) = proto(targetRef)(thisTypeInfo)

    object ThisTypeInfo extends EdgeMatcher[InvokeVirtualStaticTarget](2)
  }

  class DAICallTarget private(proto: DAICallTarget.Proto) extends CallTarget(proto) {
    assert(currentPhase >= CompilerPhase.Lowering)
    def targetSymbol = proto.target.symbol
  }

  object DAICallTarget {
    case class Proto private[DAICallTarget](target: DAITarget) extends CallTarget.Proto[DAICallTarget]() {

      def newInstance() = new DAICallTarget(this)
    }

    def apply(target: DAITarget) = Prototype.intern(Proto(target))
  }

  object DirectCall {
    def apply(target: Method)(args: Node*): Call = {
      apply(new MethodReference(target, MethodReferenceAccessKind.STATIC))(args :_*)
    }

    def apply(targetRef: MethodReference)(args: Node*): Call = {
      assert(targetRef.hasMethod)
      val callTarget = SymbolAddress(targetRef.method)
      Call(targetRef)(callTarget +: args :_*)
    }

    def unapply(call: Call) = call.target match {
      case SymbolAddress(method: Method) => Some(method)
      case t: InvokeTarget if t.targetRef.hasMethod => Some(t.targetRef.method)
      case _ => None
    }
  }

  object RTSCall {
    def apply(proc: RTSProc)(args: Node*) = {
      DirectCall(env.getRTSProc(proc))(args :_*)
    }
  }

  object DAICall {
    def apply(targetRef: MethodReference, target: DAITarget)(args: Node*) = {
      val callTarget = DAICallTarget(target)()
      Call(targetRef)(callTarget +: args :_*)
    }

    def unapply(call: Call) = call.target match {
      case dai: DAICallTarget => Some(dai.targetSymbol)
      case _ => None
    }
  }

  object InvokeInterface {
    def apply(targetRef: MethodReference, ciao: Node)(args: Node*) = {
      val callTarget = InvokeInterfaceTarget(targetRef)(args(targetRef.getReceiverArgIndex), ciao)
      Call(targetRef)(callTarget +: args :_*)
    }
  }

  object Invoke {
    def apply(targetRef: MethodReference)(args: Node*) = {
      val callTarget = if (targetRef.hasNonRecordReceiverParameter) {
        InvokeTarget.instance(targetRef)(args(targetRef.getReceiverArgIndex))
      } else {
        InvokeTarget.static(targetRef)
      }
      Call(targetRef)(callTarget +: args :_*)
    }
  }

  object InvokeVirtualStatic {
    def apply(targetRef: MethodReference)(args: Node*) = {
      require(targetRef.hasMethod && targetRef.method.hasThisTypeInfoParameter)
      val tti = args(targetRef.method.getThisTypeInfoArgIdx)
      require(cond(tti) { case _: (Param | ThisTypeInfoBy | ThisTypeInfoByCBC) => true })
      val callTarget = InvokeVirtualStaticTarget(targetRef)(tti)
      Call(targetRef)(callTarget +: args: _*)
    }
  }

  object AnyInvoke {
    def unapply(call: Call) = cond(call.target) {
      case _: AnyInvokeTarget => true
    }
  }

  object CallMethod {
    def unapply(n: Call) =
      if (n.targetRef.hasMethod) Some(n.targetRef.method, n.akind, n.invokeArgs) else None
  }

  /////////////////////////////////////////
  // Memory barriers

  /** Node for memory barriers. Each memory barrier node can contain several barrier properties.
   * It consumes and produces both control and memory.
   */
  class MemBarrier private (proto: MemBarrier.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with NotProducesValue {
    assert(kinds.nonEmpty)
    def kinds = proto.kinds
  }

  object MemBarrier {
    case class Proto private[MemBarrier] (kinds: Set[BarrierKind])
      extends FixedArgs[MemBarrier](ControlType, MemoryType)(ControlType) with ControlMemoryTagged[MemBarrier] {

      def newInstance() = new MemBarrier(this)
    }

    def apply(kinds: Set[BarrierKind]) = Prototype.intern(Proto(kinds))

    def unapply(n: MemBarrier) = Some(n.kinds)
  }

  class LocalReachabilityShield extends NodeWithFixedArgs(LocalReachabilityShield) with SpinalMemoryNode with NotProducesValue {
    def obj = arg(2)
  }

  object LocalReachabilityShield extends FixedArgs[LocalReachabilityShield](ControlType, MemoryType, TRefType)(ControlType)
    with ControlMemoryTagged[LocalReachabilityShield] {

    def newInstance() = new LocalReachabilityShield
  }

  // it would be cool to really check that barrier.cell
  //  1. stored with IntelWidth.BYTE before barrier
  //  2. loaded with IntelWidth.DWORD after barrier
  // See JET-9664.
  class StoreLoadForCell private extends NodeWithFixedArgs(StoreLoadForCell) with SpinalMemoryNode with NotProducesValue {
    def cell = arg(2)
  }

  object StoreLoadForCell extends FixedArgs[StoreLoadForCell](ControlType, MemoryType, AddrType)(ControlType)
    with ControlMemoryTagged[StoreLoadForCell] {

    def newInstance() = new StoreLoadForCell
  }

  /////////////////////////////////////////
  // Write barriers

  sealed abstract class WriteBarrier (proto: WriteBarrier.Proto[_ <: WriteBarrier])
    extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CanThrow with CompositeNode with ProducesValue {

    def value: Node
  }

  object WriteBarrier {
    sealed abstract class Proto[N <: WriteBarrier](argTypes: Type*)(retType: Type)
      extends FixedArgs[N](ControlType +: MemoryType +: argTypes: _*)(retType)
        with ControlMemoryValueTagged[N]

    def instance(obj: Node, value: Node) = {
      DirectCall(RT.WriteBarriers.instance)(obj, value)
    }

    def static(value: Node) = {
      DirectCall(RT.WriteBarriers.static)(value)
    }

    def record(value: Node) = {
      DirectCall(RT.WriteBarriers.record)(value)
    }
  }

  object EscapeWriteBarrier {
    /** Corresponds to assignment `receiver.instanceField := value` in managed context. */
    class Instance private(proto: Instance.Proto) extends WriteBarrier(proto) {
      def receiver = arg(2)
      override def value = arg(3)
    }

    object Instance {
      case class Proto private[Instance](valueType: Type)
        extends WriteBarrier.Proto[Instance](EopType.Any, valueType)(valueType) {

        override def newInstance() = new Instance(this)
      }

      def proto(valueType: Type) = Prototype.intern(Proto(valueType))
      def apply(receiver: Node, value: Node): Instance = proto(value.tpe)(receiver, value)
      def unapply(n: Instance) = Some((n.receiver, n.value))
    }

    /** Corresponds to assignment `ClassName.staticField := value` in managed context. */
    class Static private(proto: Static.Proto) extends WriteBarrier(proto) {
      override def value = arg(2)
    }

    object Static {
      case class Proto private[Static](valueType: Type)
        extends WriteBarrier.Proto[Static](valueType)(valueType) {

        override def newInstance() = new Static(this)
      }

      def proto(valueType: Type) = Prototype.intern(Proto(valueType))
      def apply(value: Node) = proto(value.tpe)(value)
    }
  }

  class WriteBarrierMarker private extends NodeWithFixedArgs(WriteBarrierMarker) with Marker with NotProducesValue

  object WriteBarrierMarker extends FixedArgs[WriteBarrierMarker](ControlType)(ControlType) with ControlTagged[WriteBarrierMarker] {
    override def newInstance() = new WriteBarrierMarker
  }

  /////////////////////////////////////////
  // Verification write barriers

  sealed abstract class VerificationWriteBarrier (proto: VerificationWriteBarrier.Proto[_ <: VerificationWriteBarrier])
    extends NodeWithFixedArgs(proto) with AssertNode with CompositeNode with NotProducesValue

  object VerificationWriteBarrier {
    sealed abstract class Proto[N <: VerificationWriteBarrier](argsCount: Int)
      extends FixedArgs[N](ControlType +: MemoryType +: Seq.fill(argsCount)(TRefType): _*)(ControlType)
        with ControlTagged[N]
  }

  /** Corresponds to assignment `receiver.instanceField := value` in unmanaged context. */
  class VerificationInstanceWriteBarrier private extends VerificationWriteBarrier(VerificationInstanceWriteBarrier) {
    def receiver = arg(2)
    def value = arg(3)
  }

  object VerificationInstanceWriteBarrier extends VerificationWriteBarrier.Proto[VerificationInstanceWriteBarrier](2) {
    def newInstance() = new VerificationInstanceWriteBarrier
  }

  /** Corresponds to assignment `ClassName.staticField := value` in unmanaged context. */
  class VerificationStaticWriteBarrier private extends VerificationWriteBarrier(VerificationStaticWriteBarrier) {
    def value = arg(2)
  }

  object VerificationStaticWriteBarrier extends VerificationWriteBarrier.Proto[VerificationStaticWriteBarrier](1) {
    def newInstance() = new VerificationStaticWriteBarrier()
  }

  /////////////////////////////////////////
  // Synchronization

  class SynchronizedRegion extends NodeWithFixedArgs(SynchronizedRegion) with StructurallyUnique with FloatingNode {
    private def outerArgIdx = 0

    def outerRaw = arg(outerArgIdx)
    def outerRaw_=(newValue: Node): Unit = updateArg(outerArgIdx, newValue)

    def outer: Option[SynchronizedRegion] = outerRaw match {
      case s: SynchronizedRegion => Some(s)
      case IConst(SynchronizedRegion.noOuterValue) => None
      case _ => shouldNotReachHere(outerRaw)
    }
    def outer_=(newValue: SynchronizedRegion): Unit = { outerRaw = newValue }

    def inners = collect[SynchronizedRegion](valueUses)

    def allOuters = ScalaCollections.iterateUntilNone(outer)(_.outer)

    def depth: Int = outer map (_.depth + 1) getOrElse 0

    def isOutermost = outer.isEmpty

    def enters = collect[MonitorEnter](valueUses)
    def exits = collect[MonitorExit](valueUses)

    def singleMonitorObj: Option[Node] = ScalaCollections.singleton(enters map (_.obj))

    override def name = super.name + "[" + depth + "]"
  }

  object SynchronizedRegion extends FixedArgs[SynchronizedRegion](IntType)(IntType) with PrototypeStrictNodeClass[SynchronizedRegion, SynchronizedRegion] {
    private val noOuterValue = 0

    def newInstance() = new SynchronizedRegion

    /** Block enclosing given node. Exit is enclosed by its block , while enter is enclosed by outer one. */
    def enclosing(node: ControlNode): Option[SynchronizedRegion] = {
      assert(isStructuredLocking)
      val foundExits = Sets[SynchronizedRegion].newQSet
      // TODO optimization opportunities
      //      currently this thing will iterate all control nodes until appropriate MonitorEnter is found
      //      in worst case it could go through almost all method nodes
      def executedBefore(n: Node) = n match {
        case xp: XPoint => xp.owner match {
          case _: MonitorOperation =>
            // In structured locking case it is considered that MonitorOperations can never throw
            Iterator.empty
          case _ => Iterator.single(xp.owner)
        }
        case _ => n.argsByTag(Tag.CONTROL) ++ n.argsByTag(Tag.XCONTROL)
      }
      Closure.withPreAction(Sets[Node].newQSet, Seq(node))(executedBefore) {
        case exit: MonitorExit if exit != node => foundExits += exit.syncRegion
        case enter: MonitorEnter if enter != node && !foundExits.contains(enter.syncRegion) =>
          return Some(enter.syncRegion)
        case _ =>
      }
      None
    }

    def apply(outer: Option[SynchronizedRegion]): SynchronizedRegion = apply(outer getOrElse noRegion())

    def noRegion() = IConst(noOuterValue)
  }


  object MonitorOperation {
    def unapply(node: MonitorOperation): Option[Node] = Some(node.obj)
  }

  trait MonitorOperation extends SpinalMemoryNode with CompositeNode {
    def obj: Node
    protected def regionArgIdx: Int

    def syncRegion = {
      require(isStructuredLocking || currentPhase < CompilerPhase.Lowering)
      arg(regionArgIdx).asInstanceOf[SynchronizedRegion]
    }

    def syncRegion_=(newValue: SynchronizedRegion): Unit = {
      require(currentPhase < CompilerPhase.Lowering)
      updateArg(regionArgIdx, newValue)
    }
  }

  class MonitorEnter private extends NodeWithFixedArgs(MonitorEnter) with MonitorOperation with CanThrow with ProducesValue {
    def obj = arg(2)
    protected def regionArgIdx = 3
  }

  object MonitorEnter extends FixedArgs[MonitorEnter](ControlType, MemoryType, TRefType, IntType)(IntType) with ControlMemoryValueTagged[MonitorEnter] {
    def newInstance() = new MonitorEnter
    def unapply(n: MonitorEnter) = Some(n.obj)
  }

  // Note that it is considered to be non-throwing because it's the only option to sanely analyze structured locking.
  class MonitorExit extends NodeWithFixedArgs(MonitorExit) with MonitorOperation with NotProducesValue {
    def obj = arg(2)

    // Before lowering this param points to corresponding SynchronizedRegion node
    // During lowering it points to locking context word produced by MonitorEnter
    private def regionOrContextArgIdx = 3
    protected def regionArgIdx = regionOrContextArgIdx

    def lockingContext: Node = {
      require(isStructuredLocking && currentPhase >= CompilerPhase.PreLowering)
      arg(regionOrContextArgIdx)
    }
    def lockingContext_=(newValue: Node): Unit = updateArg(regionOrContextArgIdx, newValue)
  }

  object MonitorExit extends FixedArgs[MonitorExit](ControlType, MemoryType, TRefType, IntType)(ControlType) with ControlMemoryTagged[MonitorExit] {
    private lazy val intentionallyInvalidPairedEnterValue = RTConst.BiasedLocking.LockingContext.INVALID_LOCKING_CONTEXT.intValue // to distinguish from valid @ConstExpr

    def newInstance() = new MonitorExit()

    /** Must be used for methods with unstructured locking. */
    def IntentionallyInvalidPairedEnter() = IConst(intentionallyInvalidPairedEnterValue)
  }


  /////////////////////////////////////////////////////////////////////

  sealed trait DelayedOp extends SpinalMemoryNode with CompositeNode

  // TODO: following nodes should be generalized for any field type and should support static field access
  class DelayedGet private(proto: DelayedGet.Proto) extends NodeWithFixedArgs(proto) with DelayedOp with ProducesValue {
    def className = proto.className
    def fieldName = proto.fieldName
    override def name = s"$simpleName[$className.$fieldName]"

    def obj = arg(2)
  }

  object DelayedGet {
    // TODO: revise value and return types
    case class Proto private[DelayedGet](className: XString, fieldName: XString, fieldType: Type)
      extends FixedArgs[DelayedGet](ControlType, MemoryType, TRefType)(fieldType) with ControlMemoryValueTagged[DelayedGet] {
      def newInstance() = new DelayedGet(this)
    }

    def apply(className: XString, fieldName: XString, fieldType: Type) = Prototype.intern(Proto(className, fieldName, fieldType))
  }

  class DelayedPut private(proto: DelayedPut.Proto) extends NodeWithFixedArgs(proto) with DelayedOp with NotProducesValue {
    def className = proto.className
    def fieldName = proto.fieldName
    override def name = s"$simpleName[$className.$fieldName]"

    def obj = arg(2)
    def value = arg(3)
  }

  object DelayedPut {
    case class Proto private[DelayedPut](className: XString, fieldName: XString, fieldType: Type)
      extends FixedArgs[DelayedPut](ControlType, MemoryType, TRefType, fieldType)(ControlType) with ControlMemoryTagged[DelayedPut] {
      def newInstance() = new DelayedPut(this)
    }

    def apply(className: XString, fieldName: XString, fieldType: Type) = Prototype.intern(Proto(className, fieldName, fieldType))
  }

  class DelayedInstanceMethodVNum private(proto: DelayedInstanceMethodVNum.Proto) extends NodeWithFixedArgs(proto) with DelayedOp with CanThrow with ProducesValue {
    def className = proto.className
    def methodName = proto.methodName
    def signature = proto.sig
    override def name = s"$simpleName[$className.$methodName$signature]"
  }

  object DelayedInstanceMethodVNum {
    case class Proto private[DelayedInstanceMethodVNum](className: XString, methodName: XString, sig: XString)
      extends FixedArgs[DelayedInstanceMethodVNum](ControlType, MemoryType)(IntType) with ControlMemoryValueTagged[DelayedInstanceMethodVNum] {
      def newInstance() = new DelayedInstanceMethodVNum(this)
    }

    def apply(className: XString, methodName: XString, sig: XString) = Prototype.intern(Proto(className, methodName, sig))
  }

  class DelayedInstanceFieldAddress private(proto: DelayedInstanceFieldAddress.Proto) extends NodeWithFixedArgs(proto) with DelayedOp with ProducesValue {
    def className = proto.className
    def fieldName = proto.fieldName
    def signature = proto.sig

    def obj = arg(2)
  }

  object DelayedInstanceFieldAddress {
    case class Proto private[DelayedInstanceFieldAddress](className: XString, fieldName: XString, sig: XString)
      extends FixedArgs[DelayedInstanceFieldAddress](ControlType, MemoryType, TRefType)(AddrType) with ControlMemoryValueTagged[DelayedInstanceFieldAddress] {
      def newInstance() = new DelayedInstanceFieldAddress(this)
    }

    def apply(className: XString, fieldName: XString, sig: XString) = Prototype.intern(Proto(className, fieldName, sig))
  }

  class DelayedMethodAddr private(proto: DelayedMethodAddr.Proto) extends NodeWithFixedArgs(proto) with DelayedOp with ProducesValue {
    def className = proto.className
    def methodName = proto.methodName
  }

  object DelayedMethodAddr {
    case class Proto private[DelayedMethodAddr](className: XString, methodName: XString)
      extends FixedArgs[DelayedMethodAddr](ControlType, MemoryType)(AddrType) with ControlMemoryValueTagged[DelayedMethodAddr] {
      def newInstance() = new DelayedMethodAddr(this)
    }

    def apply(className: XString, methodName: XString) = Prototype.intern(Proto(className, methodName))
  }


  class BoxedValue private(proto: BoxedValue.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CompositeNode with CanThrow with ProducesValue {
    def boxType = proto.boxType
    def target = boxType.valueOf
    def primitiveType = boxType.kind

    /** Returns original argument value, it may exceed actual storage.
      * This unrefined value is adjusted during storing into memory during `valueOf()` execution in case of short integral `valueTpe`.
      */
    def inValue0 = arg(2)

    /** Boxed primitive value as it would be returned by .xxxValue() method */
    final def primitiveValue(): Node = {
      if (primitiveType == BytecodeTypeKind.BOOLEAN) {
        CondVal(Cmp(IntType, Condition.NE)(inValue0, IConst(0)))
      } else {
        PutMemoryOperation.adjustValue(SignatureType.Primitive(primitiveType).toAsm, inValue0)
      }
    }
    object PrimitiveValue {
      def unapply(n: Node): Boolean = {
        if (primitiveType == BytecodeTypeKind.BOOLEAN) {
          cond(n) {
            case CondValNE(v, IConst(0)) if v == inValue0 => true
          }
        } else {
          PutMemoryOperation.isAdjustedValue(SignatureType.Primitive(primitiveType).toAsm, inValue0)(n)
        }
      }
    }

    def hasSideEffects = cond(primitiveType, inValue0) {
      // Byte.valueOf may fail with ArrayIndexOutOfBounds on big-enough arguments
      case (BytecodeTypeKind.BYTE, IConst(v)) if java.lang.Byte.MIN_VALUE <= v && v <= java.lang.Byte.MAX_VALUE => false
      case (BytecodeTypeKind.BYTE, _) => true
      // Character.valueOf may fail with ArrayIndexOutOfBounds on negative arguments
      case (BytecodeTypeKind.CHAR, IConst(v)) if v >= 0 => false
      case (BytecodeTypeKind.CHAR, _) => true
    }

    /** Determined in PreLowering from containing block temperature. Used in making decision about inlining. */
    var isHot = false
  }

  object BoxedValue {
    // TODO: replace domain with hierarchy entity
    case class Proto private[BoxedValue](boxType: JBCBoxType, domain: Domain)
      extends FixedArgs[BoxedValue](ControlType, MemoryType, ValueType(boxType.kind))(TRefType) with ControlMemoryValueTagged[BoxedValue] {
      def newInstance() = new BoxedValue(this)
    }

    def apply(kind: BytecodeTypeKind, domain: Domain) = {
      val boxType = domain match {
        case Domain.JAVA => Java.Support.BoxType(kind)
        case Domain.SCALA => XScala.Support.BoxType(kind)
        case _ => shouldNotReachHere(domain)
      }
      Prototype.intern(Proto(boxType, domain: Domain))
    }

    def apply(boxType: JBCBoxType) = {
      val domain = boxType match {
        case _: XScala.Support.BoxType => Domain.SCALA
        case _: Java.Support.BoxType => Domain.JAVA
      }
      Prototype.intern(Proto(boxType, domain))
    }
  }

  // Copying a stack-allocated object onto a heap.
  class Evacuate private(proto: Evacuate.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode
    with CompositeNode with ProducesValue {
    def obj = arg(2)
  }

  object Evacuate {
    case class Proto private[Evacuate](outputType: Type)
      extends FixedArgs[Evacuate](ControlType, MemoryType, EopType.Any)(outputType)
      with ControlMemoryValueTagged[Evacuate] {

      def newInstance() = new Evacuate(this)
    }

    def apply(node: Node): Node = Prototype.intern(Proto(node.tpe))(node)

    def unapply(node: Evacuate): Option[Node] = Some(node.obj)
  }

  // Return address of singleton object from non-heap memory
  class SingletonObject private(proto: SingletonObject.Proto) extends NodeWithFixedArgs(proto) with SpinalNode
    with CompositeNode with ProducesValue {
    assert(proto.allocType.isSingletonObject)

    def allocType = proto.allocType
  }

  object SingletonObject {
    case class Proto private[SingletonObject](allocType: symlevel.Type)
      extends FixedArgs[SingletonObject](ControlType)(ValueType(allocType))
        with ControlValueTagged[SingletonObject] {

      def newInstance() = new SingletonObject(this)
    }

    def apply(allocType: symlevel.Type) = Prototype.intern(Proto(allocType))

    def unapply(node: SingletonObject): Option[symlevel.Type] = Some(node.allocType)
  }
}
