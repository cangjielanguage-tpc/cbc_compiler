/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.assembler.{AsmType, Symbol}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.opt.middle.types.LoweredReferences.LoweredReferenceApprox
import com.huawei.excelsior.jet.compiler.opt.middle.types.LoweredReferences.LoweredReferenceApprox.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ClassType
import com.huawei.excelsior.jet.compiler.types.References.{OpenCone, RefNull, ReferenceApprox}
import com.huawei.excelsior.jet.compiler.symlevel.{MethodReference, SignatureType, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.Approximation

import scala.PartialFunction.condOpt

/**
 * AJ-specific nodes.
 *
 * @author cypok
 * @author conwor
 */

trait AJNodes { self: Universe with ObjectOperationNodes =>

  /** Unmanaged array get operation. */
  class UArrayGet private(proto: UArrayGet.Proto) extends NodeWithFixedArgs(proto) with ArrayGetOperation {
    override final def accessType: AsmType = proto.elemType
  }

  object UArrayGet {
    case class Proto private[UArrayGet](elemType: AsmType)
            extends FixedArgs[UArrayGet](ControlType, MemoryType, AddrType, IntType)(NumericType(elemType)) {
      def newInstance() = new UArrayGet(this)
    }

    def apply(elemType: AsmType) = Prototype.intern(Proto(elemType))
  }


  /** Unmanaged array put operation. */
  class UArrayPut private(proto: UArrayPut.Proto) extends NodeWithFixedArgs(proto) with ArrayPutOperation {
    override final def accessType: AsmType = proto.elemType
  }

  object UArrayPut {
    case class Proto private[UArrayPut](elemType: AsmType)
            extends FixedArgs[UArrayPut](ControlType, MemoryType, AddrType, IntType, NumericType(elemType))(ControlType)
            with ControlMemoryTagged[UArrayPut] {
      def newInstance() = new UArrayPut(this)
    }

    def apply(elemType: AsmType) = Prototype.intern(Proto(elemType))
  }


  /** Low-level address of symbol with constant offset. */
  class AddrConst private(proto: AddrConst.Proto) extends FloatingNodeWithFixedArgs(proto) with ControlledNode with Constant {
    assert(currentPhase >= CompilerPhase.Lowering, "before lowering high-level node SymbolAddress should be used")

    def symbol: Symbol = proto.symbol
    def offset: Int = proto.offset
  }

  object AddrConst {
    case class Proto private[AddrConst](symbol: Symbol, offset: Int) extends FixedArgs[AddrConst](ControlType)(AddrType) {
      def newInstance(): AddrConst = new AddrConst(this)
    }

    def apply(inCtrl: ControlNode, symbol: Symbol, offset: Int): Node =
      Prototype.intern(Proto(symbol, offset)).withExplicitArgs(inCtrl)

    def unapply(x: AddrConst) = Some(x.inCtrl, x.symbol, x.offset)
  }


  // TODO: consider making SymbolAddress "extends Constant" like some reference consts (e.g. ClassObject)
  // See Identity of Cmp(SymbolAddress, NullAddr) for motivation.
  class SymbolAddress private (val symbol: Symbol) extends CachedLeafNode[SymbolAddress](AddrType) with CompositeNode with FloatingNode {
    def cacheKey = symbol
  }

  object SymbolAddress {
    // for serialization
    def newProto(symbol: Symbol): SymbolAddress = Prototype.intern(new SymbolAddress(symbol))

    private def impl(symbol: Symbol, inCtrl: Option[ControlNode]): Node = {
      if (currentPhase < CompilerPhase.Lowering) {
        // Now there is no reason to create controlled SymbolAddress before lowering.
        assert(inCtrl.isEmpty)

        // Before backend phase we do not know accessKind,
        // because SymbolAddress node may be inlined into other component code.
        newProto(symbol)()

      } else {
        val symbolAddr = AddrConst(inCtrl getOrElse entryBlock, symbol, offset = 0)

        symbolLinker.accessKind(symbol) match {
          case AccessKind.DIRECT => symbolAddr
          case AccessKind.FAR => shouldNotReachHere("not implemented yet")
        }
      }
    }

    def apply(symbol: Symbol): Node = impl(symbol, None)
    def controlled(symbol: Symbol, inCtrl: ControlNode): Node = impl(symbol, Some(inCtrl))

    def unapply(x: Node) = condOpt(x) {
      case x: SymbolAddress => x.symbol
      case AddrConst(_, symbol, 0) => symbol
    }
  }

  /** High-level node for imported index of a type in root class.
    * Replaced to actual int constant during preparation.
    */
  class ImportedIndex private (val targetType: symlevel.ClassType) extends CachedLeafNode[ImportedIndex](IntType) with Constant {
    def cacheKey = targetType
  }

  object ImportedIndex {
    def apply(x: symlevel.ClassType): ImportedIndex = Prototype.intern(new ImportedIndex(x))()
    def unapply(x: ImportedIndex) = Some(x.targetType)
  }

  object TypeHandle {
    def apply(tpe: symlevel.Type): Node = SymbolAddress(tpe.getTypeHandle)
  }

  object RawInstanceDescriptor {
    def apply(tpe: symlevel.Type): Node = SymbolAddress(tpe.getInstanceDescriptor)
  }


  // TODO: make Constant?
  sealed abstract class BuiltInTypeInfo(proto: BuiltInTypeInfo.Proto[_ <: BuiltInTypeInfo])
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode with ControlledNode {

    def targetType = proto.targetType
  }

  object BuiltInTypeInfo {
    abstract class Proto[T <: BuiltInTypeInfo] extends FixedArgs[T](ControlType)(AddrType) with PrototypeStrictNodeClass[T, T] {
      def targetType: symlevel.Type
    }
  }

  class RunTimeTypeInfo private(proto: RunTimeTypeInfo.Proto) extends BuiltInTypeInfo(proto) {
    override def targetType: symlevel.ClassType = proto.targetType
  }

  object RunTimeTypeInfo {
    case class Proto private[RunTimeTypeInfo](targetType: symlevel.ClassType) extends BuiltInTypeInfo.Proto[RunTimeTypeInfo] {
      def newInstance() = new RunTimeTypeInfo(this)
    }

    def apply(x: symlevel.ClassType) = proto(x)()
    def proto(x: symlevel.ClassType) = Prototype.intern(Proto(x))
  }

  class InstanceDescriptor private (proto: InstanceDescriptor.Proto) extends BuiltInTypeInfo(proto) {
    override def targetType: symlevel.ClassType = proto.targetType
  }

  object InstanceDescriptor {
    case class Proto private[InstanceDescriptor] (targetType: symlevel.ClassType) extends BuiltInTypeInfo.Proto[InstanceDescriptor] {
      def newInstance() = new InstanceDescriptor(this)
    }

    def apply(x: symlevel.ClassType) = Prototype.intern(Proto(x))
  }


  // TODO: make Constant?
  class FieldAddr private (proto: FieldAddr.Proto)
    extends FloatingNodeWithFixedArgs(proto) with ControlledNode with CompositeNode {

    def field = proto.field
  }

  object FieldAddr {
    case class Proto private[FieldAddr] (field: symlevel.Field)
      extends FixedArgs[FieldAddr](ControlType)(AddrType) with PrototypeStrictNodeClass[FieldAddr, FieldAddr] {

      def newInstance() = new FieldAddr(this)
    }

    def apply(x: symlevel.Field) = Prototype.intern(Proto(x))
    def unapply(x: FieldAddr) = Some(x.field)
  }


  /** Cangjie-specific wrapper to represent managed method's address as a CCall CFunc function pointer. */
  class CFuncWrapperAddr private(proto: CFuncWrapperAddr.Proto)
    extends FloatingNodeWithFixedArgs(proto) with ControlledNode with CompositeNode {

    def target = proto.target
  }

  object CFuncWrapperAddr {
    case class Proto private[CFuncWrapperAddr](target: symlevel.Method)
      extends FixedArgs[CFuncWrapperAddr](ControlType)(AddrType) with PrototypeStrictNodeClass[CFuncWrapperAddr, CFuncWrapperAddr] {

      assert(!target.isAbstract)
      assert(target.isCAnnotated)

      def newInstance() = new CFuncWrapperAddr(this)
    }

    def apply(x: symlevel.Method) = Prototype.intern(Proto(x))
    def unapply(x: CFuncWrapperAddr) = Some(x.target)
  }


  /** Address of instance method from VMT. */
  class VirtualMethodAddr private(proto: VirtualMethodAddr.Proto)
    extends FloatingNodeWithFixedArgs(proto) with ControlledNode with CompositeNode {

    def originalRef = proto.originalRef

    def obj = arg(1)
  }

  object VirtualMethodAddr {
    case class Proto private[VirtualMethodAddr](originalRef: MethodReference)
      extends FixedArgs[VirtualMethodAddr](ControlType, TRefType)(AddrType) with PrototypeStrictNodeClass[VirtualMethodAddr, VirtualMethodAddr] {

      assert(originalRef.isVirtualCall && originalRef.hasVirtualMethodSlot)

      def newInstance() = new VirtualMethodAddr(this)
    }

    def apply(x: MethodReference) = Prototype.intern(Proto(x))
    def unapply(x: VirtualMethodAddr) = Some(x.originalRef, x.obj)
  }

  /** Address of field in record.
    *
    * Inspired by `getelementptr` instruction in LLVM IR.
    * TODO: expand functionality of this node to be as powerful as `getelementptr`
    *       and use it instead of GetField/GetStatic to access nested record fields
    */
  class GetElementPtr private(proto: GetElementPtr.Proto)
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode {

    def field = proto.field

    def base = arg(0)
  }

  object GetElementPtr {
    case class Proto private[GetElementPtr](field: symlevel.Field)
      extends FixedArgs[GetElementPtr](ValueType.fromSig(SignatureType.fromSymType(field.getDeclaringClass)))(AddrType) { // TODO: obtain signature type of declaring class without erasure

      def newInstance() = new GetElementPtr(this)
    }

    def proto(x: symlevel.Field) = Prototype.intern(Proto(x))

    def apply(field: symlevel.Field)(base: Node) = proto(field)(base)
    def unapply(x: GetElementPtr) = Some(x.field, x.base)
  }


  /** Node for AJ string (BString or UString): implementation of bstr and ustr intrinsics. */
  class AJString private (val str: XString, val bstr: Boolean) extends CachedLeafNode[AJString](AddrType) with CompositeNode with FloatingNode with Constant {
    def cacheKey = (str, bstr)
  }

  object AJString {
    // for serialization
    def newProto(str: XString, bstr: Boolean) = Prototype.intern(new AJString(str, bstr))

    def apply(str: XString, bstr: Boolean): Node = {
      if (currentPhase < CompilerPhase.Lowering || targetArch == CBC) {
        // Before backend phase and in CBC we cannot create const data segments.
        newProto(str, bstr)()
      } else {
        // During and after lowering it is not good to create composite nodes.
        SymbolAddress(symbolLinker.makeConstStringData(str, bstr))
      }
    }

    def unapply(x: AJString) = Some(x.str, x.bstr)

    def bstr(str: XString): Node = apply(str, bstr = true)
    def bstr(str: String): Node = bstr(XString(str))

    def ustr(str: XString): Node = apply(str, bstr = false)
    def ustr(str: String): Node = ustr(XString(str))
  }


  /** Current ExecEnv object. Presents only in managed methods. */
  class ExecEnv private extends LeafNode[ExecEnv](ExecEnvType) with FloatingNode

  object ExecEnv {
    private lazy val instance = new ExecEnv
    def apply() = instance()
  }

  class StackDescriptor private extends LeafNode[StackDescriptor](AddrType) with FloatingNode

  object StackDescriptor {
    private lazy val instance = new StackDescriptor
    def apply() = instance()
  }

  /** Node for header of current frame. Presents only in managed methods. */
  class FrameHeader private extends LeafNode[FrameHeader](AddrType) with FloatingNode

  object FrameHeader {
    private lazy val instance = new FrameHeader
    def apply() = instance()
  }

  /** High-level node for type handle of caller class. Implementation of JavaCallStackUtils.getCallerClassHandle intrinsic. */
  class AJCallerClass private(proto: AJCallerClass.Proto) extends NodeWithFixedArgs(proto)
    with SpinalMemoryNode with CanThrow with CompositeNode with ProducesValue {

    def ic = proto.ic
    def depth: Node = arg(2)
  }

  object AJCallerClass {
    case class Proto private[AJCallerClass](ic: InlineContext)
      extends FixedArgs[AJCallerClass](ControlType, MemoryType, IntType)(AddrType) with ControlMemoryValueTagged[AJCallerClass] {

      def newInstance() = new AJCallerClass(this)
    }

    def apply(ic: InlineContext) = Prototype.intern(Proto(ic))
  }


  /** Collected variable arguments for varargs method.
    * VarArgsList node has type `Object[]` to conform Java style varargs type.
    *
    *  It is temporal node that dies after Dataflow pass and should not be serialized.
    */
  class VarArgsList extends FloatingNodeWithFixedArgs(VarArgsList)

  object VarArgsList extends FixedArgs[VarArgsList](TRefType)(TRefType) {
    protected def newInstance() = new VarArgsList

    def unapply(n: VarArgsList): Some[(Seq[Node], Seq[SignatureType])] = {
      val builder = n.arg.asInstanceOf[Builder]
      Some((builder.varArgsReversed.reverse, builder.types))
    }

    def start() = Builder()

    /**
     *  Builder holds var arguments for varargs method.
     *  The arguments are added in reverse order, since VarArgsBuilder.arg intrinsic calls take values from
     *  bytecode stack in reverse order.
     *
     *  It is temporal node that dies after Dataflow pass and should not be serialized.
     */
    class Builder extends FloatingNodeWithVarArgs(Builder) with StructurallyUnique with ControlledNode {
      private[VarArgsList] var types = List[SignatureType]()

      def addVarArg(n: Node, t: SignatureType): Builder = {
        assert(n.tpe == ValueType.fromSig(t))
        addArg(n)
        types = t :: types
        this
      }

      def done() = VarArgsList(this)

      // This is a workaround for bug SI-2034: "getClass fails for doubly nested inner-class"
      override def name = "VarArgsListBuilder"

      def varArgsReversed = argsTail(1)
    }

    object Builder extends VarArgs[Builder](ControlType)(ValueType)(TRefType) {
      protected def newInstance() = new Builder
    }
  }


  object ThinTypeHandle {
    def apply(tpe: symlevel.ClassType): Node = {
      require(tpe.isPolyThinClass)
      SymbolAddress(tpe.getThinTypeHandle)
    }
  }


  object ThinNull {
    def apply() = AnyNull(ThinType)
    def unapply(n: AnyNull): Boolean = n.tpe == ThinType
  }


  class ThinInstanceOf private (proto: ThinInstanceOf.Proto) extends FloatingNodeWithFixedArgs(proto) with CompositeNode {
    require(targetType.isThinClass)
    def targetType = proto.targetType
    def obj: Node = arg(0)
  }

  object ThinInstanceOf {
    case class Proto private[ThinInstanceOf] (targetType: symlevel.Type) extends FixedArgs[ThinInstanceOf](ThinType)(IntType) {
      def newInstance() = new ThinInstanceOf(this)
    }

    def apply(targetType: symlevel.Type) = Prototype.intern(Proto(targetType))
  }


  class ThinCheckCast private (proto: ThinCheckCast.Proto) extends PureCheck(proto) with CompositeNode with TypeFilterNode with NotProducesValue {
    require(targetType.isThinClass)

    def targetType = asClassType(proto.targetType)

    def obj: Node = arg(1)

    def filteredArg: Node = obj

    def filterType(tpe: Approximation, point: ControlNode): (Approximation, Boolean) = tpe match {
      case argTpe: ReferenceApprox if point == this =>
        argTpe weakIntersect OpenCone(ClassType(targetType), mayBeNull = true)
      case argTpe: LoweredReferenceApprox => (argTpe, true)
      case _ =>
        shouldNotReachHere(tpe)
    }
  }

  object ThinCheckCast {
    case class Proto private[ThinCheckCast] (targetType: symlevel.Type, trusted: Boolean)
      extends PureCheckPrototype[ThinCheckCast](ControlType, ThinType)(ControlType)(targetType) with ControlTagged[ThinCheckCast] {

      def newInstance() = new ThinCheckCast(this)
    }

    def apply(targetType: symlevel.Type, trusted: Boolean = false): Proto = Prototype.intern(Proto(targetType, trusted))
    def unapply(c: ThinCheckCast): Option[(symlevel.Type, Node)] = Some(c.targetType, c.obj)
  }


  class ThinNullCheck private (proto: ThinNullCheck.Proto) extends AbstractNullCheck(proto) with TypeFilterNode with NotProducesValue {
    protected def objArgIdx: Int = 1

    def filteredArg: Node = obj

    def filterType(tpe: Approximation, point: ControlNode): (Approximation, Boolean) = tpe match {
      case argTpe: ReferenceApprox if point == this => argTpe subtract RefNull
      case argTpe: LoweredReferenceApprox if point == this => argTpe subtract LoweredRefNull
      case _ => shouldNotReachHere(tpe)
    }
  }

  object ThinNullCheck {
    case class Proto private[ThinNullCheck] (trusted: Boolean)
      extends PureCheckPrototype[ThinNullCheck](ControlType, ThinType)(ControlType)() with ControlTagged[ThinNullCheck] {

      def newInstance() = new ThinNullCheck(this)
    }

    def apply(args: Node*): ThinNullCheck = apply(false)(args: _*)
    def apply(trusted: Boolean): Proto = Prototype.intern(Proto(trusted))
  }


  class ThinNew private (proto: ThinNew.Proto) extends NodeWithFixedArgs(proto)
    with SpinalMemoryNode with CompositeNode with NotProducesValue {

    require(initType.isThinClass)
    def initType = proto.initType
    def addr: Node = arg(2)
  }

  object ThinNew {
    case class Proto private[ThinNew] (initType: symlevel.Type)
      extends FixedArgs[ThinNew](ControlType, MemoryType, ThinType)(ControlType) with ControlMemoryTagged[ThinNew] {

      def newInstance() = new ThinNew(this)
    }

    def apply(initType: symlevel.Type) = Prototype.intern(Proto(initType))
    def unapply(n: ThinNew): Option[(symlevel.Type, Node)] = Some(n.initType, n.addr)
  }


  /** This check guarantees, that `base` argument is address not equal to 0, pointed to structure,
    * which size is at least `offset`. Under it we can read flat thin field from `base` by `offset`
    * displacement and be sure, that result will not be null. This check is always trusted.
    */
  class GetFlatThinCheck private() extends PureCheck(GetFlatThinCheck) with CompositeNode with NotProducesValue {
    def base = arg(1)
    def offset = arg(2)
  }

  object GetFlatThinCheck extends PureCheckPrototype[GetFlatThinCheck](ControlType, AddrType, IntType)(ControlType)() with ControlTagged[GetFlatThinCheck] {
    def trusted: Boolean = true
    def newInstance() = new GetFlatThinCheck
    def unapply(x: GetFlatThinCheck) = Some((x.base, x.offset))
  }


  class GetFlatThin private(proto: GetFlatThin.Proto) extends FloatingNodeWithFixedArgs(proto) with ControlledNode with CompositeNode {
    def base = arg(1)
    def offset = arg(2)
    def thinType = proto.thinType
  }

  object GetFlatThin {
    case class Proto(thinType: symlevel.ClassType) extends FixedArgs[GetFlatThin](ControlType, AddrType, IntType)(ThinType) {
      def newInstance() = new GetFlatThin(this)
    }
    def apply(thinType: symlevel.ClassType): Proto = Prototype.intern(Proto(thinType))
    def unapply(x: GetFlatThin) = Some((x.base, x.offset, x.thinType))
  }


  /////////////////////////////////////////
  // Lock wrappers

  sealed abstract class LockWrapper (proto: LockWrapper.Proto[_ <: LockWrapper])
    extends NodeWithFixedArgs(proto) with SpinalMemoryNode with CompositeNode with NotProducesValue

  object LockWrapper {
    sealed abstract class Proto[N <: LockWrapper]
      extends FixedArgs[LockWrapper](ControlType, MemoryType)(ControlType) with ControlMemoryTagged[LockWrapper]
  }

  class IncHeldLocks private extends LockWrapper(IncHeldLocks)

  object IncHeldLocks extends LockWrapper.Proto[IncHeldLocks] {
    override protected def newInstance() = new IncHeldLocks
  }

  class DecHeldLocks extends LockWrapper(DecHeldLocks)

  object DecHeldLocks extends LockWrapper.Proto[IncHeldLocks] {
    override def newInstance() = new DecHeldLocks
  }
}
