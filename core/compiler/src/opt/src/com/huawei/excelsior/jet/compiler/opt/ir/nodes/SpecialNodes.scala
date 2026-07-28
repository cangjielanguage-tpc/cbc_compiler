/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.jet.assembler.{AsmType, Label}
import com.huawei.excelsior.jet.assembler.util.Overflows
import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Universe}

import collection.mutable.ArrayBuffer
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.abi.ABI.TailSlot
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.middle.ValueRanges
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.{Domain, symlevel}
import com.huawei.excelsior.jet.compiler.util.Maps
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.condOpt
import scala.collection.mutable
import scala.collection.mutable.HashSet

/**
 * Special nodes are nodes that not corresponds to any bytecode operation and are created for convenience.
 *
 * 1) Bytecode parser node
 * 2) Types lowering nodes
 * 3) Backend nodes
 * 4) Marker nodes
 *
 * @author paul
 * @author cypok
 * @author conwor
 */

trait SpecialNodes extends ValueRanges { self: Universe with Nodes =>

  /////////////////////////////////////////
  // Special bytecode parser nodes

  /**
   * While bytecode parsing it can appear auxiliary nodes that cannot have any uses:
   * merge data of incompatible types and high word of long and double values.
   * They exist only temporally and provides additional checks that Java bytecode verifier must handle
   * but the checks cannot actually happen while bytecode parsing if we parse verified bytecode
   * except bugs in the bytecode parsing.
   * Its only argument is itself and it should never be committed.
   */
  class VerificationNode extends FloatingNodeWithFixedArgs(VerificationNode) {
    override def commitImpl(): Unit = { shouldNotCallThis() }
  }

  object VerificationNode extends FixedArgs[VerificationNode]()(ValueType) {
    def newInstance() = new VerificationNode
  }


  /**
   * Proxy is a node which actual form will be known later. For instance, sometimes while bytecode parsing
   * we do not know the actual state of local variables an stack registers (because of backward branches).
   * In this case, we temporally create proxy nodes for the slots of VMState that will be replaced later
   * with actual nodes.
   */
  class Proxy private (keyType: Type) extends LeafNode[Proxy](keyType) with BlockParamNode {
    private var _block: Block = _
    override def block: Block = _block
  }

  object Proxy {
    private def instance(keyType: Type, block: Block) = {
      assert(keyType.isValueType)
      val inst = new Proxy(keyType)
      inst._block = block
      inst
    }

    /** Creates and commits [[Proxy]] as it expected */
    def apply(tpe: Type)(block: Block): Proxy = instance(tpe, block)()

    /** Creates instance of a leaf node, but doesn't commit it to Universe */
    def raw(tpe: Type)(block: Block): Proxy = instance(tpe, block)
  }
  
  /** Special node, used to create exception edge between some BBlock and XBlock in IR building process.
    * When we know that some BBlock may have throwable nodes, but we do not build them yet, we can use
    * this node to prevent handler from becoming unreachable. */
  class HandlerAnchor extends NodeWithFixedArgs(HandlerAnchor) with SpinalNode with NotProducesValue {
    override def canThrow = true
  }

  object HandlerAnchor extends FixedArgs[HandlerAnchor](ControlType)(ControlType) with ControlTagged[HandlerAnchor] {
    def newInstance() = new HandlerAnchor()

    def create(from: Block, to: XBlock): Unit = {
      val anchor = insertCodeBefore(from.blockEnd, useDefaultHandler = true) {
        HandlerAnchor()
      }
      to.addArg(anchor.xpoint)
    }
  }


  /////////////////////////////////////////
  // Backend nodes

  /** Transfer is a node that moves some value from one resource to another without change of it (but with
    * possible change of type). All transfers are generated the same way - register allocation, immediates
    * allowing, code generation.
    *
    * Subset of transfers (Copy) could be also eliminated in post-processing, because they don't change type
    * of their arguments. Instead, transfers that change type (PublishRef, ConcealRef) could not be eliminated,
    * because type difference affects metadata (GC maps).
    */
  trait Transfer extends Node {
    protected def transferArgIdx: Int
    def transferArg: Node = arg(transferArgIdx)
  }

  object Transfer {
    def unapply(transfer: Transfer) = Some(~~>(transfer.transferArg.resource, transfer.resource))

    object ArgEdge { // TODO: improve EdgeMatcher and use it here
      def unapply(edge: Edge): Option[Transfer] = edge.target match {
        case transfer: Transfer if edge.targetArgIndex == transfer.transferArgIdx => Some(transfer)
        case _ => None
      }
    }
  }

  case class ~~> (from: Resource, to: Resource)


  /** Copy is a transfer, which may be eliminated in post-processing. It does not change argument value
    * and argument type.
    *
    * There are two categories of copies:
    *   1) inserted in IR before backend (having their own value)
    *   2) inserted in IR during backend (always a synonym of some other value)
    *
    * The difference between these two categories is critical for backend algorithms (unblocking and normalization),
    * but for any other parts of compiler (post-processing, asm generator) there are no difference between them.
    */
  class Copy private[SpecialNodes](proto: Copy.Proto) extends FloatingNodeWithFixedArgs(proto) with Transfer {
    override protected def transferArgIdx: Int = proto.argTypes.size - 1

    var allowedResults: ResourceSet = emptySet

    def hasOwnValue = proto.hasOwnValue

    /** Returns true iff this Copy moves value to FrameSlot. */
    def isStore: Boolean = allowedResults.isSingleton && allowedResults.single.isInstanceOf[FrameSlot]

    def storeSlot: FrameSlot = {
      assert(isStore)
      allowedResults.single.asInstanceOf[FrameSlot]
    }
  }

  object Copy {
    class Proto private[SpecialNodes](val hasOwnValue: Boolean, val argTypes: Type*) extends FixedArgs[Copy](argTypes: _*)(argTypes.last) with PrototypeStrictNodeClass[Copy, Copy] {
      def newInstance() = new Copy(this)
    }

    private def apply(from: Node, hasOwnValue: Boolean, to: ResourceSet): Copy = {
      val n = Prototype.intern(new Proto(hasOwnValue, from.tpe))(from)
      n.allowedResults = to
      n
    }

    def withoutValue(from: Node, to: ResourceSet): Copy = Copy(from, hasOwnValue = false, to)
    def withOwnValue(from: Node): Copy = Copy(from, hasOwnValue = true, universalSet)

    def unapply(copy: Copy) = Some(~~>(copy.transferArg.resource, copy.resource))
  }


  /** Copy node, which presents own value as argument of node, which will spoil it's argument. */
  class SpoiledArgSaver private(proto: SpoiledArgSaver.Proto) extends Copy(proto)

  object SpoiledArgSaver {
    case class Proto private[SpoiledArgSaver](keyType: Type) extends Copy.Proto(true, keyType) {
      override def newInstance() = new SpoiledArgSaver(this)
    }

    def apply(node: Node): Copy = {
      val n = Prototype.intern(Proto(node.tpe))(node)
      n.allowedResults = universalSet
      n
    }

    def unapply(sas: SpoiledArgSaver) = Some(sas.arg)
  }


  class CallArgStore private (proto: CallArgStore.Proto) extends Copy(proto)

  object CallArgStore {
    case class Proto private[CallArgStore] (keyType: Type) extends Copy.Proto(true, ControlType, keyType) {
      override def newInstance() = new CallArgStore(this)
    }

    def apply(inCtrl: Node, from: Node, to: FrameSlot): Copy = {
      val n = Prototype.intern(Proto(from.tpe))(inCtrl, from)
      n.allowedResults = setOf(to)
      n
    }
  }


  class InterfaceCastCBC private(proto: InterfaceCastCBC.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with ProducesValue {
    def targetType = proto.targetType
    def obj = arg(1)
  }

  object InterfaceCastCBC { // TODO-CBC should we change symlevel.Type to ClassType?
    case class Proto private[InterfaceCastCBC](targetType: symlevel.Type) extends FixedArgs[InterfaceCastCBC](ControlType, ValueType(targetType))(AddrIntType)
      with ControlValueTagged[InterfaceCastCBC] {
      def newInstance() = new InterfaceCastCBC(this)
    }

    def apply(targetType: symlevel.Type): InterfaceCastCBC.Proto = Prototype.intern(InterfaceCastCBC.Proto(targetType))
    def unapply(n: InterfaceCastCBC) = Some(n.targetType, n.obj)
  }


  /** Converts plain EOP to rich one in CBC. */
  class EnrichCBC private(proto: EnrichCBC.Proto) extends FloatingNodeWithFixedArgs(proto) {
    def rcvType: symlevel.Type = proto.rcvType
    def interfaceType: symlevel.Type = proto.interfaceType
    def obj: Node = arg(0)
  }

  object EnrichCBC {
    case class Proto private[EnrichCBC](rcvType: symlevel.Type, interfaceType: symlevel.Type)
      extends FixedArgs[EnrichCBC](ValueType(rcvType))(EopType.Eop(interfaceType)) {

      def newInstance() = new EnrichCBC(this)
    }

    def apply(rcvType: symlevel.Type, interfaceType: symlevel.Type): EnrichCBC.Proto = Prototype.intern(Proto(rcvType, interfaceType))
  }


  class ThisTypeInfo private(proto: ThisTypeInfo.Proto)
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode with ControlledNode {
    def target: SignatureType = proto.target
  }

  object ThisTypeInfo {
    case class Proto private[ThisTypeInfo](target: SignatureType)
      extends FixedArgs[ThisTypeInfo](ControlType)(AddrType) with PrototypeStrictNodeClass[ThisTypeInfo, ThisTypeInfo] {
      def newInstance() = new ThisTypeInfo(this)
    }

    def apply(x: SignatureType) = proto(x)()

    def proto(x: SignatureType) = Prototype.intern(Proto(x))
  }

  class ThisTypeInfoCBC private(proto: ThisTypeInfoCBC.Proto)
    extends FloatingNodeWithFixedArgs(proto) with CompositeNode with ControlledNode {
    def target: SignatureType = proto.target
  }

  object ThisTypeInfoCBC {
    case class Proto private[ThisTypeInfoCBC](target: SignatureType)
      extends FixedArgs[ThisTypeInfoCBC](ControlType)(AddrType) with PrototypeStrictNodeClass[ThisTypeInfoCBC, ThisTypeInfoCBC] {
      def newInstance() = new ThisTypeInfoCBC(this)
    }

    def apply(x: SignatureType) = proto(x)()
    def proto(x: SignatureType) = Prototype.intern(Proto(x))
  }

  /** Obtains `ThisTypeInfo` of its argument `obj`. */
  class ThisTypeInfoBy private extends FloatingNodeWithFixedArgs(ThisTypeInfoBy) with CompositeNode with ControlledNode {
    def obj: Node = arg(1)
  }

  object ThisTypeInfoBy extends FixedArgs[ThisTypeInfoBy](ControlType, TRefType)(AddrType) {
    def newInstance() = new ThisTypeInfoBy
  }

  class ThisTypeInfoByCBC private extends FloatingNodeWithFixedArgs(ThisTypeInfoByCBC) with CompositeNode with ControlledNode {
    def obj: Node = arg(1)
  }

  object ThisTypeInfoByCBC extends FixedArgs[ThisTypeInfoByCBC](ControlType, TRefType)(AddrType) {
    def newInstance() = new ThisTypeInfoByCBC
  }

  final class Constraints extends FloatingNodeWithVarArgs(Constraints) with ControlledNode {
    def owner = arg(0)

    // Map from edge to resource, where it's source should be live.
    private val liveOut = new ArrayBuffer[Resource]

    private val constrainedMap = Maps[Node].newQMap[Edge]

    /** Sets, that given `node` should be live on given `resource`. */
    def addWithResource(node: Node, resource: Resource): Unit = {
      if ((constrainedMap contains node) && (resource == InvalidResource || resource == delegate(node).resource)) {
        return
      }
      val edge = addArg(node)
      constrainedMap(node) = edge
      assert(edge.targetArgIndex == liveOut.size + 1)
      liveOut += resource
    }

    def +=(x: Node): Unit = addWithResource(x, InvalidResource)
    def +=(x: Node, ys: Node*): Unit = { this +=x; this ++= ys }
    def ++=(xs: IterableOnce[Node]): Unit = xs.iterator foreach +=

    /** @return node, that presents given `node` in this constraints. */
    def delegate(node: Node): Node = constrainedMap(node).source

    def isEmpty = constrainedMap.isEmpty

    def foreach[U](f: Node => U): Unit = constrainedMap.keysIterator foreach f

    def setResource(n: Node, r: Resource): Unit = {
      val idx = constrainedMap(n).targetArgIndex - 1
      assert(liveOut(idx) == InvalidResource)
      liveOut(idx) = r
    }

    def contains(node: Node): Boolean = constrainedMap.contains(node)

    /** Returns set of resources, used in this constraints.
      *
      * NOTE: we could not use `liveOut` field, because after post-processing it does not contain actual information.
      * TODO: refactor constraints
      */
    def liveResources(): ResourceSet = setOf(valueArgs.map(_.resource))
  }

  object Constraints extends VarArgs[Constraints](ControlType)(ValueType)(VoidType) with PrototypeStrictNodeClass[Constraints, Constraints] {
    def newInstance() = new Constraints

    /** @return resource, where given `edge` source should be live at block end. */
    def shouldBeLiveOn(edge: Edge): Resource = {
      val Edge(_, c: Constraints) = edge
      c.liveOut(edge.targetArgIndex - 1)
    }
  }


  class BulldozerHint(proto: BulldozerHint.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with NotProducesValue {
    def node: Node = arg(1)

    def store = proto.store
    def load = proto.load
    def spill = proto.spill
    def spillAssert = proto.spillAssert

    override def name: String = {
      val kind =
        if (spillAssert)    "SpillAssert" else
        if (spill)          "Spill" else
        if (store && load)  "StoreLoad" else
        if (store)          "Store" else
        if (load)           "Load" else
        shouldNotReachHere()

      "BulldozerHint[" + kind + "]"
    }
  }

  object BulldozerHint {
    case class Proto private[BulldozerHint](store: Boolean, load: Boolean, spill: Boolean, spillAssert: Boolean)
      extends FixedArgs[BulldozerHint](ControlType, ValueType)(ControlType) with ControlTagged[BulldozerHint] {

      def newInstance() = new BulldozerHint(this)
    }

    val store       = Proto(store = true,   load = false, spill = false,  spillAssert = false)
    val load        = Proto(store = false,  load = true,  spill = false,  spillAssert = false)
    val storeLoad   = Proto(store = true,   load = true,  spill = false,  spillAssert = false)
    val spill       = Proto(store = true,   load = false, spill = true,   spillAssert = false)
    val spillAssert = Proto(store = false,  load = false, spill = false,  spillAssert = true)
  }


  /** Marks block, where interpreter case started. */
  class InterpreterCaseMarker extends NodeWithFixedArgs(InterpreterCaseMarker) with Marker with NotProducesValue

  object InterpreterCaseMarker extends FixedArgs[InterpreterCaseMarker](ControlType)(ControlType) with ControlTagged[InterpreterCaseMarker] {
    override def newInstance() = new InterpreterCaseMarker()
  }


  /** GC-point operation node. It is used to inform the thread that GC is needed. */
  // TODO: JET-11628
  class GCPoint extends NodeWithFixedArgs(GCPoint) with SpinalNode with NotProducesValue

  object GCPoint extends FixedArgs[GCPoint](ControlType)(ControlType) with ControlTagged[GCPoint] {
    def newInstance() = new GCPoint
  }


  /** [[ExecEnvType]]-based pointers invalidation point. */
  class ExecEnvInvalidationPoint extends NodeWithFixedArgs(ExecEnvInvalidationPoint) with SpinalNode with NotProducesValue

  object ExecEnvInvalidationPoint extends FixedArgs[ExecEnvInvalidationPoint](ControlType)(ControlType) with ControlTagged[ExecEnvInvalidationPoint] {
    def newInstance() = new ExecEnvInvalidationPoint
  }


  class PreCall extends NodeWithFixedArgs(PreCall) with SpinalNode with NotProducesValue with MayHaveImplicitCheck {
    override def hasXSite = true
  }

  object PreCall extends FixedArgs[PreCall](ControlType)(ControlType) with ControlTagged[PreCall] {
    def newInstance() = new PreCall
  }


  /** Trap check on address.
    *
    * NOTE: do not try to combine TrapCheck with arbitrary Lea, because TrapCheck implementation should have fixed
    * binary format, known to runtime instructions decoder. Currently it is load from [base + offset] addressing
    * mode to register.
    */
  class TrapCheck extends NodeWithFixedArgs(TrapCheck) with SpinalNode with NotProducesValue {
    def addr = arg(1)
  }

  object TrapCheck extends FixedArgs[TrapCheck](ControlType, AddrType)(ControlType) with ControlTagged[TrapCheck] {
    def newInstance() = new TrapCheck
  }

  /**
    * Lea (load effective address) is a node used for two purposes:
    * 1) Combine addresses calculation for memory access operations
    * 2) Combine arithmetic operations for effective code generation
    *
    * Most common form of Lea is [base + index*scale + disp], where:
    * 1) scale is constant from set {1, 2, 4, 8} (proto argument)
    * 2) disp is 32-bit signed constant (proto argument)
    * 3) base and index are variables
    *
    * Most common form of Lea named `Scaled`, other forms are:
    * 1) [base + disp] - `Base`
    * 2) [index*scale + disp] - `Baseless`
    *
    * Types of `base` may be: Int, Long, Reference. If `base` is Reference type, Lea has special [[IntraReferenceType]],
    * means that it's value is an address within traceable object. Such values should not be alive through
    * GC-points, as GC can move traceable objects and not correct [[IntraReferenceType]]-d Lea values. For Baseless Lea
    * it's type determined in constructor. In other cases Lea type is the same with it's `base` type.
    *
    * Types of `index` may be: Int, Long. It should be the same width as Lea type or has less width.
    *
    * If Lea and `index` types have equal width, value of Lea operation is:
    *     `base` + `index` * `scale` + (`disp` sign-extended to `base` type)
    *
    * If width of Lea and `index` differs (means that `index` type is Int and Lea type is Long or Reference),
    * value of Lea operation is:
    *     1) UNDEFINED, if `index` value is negative
    *     2) `base` + (`index` zero-extended to `base` type) * `scale` + (`disp` sign-extended to `base` type), otherwise
    *
    * Lea nodes created in two processes:
    * 1) Lowering of high-level memory access operations (get/put field/static/array)
    * 2) Combining of arithmetic operations in preparation
    *
    * If Lea result used in RawMemoryAccess operation, backend tries to combine its calculation with access instruction.
    */
  class Lea private (proto: Lea.Proto) extends FloatingNodeWithFixedArgs(proto) {
    assert(currentPhase >= CompilerPhase.Lowering)

    def disp = proto.disp

    /** Returns true, iff this Lea is undefined for negative indices. */
    def undefinedForNegativeIndex: Boolean = this match {
      case Lea.Scaled(_, index, _, _) => fixMeArm64(!typesWithSameSize(tpe, index.tpe)) // FIXME: on arm64 maybe defined for any index
      case _ => false
    }

    private def typesWithSameSize(x: Type, y: Type) = (x, y) match {
      case (TypeWithSize(xs), TypeWithSize(ys)) => xs == ys
      case _ => shouldNotCallThis()
    }

    /** If Lea type is 64-bit, we cannot simple increment it's displacement, because in Lea calculation it will be
      * sign-extended to 64-bit type. This method checks that disp can be incremented with `x` * `m`. */
    def checkDispInc(x: Int, m: Int = 1) = {
      (typeSize(tpe) == 4) || (!Overflows.smul(x, m, 32) && !Overflows.sadd((x*m).toLong, disp.toLong, 32))
    }

    /** [base + ...] => [...] */
    def withoutBase(tpe: Type): Node = this match {
      case Lea.Baseless(_, _, _)              => this
      case Lea.Base(_, disp)                  => IntegralConst(tpe)(disp)
      case Lea.Scaled(_, index, scale, disp)  => Lea.Baseless(tpe, index, scale, disp)
    }

    /** [... + disp] => [... + newDisp] */
    def withDisp(newDisp: Int): Node = this match {
      case Lea.Baseless(index, scale, _)     => Lea.Baseless(this.tpe, index, scale, newDisp)
      case Lea.Base(base, _)                 => Lea.Base(base, newDisp)
      case Lea.Scaled(base, index, scale, _) => Lea.Scaled(base, index, scale, newDisp)
    }

    /** [base + ... + disp] => [newBase + ... + newDisp] */
    def withBaseAndDisp(newBase: Node, newDisp: Int) = this match {
      case Lea.Baseless(index, scale, _)  => Lea.Scaled(newBase, index, scale, newDisp)
      case Lea.Base(_, _)                 => Lea.Base(newBase, newDisp)
      case Lea.Scaled(_, index, scale, _) => Lea.Scaled(newBase, index, scale, newDisp)
    }
  }

  object Lea {

    abstract class Proto(argTypes: Type*)(resType: Type) extends FixedArgs[Lea](argTypes: _*)(resType) {
      def disp: Int
    }

    private def suitableBaseTypeForLea(baseType: Type): Boolean = {
      baseType.isIntegralType || baseType.isStructureType || !typeChecksEnabled
    }

    private def suitableIndexTypeForLea(indexType: Type): Boolean = {
      indexType.isIntegralType || !typeChecksEnabled
    }

    private def resultTypeByBaseType(baseType: Type): Type = baseType match {
      case TRefType => IntraReferenceType
      case _: RecordAddrType | ThinType => AddrType
      case _ => baseType
    }

    private case class BaseProto(baseType: Type, disp: Int) extends Proto(baseType)(resultTypeByBaseType(baseType)) {
      assert(suitableBaseTypeForLea(baseType))
      def newInstance() = new Lea(this)
    }

    private case class ScaledProto(baseType: Type, indexType: Type, scale: Int, disp: Int) extends Proto(baseType, indexType)(resultTypeByBaseType(baseType)) {
      assert(suitableBaseTypeForLea(baseType))
      assert(suitableIndexTypeForLea(indexType))
      def newInstance() = new Lea(this)
    }

    private case class BaselessProto(retTpe: Type, indexType: Type, scale: Int, disp: Int) extends Proto(indexType)(retTpe) {
      assert(suitableIndexTypeForLea(indexType))
      def newInstance() = new Lea(this)
    }

    object Base {
      // Special init function to emulate [[ExecEnv]] rematerialization with Lea[ExecEnv, 0]
      // Used only in BackEnd with disabled Identities
      def apply0(base: Node, disp: Int) = Prototype.intern(BaseProto(base.tpe, disp))(base)

      def apply(base: Node, disp: Int) = {
        if (disp == 0) {
          base
        } else {
          apply0(base, disp)
        }
      }

      def unapply(lea: Lea) = lea.proto match {
        case proto: BaseProto => Some((lea.arg(0), proto.disp))
        case _ => None
      }
    }

    object Baseless {
      def apply(tpe: Type, index: Node, scale: Int, disp: Int) = {
        if (tpe == index.tpe && scale == 1 && disp == 0) {
          index
        } else {
          Prototype.intern(BaselessProto(tpe, index.tpe, scale, disp))(index)
        }
      }

      def unapply(lea: Lea) = lea.proto match {
        case proto: BaselessProto => Some((lea.arg(0), proto.scale, proto.disp))
        case _ => None
      }
    }

    object Scaled {
      def apply(base: Node, index: Node, scale: Int, disp: Int = 0) = {
        assert(applicableScale(scale))
        Prototype.intern(ScaledProto(base.tpe, index.tpe, scale, disp))(base, index)
      }

      def unapply(lea: Lea) = lea.proto match {
        case proto: ScaledProto => Some((lea.arg(0), lea.arg(1), proto.scale, proto.disp))
        case _ => None
      }

      def applicableScale(scale: Long) = scale match {
        case 1L | 2L | 4L | 8L => true
        case _ => false
      }
    }

    object AnyWithIndex {
      def unapply(lea: Lea) = lea match {
        case Baseless(index, scale, disp) => Some((index, scale, disp))
        case Scaled(_, index, scale, disp) => Some((index, scale, disp))
        case _ => None
      }
    }

    object AnyWithBase {
      def unapply(lea: Lea) = lea match {
        case Base(base, disp) => Some((base, disp))
        case Scaled(base, _, _, disp) => Some((base, disp))
        case _ => None
      }
    }

    object ArithPattern {

      object Scaled {
        def unapply(x: Node): Option[(Node, Int)] = condOpt(x) {
          case Shift(ArithOp.LSL, index, DWordConst(c)) if Lea.Scaled.applicableScale(1L << c) => (index, 1 << c)
        }
      }

      object AddScaled {
        def unapply(x: Node): Option[(Node, Node, Int)] = condOpt(x) {
          case Add(l @ Scaled(li, ls), r @ Scaled(ri, rs)) =>
            // It is better to select multiplication without other uses as index
            if (l.uses.size < r.uses.size) {
              (r, li, ls)
            } else {
              (l, ri, rs)
            }
          case Add(l, Scaled(ri, rs)) => (l, ri, rs)
          case Add(Scaled(li, ls), r) => (r, li, ls)
          case Add(l, r) => (l, r, 1)
        }
      }

      def unapply(node: Node): Option[Lea] = (node match {
        case Add(AddScaled(base, index, scale), DWordConst(disp)) =>
          Some(Lea.Scaled(base, index, scale, disp))

        case Add(base, DWordConst(disp)) =>
          Some(Lea.Base(base, disp))

        case AddScaled(base, index, scale) =>
          Some(Lea.Scaled(base, index, scale))

        case _ => None

      }).map(_.asInstanceOf[Lea]) // TODO: make sure, that Identities will not convert new node from Lea to any other node type
    }
  }


  /** TOP GC algorithm uses TDBarriers (unconditional dereferences of obj.TD field) to implement read barriers
    * and intercept all field accesses of displaced (a.k.a. concurrently evacuated, moved, proxy) objects.
    */
  class TDBarrier private (proto: TDBarrier.Proto) extends FloatingNodeWithFixedArgs(proto) with ControlledNode {
    def obj = arg(TDBarrier.ObjEdge.index)
    def argMayBeNull: Boolean = proto.argMayBeNull
    def argMayBeRich: Boolean = proto.argMayBeRich
  }

  object TDBarrier {
    case class Proto(argMayBeNull: Boolean, argMayBeRich: Boolean) extends FixedArgs[TDBarrier](ControlType, TRefType)(TDBarrieredReferenceType) {
      override def newInstance() = new TDBarrier(this)
    }

    object ObjEdge extends EdgeMatcher[TDBarrier](1)

    def apply(argMayBeNull: Boolean, argMayBeRich: Boolean) = Prototype.intern(Proto(argMayBeNull, argMayBeRich))
  }


  class NoTDBarrierMarker extends NodeWithFixedArgs(NoTDBarrierMarker) with Marker with NotProducesValue

  object NoTDBarrierMarker extends FixedArgs[NoTDBarrierMarker](ControlType)(ControlType) with ControlTagged[NoTDBarrierMarker] {
    override def newInstance() = new NoTDBarrierMarker()
  }


  class TailPointer private extends LeafNode[TailPointer](AddrType) with BlockParamNode {
    override def block = entryBlock
  }

  object TailPointer {
    private val instance = new TailPointer
    def apply() = instance()
    def unique: Option[TailPointer] = if (instance.isCommitted) Some(instance) else None
  }


  class LoadTailParam private (proto: LoadTailParam.Proto) extends FloatingNodeWithFixedArgs(proto) with ProducesValue {
    def tail = arg(0)
    def param = arg(1)
  }

  object LoadTailParam {
    case class Proto(keyType: Type) extends FixedArgs[LoadTailParam](AddrType, keyType)(keyType) {
      override def newInstance() = new LoadTailParam(this)
    }

    def apply(keyType: Type) = Prototype.intern(Proto(keyType))

    def unapply(x: LoadTailParam) = Some((x.tail, x.param.resource.asInstanceOf[TailSlot].offset))
  }

  /////////////////////////////////////////
  // Special marker nodes

  trait ColdNode extends ControlNode

  /** Marks its blocks as cold (it is unlikely to be executed). */
  class ColdCodeMarker extends NodeWithFixedArgs(ColdCodeMarker) with ColdNode with Marker with NotProducesValue

  object ColdCodeMarker extends FixedArgs[ColdCodeMarker](ControlType)(ControlType) with ControlTagged[ColdCodeMarker] {
    override def newInstance() = new ColdCodeMarker
  }

  /** Marks its blocks as warm (it is unlikely to be executed based on PGO). */
  class WarmCodeMarker extends NodeWithFixedArgs(ColdCodeMarker) with ColdNode with Marker with NotProducesValue

  object WarmCodeMarker extends FixedArgs[WarmCodeMarker](ControlType)(ControlType) with ControlTagged[WarmCodeMarker] {
    override def newInstance() = new WarmCodeMarker
  }

  /** Marks the loop in which it resides as counted. */
  class CountedLoopMarker extends NodeWithFixedArgs(CountedLoopMarker) with Marker with NotProducesValue

  object CountedLoopMarker extends FixedArgs[CountedLoopMarker](ControlType)(ControlType) with ControlTagged[CountedLoopMarker] {
    override def newInstance() = new CountedLoopMarker()
  }


  /** Marks a point inside a loop scheduled for peeling. */
  class LoopPeelingMarker extends NodeWithFixedArgs(LoopPeelingMarker) with Marker with NotProducesValue

  object LoopPeelingMarker extends FixedArgs[LoopPeelingMarker](ControlType)(ControlType) with ControlTagged[LoopPeelingMarker] {
    override def newInstance() = new LoopPeelingMarker()
  }


  /** Marks the loop in which header it resides as not unrollable. */
  class NoLoopUnrollingMarker extends NodeWithFixedArgs(NoLoopUnrollingMarker) with Marker with NotProducesValue

  object NoLoopUnrollingMarker extends FixedArgs[NoLoopUnrollingMarker](ControlType)(ControlType) with ControlTagged[NoLoopUnrollingMarker] {
    override def newInstance() = new NoLoopUnrollingMarker()
  }


  /////////////////////////////////////////
  // Versioning and streamlining nodes

  /** Marks the point for versioning predicate accumulation, which splits control flow into hot and cold paths.
    *
    * Example of versioning point P, where `v(X)` denotes value use of node `X`:
    * {{{
    *          |                         |                           |
    *          P                         P                           P
    *      v(P)|                     v(A)|                       v(A)|
    *         \|      append(A)         \|      append(B)           \|
    *         if0    ---------->   v(P) if0    ---------->     v(B) if0
    *         / \                     \ / \                       \ / \
    *        T   F                    if1  F                 v(P) if1  F
    *                                 / \                       \ / \
    *                                T   F                      if2  F
    *                                                           / \
    *                                                          T   F
    * }}}
    *
    * Note that hot path should always be the ''true'' one.
    */
  class GradientVersioningPoint extends NodeWithFixedArgs(GradientVersioningPoint) with Marker with ProducesValue {
    def branchOption: Option[If] = ScalaCollections.singleton(valueUses) collect { case x: If => x }
    def branch: If = branchOption.get

    def hotExit: If.Exit = branch.trueExit
    def coldExit: If.Exit = branch.falseExit

    def append(pred: PredicateConstructor): Unit = {
      replaceByPredicate(branch, pred && PredicateConstructor.atom(this))
    }
  }

  object GradientVersioningPoint extends FixedArgs[GradientVersioningPoint](ControlType)(ConditionType) with ControlValueTagged[GradientVersioningPoint] {
    override def newInstance() = new GradientVersioningPoint()

    private var eliminated = false
    def enabled = !eliminated

    def eliminateAll(): Boolean = {
      assert(!eliminated) // eliminate only once
      var changed = false
      for (p <- all[GradientVersioningPoint]) {
        strikeOutWithValueUses(p, True())
        changed = true
      }
      eliminated = true
      changed
    }
  }

  /////////////////////////////////////////
  // InlineContextRegion nodes

  abstract class ICRegionOp protected (proto: ICRegionOpProto[_ <: ICRegionOp])
    extends NodeWithFixedArgs(proto) with Marker with NotProducesValue {

    def ic = proto.ic
  }

  abstract class ICRegionOpProto[N <: ICRegionOp] extends FixedArgs[N](ControlType)(ControlType) with ControlTagged[N] {
    def ic: InlineContext
  }


  class ICRegionEnter private (proto: ICRegionEnter.Proto) extends ICRegionOp(proto)

  object ICRegionEnter {
    case class Proto private[ICRegionEnter] (ic: InlineContext) extends ICRegionOpProto[ICRegionEnter] {
      def newInstance() = new ICRegionEnter(this)
    }

    def apply(ic: InlineContext) = Prototype.intern(Proto(ic))
  }


  class ICRegionExit private (proto: ICRegionExit.Proto) extends ICRegionOp(proto)

  object ICRegionExit {
    case class Proto private[ICRegionExit] (ic: InlineContext) extends ICRegionOpProto[ICRegionExit] {
      def newInstance() = new ICRegionExit(this)
    }

    def apply(ic: InlineContext) = Prototype.intern(Proto(ic))
  }


  /////////////////////////////////////////
  // Value range filters

  /** Node which guarantees that `filteredValue` has `filteredValueRange` below its `filteredValueCtrl`. */
  trait ValueRangeFilter extends SpinalNode with NotProducesValue {
    def filteredValue: Node
    def filteredValueRange: ValueRange
    def filteredValueCtrl: ControlNode
  }


  class RawValueRangeFilter private (proto: RawValueRangeFilter.Proto) extends NodeWithFixedArgs(proto) with ValueRangeFilter {
    def filteredValue = arg(1)
    def from = arg(2)
    def to = arg(3)

    def filteredValueRange = ValueRange(from, to, this)
    def filteredValueCtrl = this
  }

  object RawValueRangeFilter {
    case class Proto private[RawValueRangeFilter] (keyType: Type)
      extends FixedArgs[RawValueRangeFilter](ControlType, keyType, keyType, keyType)(ControlType)
        with ControlTagged[RawValueRangeFilter] {

      override def newInstance() = new RawValueRangeFilter(this)
    }

    private def proto(keyType: Type) = Prototype.intern(Proto(keyType))

    def apply(value: Node, from: Node, to: Node) = {
      require(value.tpe == from.tpe && from.tpe == value.tpe, s"incompatible types: $value, $from, $to")
      proto(value.tpe)(value, from, to)
    }
  }


  /////////////////////////////////////////
  // Memory prefetch nodes

  class Prefetch private (proto: Prefetch.Proto) extends RawMemoryAccess(proto) with SpinalNode with NotProducesValue {
    def forWrite = proto.forWrite
  }

  object Prefetch {
    case class Proto private[Prefetch] (forWrite: Boolean) extends RawMemoryAccess.Proto[Prefetch](ControlType, AddrType)(ControlType) with ControlTagged[Prefetch] {
      def addrIdx: Int = 1
      def accessType: AsmType = AsmType.I8
      def newInstance() = new Prefetch(this)
    }

    def apply(forWrite: Boolean) = Prototype.intern(Proto(forWrite))
  }


  /////////////////////////////////////////
  // Nodes for non-SSA variables

  class AssignVar private(proto: AssignVar.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with NotProducesValue {
    def variable = proto.variable
    def value = arg(1)
  }

  object AssignVar {
    case class Proto private[AssignVar] (variable: Var) extends FixedArgs[AssignVar](ControlType, variable.tpe)(ControlType)
      with SpinalNodePrototype[AssignVar] with ControlTagged[AssignVar] {

      assert(variable.tpe.isValueType)
      protected def newInstance() = new AssignVar(this)
    }

    def apply(variable: Var) = Prototype.intern(Proto(variable))
    def unapply(x: AssignVar): Option[(Var, Node)] = Some(x.variable, x.value)
  }


  class ReadVar private(proto: ReadVar.Proto) extends NodeWithFixedArgs(proto) with SpinalNode with ProducesValue {
    def variable = proto.variable
  }

  object ReadVar {
    case class Proto private[ReadVar](variable: Var) extends FixedArgs[ReadVar](ControlType)(variable.tpe)
      with SpinalNodePrototype[ReadVar] with ControlValueTagged[ReadVar] {

      assert(variable.tpe.isValueType)
      protected def newInstance() = new ReadVar(this)
    }

    def apply(variable: Var) = Prototype.intern(Proto(variable))
    def unapply(x: ReadVar): Option[Var] = Some(x.variable)
  }


  /////////////////////////////////////////
  // Other special nodes

  /** Fake value node with ''bottom'' value type. It may be used in unreachable code or in code which will became unreachable.
    *
    * Note: this node ''must not'' be cached or value numbered to avoid any weird interactions in unreachable code
    *       (see JET-12331).
    */
  class NoValue private extends LeafNode[NoValue](UnreachableValueType) with FloatingNode

  object NoValue {
    private val instance = new NoValue

    /** Calls makeNode and commits `instance`, performing all on-commit optimizations, if `instance` isn't commited.
      * NoValue is singleton, so this doesn't create any new instances of NoValue.
      */
    def apply() = instance()

    /** Returns the only NoValue node in whole IR. Returns `None` if NoValue node isn't commited.
      * This is equal to all[NoValue] except we don't have to iterate over IR to collect NoValue.
      */
    def unique: Option[NoValue] = if (instance.isCommitted) Some(instance) else None

    /** Returns the NoValue node in current scope if any. */
    def inCurrentScope: Option[NoValue] = NoValue.unique.filter(currentScope.contains)
  }

  /** This node converts an exception into the target language domain (specified in the proto).
    * For example, it converts `AJOutOfMemoryError` into `j.l.OutOfMemoryError` if the target domain is Java.
    * The actual conversion is implemented via a runtime call to `ExceptionHandling_convertIntoDomain`.
    */
  class ConvertDomain private(proto: ConvertDomain.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with ProducesValue {
    def domain = proto.domain

    def obj = arg(2)
  }

  object ConvertDomain {
    case class Proto private[ConvertDomain](domain: Domain)
      extends FixedArgs[ConvertDomain](ControlType, MemoryType, TRefType)(TRefType)
      with ControlMemoryValueTagged[ConvertDomain] {

      def newInstance() = new ConvertDomain(this)
    }

    def apply(domain: Domain) = Prototype.intern(Proto(domain))

    def unapply(x: ConvertDomain) = Some(x.domain, x.obj)
  }


  /////////////////////////////////////////
  // JVM state node (not committed in IR).

  class JVMState private extends LeafNode[JVMState](VMStateType) with FloatingNode {
    override def apply(args: Node*) = shouldNotCallThis("JVMState should not be committed to IR")
  }

  object JVMState {
    private lazy val instance = new JVMState().raw()

    def apply() = instance
  }


  /////////////////////////////////////////
  // Debug info support nodes

  trait DebugBreakpoint extends SpinalMemoryNode with NotProducesValue


  class DebugTextPosBreakpoint extends NodeWithFixedArgs(DebugTextPosBreakpoint) with DebugBreakpoint {
    override def name = s"$simpleName[$pos]"
  }

  object DebugTextPosBreakpoint extends FixedArgs[DebugTextPosBreakpoint](ControlType, MemoryType)(ControlType) with ControlMemoryTagged[DebugTextPosBreakpoint] {
    override def newInstance() = new DebugTextPosBreakpoint()
  }


  /** This marker identifies position in machine code, where natural prologue of method finished. This position used
    * for debuggers as position of function breakpoint. `natural` prologue includes standard prologue and initialization
    * of param debug variables.
    */
  class DebugPrologueEndBreakpoint extends NodeWithFixedArgs(DebugPrologueEndBreakpoint) with DebugBreakpoint

  object DebugPrologueEndBreakpoint extends FixedArgs[DebugPrologueEndBreakpoint](ControlType, MemoryType)(ControlType) with ControlMemoryTagged[DebugPrologueEndBreakpoint] {
    override def newInstance() = new DebugPrologueEndBreakpoint()
  }

  /////////////////////////////////////////
  // Coverage support node

  class CoverageCounter(proto: CoverageCounter.Proto) extends NodeWithFixedArgs(proto) with SpinalMemoryNode with NotProducesValue {
    def locs = proto.locs

    override def name = s"$simpleName[$this]"
    override def toString = locs.map((file, lines) => s"$file: [${lines.mkString(",")}]").mkString(",")
  }

  object CoverageCounter {
    case class Proto private[CoverageCounter](locs: Array[(String, Array[Int])])
      extends FixedArgs[CoverageCounter](ControlType, MemoryType)(ControlType)
      with ControlMemoryTagged[CoverageCounter] {

      def newInstance() = new CoverageCounter(this)
    }

    def apply(locs: mutable.HashMap[String, mutable.HashSet[Int]]) =
      Prototype.intern(Proto(locs.map((k, v) => (k, v.toArray.sorted)).toArray))
  }

  /////////////////////////////////////////
  // Temp node for scope merging

  class ScopeAnchor extends NodeWithFixedArgs(ScopeAnchor) with Marker with NotProducesValue

  object ScopeAnchor extends FixedArgs[ScopeAnchor](ControlType)(ControlType) with ControlTagged[ScopeAnchor] {
    override def newInstance() = new ScopeAnchor
  }
}
