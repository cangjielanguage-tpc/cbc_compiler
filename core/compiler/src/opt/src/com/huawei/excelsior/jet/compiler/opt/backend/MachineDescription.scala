/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.assembler.Location.{FReg, IReg}
import com.huawei.excelsior.jet.codeemitter.BarrierKind.STRICT_MEM
import com.huawei.excelsior.jet.compiler.Env.{tailRegister, targetArch}
import com.huawei.excelsior.jet.compiler.abi.ABI.{AltLocation, TailSlot}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.util.ScalaCollections.singleton
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.cond
import scala.annotation.nowarn

/** Description of machine-dependent node options.
  *
  * @author conwor
  * @author liontiger
  */
@nowarn("msg=match may not be exhaustive")
trait MachineDescription { self: Universe with BackEnd =>

  import RegFile.*


  /////////////////////////////////////////////////////////////////////////////
  // Resources

  class ResourceKind
  object ImmResourceKind extends ResourceKind
  class RegResourceKind extends ResourceKind
  object IRegResourceKind extends RegResourceKind
  object FRegResourceKind extends RegResourceKind
  object TailSlotResourceKind extends ResourceKind
  class MemoryResourceKind extends ResourceKind
  object FrameSlotResourceKind extends MemoryResourceKind
  object AltLocationResourceKind extends MemoryResourceKind

  /** Returns kind of `resource`. */
  def resourceKind(resource: Resource): ResourceKind = resource match {
    case Immediate    => ImmResourceKind
    case _: IReg      => IRegResourceKind
    case _: FReg      => FRegResourceKind
    case _: FrameSlot => FrameSlotResourceKind
    case _: TailSlot  => TailSlotResourceKind
    case _: AltLocation => AltLocationResourceKind
  }


  /////////////////////////////////////////////////////////////////////////////
  // Transfers

  /** Returns true iff immediate `source` may be moved to memory (heap/stack according to `isStack` arg) directly without register usage. */
  def mayImmediateBeMovedToMemoryDirectly(source: Node, isStack: Boolean): Boolean

  def temporaryResourcesForTransfer(to: ResourceKind, from: ResourceKind, source: Node): Option[ResourceSet] = (to, from, source) match {
    case (_: RegResourceKind, ImmResourceKind, _: IConst | _: FConst | _: AnyNull | _: DerivedPtr.Local | _: DerivedPtr.Global) =>
      None

    case (_: RegResourceKind, ImmResourceKind, _: LConst | _: DConst) =>
      assert(targetArch.is64Bit)
      None

    case (IRegResourceKind, ImmResourceKind, _: StackAlloc | _: AddrConst) =>
      None

    case (FRegResourceKind, ImmResourceKind, _: StackAlloc | _: AddrConst) =>
      Some(allIRegsSet)

    case (_: RegResourceKind, ImmResourceKind, _) =>
      shouldNotReachHere(s"unexpected immediate in backend: $source")

    case (_: RegResourceKind, _: RegResourceKind, _) =>
      None

    case (_: RegResourceKind, _: MemoryResourceKind, _) =>
      None

    case (_: MemoryResourceKind, ImmResourceKind, _) =>
      if (mayImmediateBeMovedToMemoryDirectly(source, isStack = to == FrameSlotResourceKind)) {
        None
      } else {
        // TODO: why not use all regs?
        regFileOf(source) match {
          case IREG => Some(allIRegsSet)
          case FREG => Some(allFRegsSet)
        }
      }

    case (_: MemoryResourceKind, _: RegResourceKind, _) =>
      None

    case (_: MemoryResourceKind, _: MemoryResourceKind, _) =>
      // TODO: why not use all regs?
      regFileOf(source) match {
        case IREG => Some(allIRegsSet)
        case FREG => Some(allFRegsSet)
      }

    case _ => shouldNotReachHere(s"unexpected transfer of $source from $from to $to")
  }

  def temporaryResourcesForTransfer(to: Resource, from: Resource, source: Node): Option[ResourceSet] =
    temporaryResourcesForTransfer(resourceKind(to), resourceKind(from), source)

  /** Returns true iff value from given `from` to given `to` resources may be copied with one transfer node. */
  final def applicableResourcesForTransfer(to: Resource, from: Resource, source: Node): Boolean =
    (to == from) || temporaryResourcesForTransfer(to, from, source).isEmpty

  /** Returns temporary resources for insertion of intermediate copy if original one cannot be generated directly. */
  def temporaryResourcesForIntermediateCopy(source: Node): Option[ResourceSet] = None


  /////////////////////////////////////////////////////////////////////////////
  // Node arguments options
  //  - immediateness
  //  - correct argument/result for transfer
  //  - bound with result
  //  - argument placement spoiling in it's instruction

  /** Returns true iff given `use` should be immediate. */
  protected def shouldBeUsedAsImmediate(use: Edge): Boolean = use match {
    case InitObj.SlotEdge(_) =>
      true
    case StackZeroing.Single.SlotEdge(_) =>
      true
    case ZeroRefs.RecordEdge(_) =>
      true
    case Edge(_: Void | _: MutFunc.Host | _: LightInterfCastCBC, _) =>
      true

    case InvokeVirtualStaticTarget.ThisTypeInfo(_) =>
      // See [[replaceUnusedValueArgs]]
      use.source match {
        case LConst(0) => true
        case x => shouldNotReachHere(x)
      }

    case ArrayIndexCheck.ArrayEdge(_) | AnyInvokeTarget.ReceiverEdge(_) =>
      // See [[replaceUnusedValueArgs]]
      assert(use.source.isInstanceOf[AnyNull]); true

    case _ => false
  }

  /** Returns true iff given `use` may be implemented as real immediate. */
  def mayBeUsedAsImmediate(use: Edge): Boolean = use.source.isInstanceOf[Constant] && (use match {
    case Transfer.ArgEdge(copy: Copy) =>
      val toKind = if (copy.isStore) {
        FrameSlotResourceKind
      } else {
        regFileOf(copy) match {
          case IREG => IRegResourceKind
          case FREG => FRegResourceKind
        }
      }
      temporaryResourcesForTransfer(toKind, ImmResourceKind, use.source).isEmpty

    case StoreMemory.InValueEdge(_) | PutJavaFieldOperation.InValueEdge(_) | ArrayPutOperation.InValueEdge(_) =>
      mayImmediateBeMovedToMemoryDirectly(use.source, isStack = false)

    case Call.Target(_) => cond(use.source) {
      case SymbolAddress(_: Method) => true
    }

    case _ => shouldBeUsedAsImmediate(use)
  })

  /** Returns sequence of argument edges, one of which should be bound with result. */
  def boundEdges(node: Node): Seq[Edge] = node match {
    case x: TDBarrier => Seq(x.inEdge(TDBarrier.ObjEdge.index))
    case _ => Seq.empty
  }

  /** Returns optional edge to `node` argument, bound with `node` result by resource.
    * Returns None, if there is no one bound argument or if there are several bound argument. */
  final def uniqueBoundEdge(node: Node) = singleton(boundEdges(node))

  /** Returns true iff `node` is bound to one of its arguments. */
  final def isBoundNode(node: Node): Boolean = boundEdges(node).nonEmpty

  /** Returns true iff `edge` may be bound for its target. */
  final def isBoundEdge(edge: Edge): Boolean = boundEdges(edge.target.groupRoot).contains(edge)

  /** Returns true iff `edge` is an actual bound edge selected (among other potential bound edges) for its target in backend. */
  final def isSelectedBoundEdge(edge: Edge): Boolean =
    isBoundEdge(edge) && edge.target.groupRoot.resource == edge.source.resource

  /** Returns true iff on current architecture some bound nodes may be combined with predecessor mov. */
  def combineSomeBoundNodesWithMoves: Boolean = false

  /** Returns true iff bound `node` may be combined with predecessor mov. */
  def mayBeCombinedWithMov(node: Node): Boolean = false

/** Returns true iff edge is special call argument.  */
  protected def isSpecialCallArgEdge(edge: Edge): Boolean = cond(edge.source) {
    case _: CallArgStore | _: Void | _: MutFunc.Host => true
  }

  // TODO: may this function be helpful in backend, not in BGCM only?
  /** Returns true iff argument passing through `edge` will be spoiled in its instruction. This predicate is used to
    * determine where to insert [[SpoiledArgSaver]] nodes used to calculate precise RP around `edge` target in BGCM.
    */
  def argumentShouldBeSaved(edge: Edge): Boolean = edge.target match {
    case call: Call =>
      if (call.isParamEdge(edge) && !isSpecialCallArgEdge(edge)) {
        // Argument passed on register.
        call match {
          case DirectCall(target) if target.isNoTracedRegsOnEntry && !edge.source.isFP => true
          case _ => !call.abi.isPreservedParameter(call.invokeArgIdx(edge))
        }
      } else {
        // Edge is not param (for example, call target) or passed on stack slot.
        // TODO: CallArgStore nodes are workaround, rework it
        false
      }

    case target =>
      uniqueBoundEdge(target) match {
        case Some(`edge`) => true

        case _ =>
          // For nodes with several bound arguments we cannot actually decide, which argument will be bound
          // with result, so we cannot decide which `edge` resource will be spoiled by `target` generation.
          // Luckily, backend will handle increasing RP in this case. TODO: it is not good, actually.
          false
      }
  }

  /** Returns true iff `rma` node can embrace `lea` addressing mode in dereference instruction. */
  def memoryAccessCanBeGroupedWithLea(rma: RawMemoryAccess, lea: Lea): Boolean

  def isAccessTypeConformsLea(tpe: AsmType, lea: Lea): Boolean


  /////////////////////////////////////////////////////////////////////////////
  // Node spoiled resources options
  //  - volatile resources (fixed 1-element sets of spoiled resources, e.g. volatile resources for call)
  //  - temporal registers (arbitrary registers, selected from the whole set of registers from according file)

  enum ExitKind {
    case NORMAL  // normal control exit of node
    case TRAP    // exceptional exit of node

    // All exits of node with code existing on them. Used to filter out resources which are spoiled only on
    // exceptional exit. If node do not have handler, this resources may be not allocated for node.
    case ALL_INSIDE_METHOD
  }
  import ExitKind.*

  /** Returns set of registers from the `file` volatile on any exit of `node`. */
  protected def volatileRegistersOnAnyExit(node: Node, file: RegFile): ResourceSet = (node, file) match {
    case (call: Call, FREG) =>
      setOf(call.abi.volatileFRegs)

    case (call @ DirectCall(method), IREG) if method.isNoTracedRegsOnEntry =>
      assert(!call.abi.hasRealTail)
      allIRegsSet ensuring (_ contains tailRegister)

    case (call: Call, IREG) if call.abi.hasRealTail =>
      setOf(call.abi.volatileIRegs) + tailRegister

    case (call: Call, IREG) =>
      setOf(call.abi.volatileIRegs)

    case _ => emptySet
  }

  /** Returns set of frame slots volatile on any exit of `node`. */
  protected def volatileSlotsOnAnyExit(node: Node): ResourceSet = node match {
    case call: Call if call.abi.argumentSlotsAreVolatile(rootABI.methodType) =>
      setOf(call.groupedArgs collect { case store: CallArgStore => store.storeSlot })

    case _ => emptySet
  }

  private def volatileAltLocationsOnAnyExit(node: Node): ResourceSet = node match {
    case call: Call => if (call.abi.hasAltLocationResult) nonResultAltLocationsSet else allAltLocationsSet
    case _ => emptySet
  }

  /** Returns set of registers from the `file` volatile on [[TRAP]] exit of `node` not including the ones returning by [[volatileRegistersOnAnyExit]]. */
  protected def extraVolatileRegistersOnTrapExit(node: Node, file: RegFile): ResourceSet = emptySet

  /** Returns set of registers from the `file` volatile on `exit` of `node`, not including registers which will be used for result allocation. */
  private def volatileRegisters(node: Node, file: RegFile, exit: ExitKind): ResourceSet = {
    val base = volatileRegistersOnAnyExit(node, file)
    lazy val extraTrap = extraVolatileRegistersOnTrapExit(node, file)

    val volatile = (node, exit) match {
      case (node: SpinalNode, ALL_INSIDE_METHOD) if node.hasXHandler => base | extraTrap
      case (_, TRAP) => base | extraTrap
      case _ => base
    }

    volatile -- (node.groupRoot.groupResults flatMap resultResourceSingleton)
  }

  /** Returns set of resources volatile on `exit` of `node`. */
  final def volatileResources(node: Node, exit: ExitKind = ALL_INSIDE_METHOD): ResourceSet = {
    val registers = unionOf(allRegFiles.iterator map { file => volatileRegisters(node, file, exit) })
    val slots = volatileSlotsOnAnyExit(node)
    val altLocations = volatileAltLocationsOnAnyExit(node)
    registers | slots | altLocations
  }

  /** Returns amount of temporal registers from the `file` required to `node`. */
  protected def temporalRegistersAmount(node: Node, file: RegFile): Int = (node, file) match {
    case (x: TDBarrier, IREG) => if (x.argMayBeRich) 2 else 1
    case _ => 0
  }

  /** Returns amount of registers (both volatile and temporal) from the `file` spoiled by `node` on `exit`. */
  final def spoiledRegistersAmount(node: Node, file: RegFile, exit: ExitKind = ALL_INSIDE_METHOD): Int =
    volatileRegisters(node, file, exit).size + temporalRegistersAmount(node, file)

  /** Returns whether the are any registers spoiled by `node` on `exit`. */
  final def hasSpoiledRegisters(node: Node, exit: ExitKind = ALL_INSIDE_METHOD): Boolean =
    allRegFiles exists (spoiledRegistersAmount(node, _, exit) > 0)

  /** Returns sequence of sets for allocation of resources spoiled by `node` on `exit`. For volatile resources there
    * are 1-element sets. For temporal resources there are whole registers set from according file.
    */
  final def spoiledResourcesSets(node: Node, exit: ExitKind = ALL_INSIDE_METHOD): Seq[ResourceSet] = {
    val registerSets = allRegFiles flatMap { file =>
      val volatilesSets = volatileRegisters(node, file, exit).asSeq map { r => setOf(r) }
      val temporalsSets = temporalRegistersAmount(node, file)
      volatilesSets ++ Seq.fill(temporalsSets)(regSetOf(file))
    }
    val slotSets = volatileSlotsOnAnyExit(node).asSeq map { s => setOf(s) }
    val altLocationSets = volatileAltLocationsOnAnyExit(node).asSeq map setOf
    registerSets ++ slotSets ++ altLocationSets
  }

  /** Returns set of volatile resources on `exit` from `check` if `check` becomes implicit in some appropriate node. */
  def implicitCheckVolatileResources(check: PureCheck, exit: ExitKind): ResourceSet


  /////////////////////////////////////////////////////////////////////////////
  // Node result options

  protected def resultResourcesSetImpl(node: Node): ResourceSet = node match {
    case _: Constant =>
      immSet

    case call: Call =>
      if (call.abi.returnType.isZST) invalidSet else setOf(call.abi.resultLocation)

    case copy: Copy if !copy.allowedResults.isUniverse =>
      assert(copy.tpe != VoidType)
      // TODO: support universeSet in node allocation mechanics and remove this patch
      copy.allowedResults

    case _: ExecEnv =>
      eeIRegSet

    case _: TailPointer =>
      tailRegSet

    case _: Param =>
      setOf(node.resource) // TODO: consider using API

    case _ => node.tpe match {
      case _: ControlType | VoidType => invalidSet
      case _ => resRegs(node)
    }
  }

  /** Returns set of resources where result of `node` may be allocated. */
  final def resultResourcesSet(node: Node): ResourceSet = {
    assert(node.mayHaveResource)
    resultResourcesSetImpl(node)
  }

  /** Returns Some(r) iff `r` is a single allowed result resource for `node`. Returns None iff one does not exists. */
  final def resultResourceSingleton(node: Node): Option[Resource] = singleton(resultResourcesSet(node).iterator)

  /** Returns set of allowed result resources for `node`, taken current machine `state` into account for bound nodes. */
  final def resultCandidates(node: Node)(state: Value => MutableResourceSet): ResourceSet = {
    if (isBoundNode(node)) {
      unionOf(boundEdges(node) map (e => nodeForm(node).argumentCandidates(e)(state))) &~ immSet
    } else {
      resultResourcesSet(node)
    }
  }

  /** Returns true iff `node` generated in storage by its nature. */
  def generatedInStorage(node: Node): Boolean = node match {
    case _: Constant => true
    case Param(num) => rootABI.paramLocations(num).isInstanceOf[TailSlot]
    case _: CallArgStore => true
    case _ => false
  }

  /** Returns true iff `node` always generated on register. */
  final def generatedOnRegister(node: Node): Boolean = !generatedInStorage(node)

  /** Returns true iff `node` generation will not produce any code. */
  def noCodeShouldBeGenerated(node: Node): Boolean = node match {
    case _: Marker | _: Param | _: Proxy | _: Block | _: Constant | _: BulldozerHint | _: TailPointer |
         _: Constraints | _: EndLocalUnmovable | _: LocalReachabilityShield | _: Phi |
         _: ExecEnvInvalidationPoint | _: PreCall | _: Projection | _: ExecEnv =>
      true

    case memBarrier: MemBarrier => memBarrier.kinds.forall(_ == STRICT_MEM)

    case transfer: Transfer => transfer.resource == transfer.transferArg.resource

    case _ => false
  }


  /////////////////////////////////////////////////////////////////////////////
  // Live ranges checks

  abstract class LiveRangesCheck {
    def toCheck(rangeRoot: Node): Boolean
    def checkPoints(): IterableOnce[Node]

    final def check(): Unit = {
      val points = checkPoints().iterator.toList
      for (node <- allNodes if node.producesValue) {
        if (node.mayHaveResource && toCheck(node)) {
          val range = LiveRanges.ssa(node)
          for (point <- points) {
            assert(!(range contains point), s"live range of $node contains forbidden point $point")
          }
        }
      }
    }
  }

  /** Checks that all [[FragilePointerType]]-d values do not live through invalidation points. */
  private val fragilePointerTypedCheck: LiveRangesCheck = new LiveRangesCheck {
    override def toCheck(rangeRoot: Node) = cond(rangeRoot) { case node: FloatingNode if node.isFragilePointer => true }
    override def checkPoints() = allNodes filter { node => node.isGroupRoot && (couldInvalidateFragilePointers(node) || node.isInstanceOf[Return]) }
  }

  /** Checks that no live ref values are exist on registers in gc safe regions (TrapCheck.Enter - TrapCheck.Leave). */
  private val gcSafeRegionsCheck: LiveRangesCheck = new LiveRangesCheck {
    override def toCheck(rangeRoot: Node) = mayBeTraceableReference(rangeRoot) && rangeRoot.resource.isIReg
    override def checkPoints() = all[Call].filter(_.gcActions.generateGCSafeRegion)
  }

  def liveRangesChecks(): Seq[LiveRangesCheck] = Seq(fragilePointerTypedCheck, gcSafeRegionsCheck)


  /////////////////////////////////////////////////////////////////////////////
  // Other

  /** Returns true iff `node` rematerialization costs about nothing (e.g. IConst on amd64, which may be
    * inlined in a lot of instructions as immediate). */
  def zeroCostRematerialization(node: Node): Boolean = node match {
    case _: Constant => true
    case _ => false
  }

  /** Returns true iff resource where node live is fixed and read-only. */
  final def nodeOnReadOnlyResource(node: Node): Boolean = node match {
    case _: Param =>
      // Tail params are owned by the frame which passed them, so we treat them as read-only for now.
      // TODO: consider to allow writes that preserve reference/primitive marks and adjust ABI.argumentSlotsAreVolatile accordingly.
      generatedInStorage(node)
    case _: Constant | _: ExecEnv => true
    case _ => false
  }

  /** Returns true iff current platform allows to group efficiently divRem with its check. */
  def implicitDivisorCheckAllowed: Boolean

  /** Returns true iff current platform benefits from combining arithmetic expressions into Lea node and group it with RMA. */
  def lowLevelMemoryOperationsAddressesCombiningInLeaHasImpact: Boolean = true

  /** Returns true iff current platform benefits from combining arithmetic operations to Lea nodes. */
  def arithOperationsCombiningInLeaHasImpact: Boolean = true

  /** Returns true iff `r` is storage resource. */
  def isStorageResource(r: Resource): Boolean = r match {
    case _: FrameSlot | Immediate => true
    case _ => false
  }
}
