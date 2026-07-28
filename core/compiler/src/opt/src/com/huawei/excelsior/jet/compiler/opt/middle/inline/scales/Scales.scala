/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline.scales

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.types.Guards.{CHABitGuard, Guard, LevelGuard, MaxClosedConeGuard, MethodGuard, OpenConeGuard, PointGuard}
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind as MAK
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.jet.util.ScalaCollections.sumBy
import com.huawei.excelsior.jet.util.graph.Loop
import xscala.util.MathUtils.*

import scala.Function.const
import scala.annotation.nowarn

trait Scales { self: Universe =>

  def nodeWeight(node: Node): Double

  def nonInvariantLoopWeight(loop: Loop[Block], invariant: Node => Boolean = const(false)): Double = withAnyGCM {
    // Note: invariant nodes do not contribute to loop weight.
    def nonInvariantWeight(n: Node) = if (invariant(n)) 0.0 else nodeWeight(n)
    val nodes = Closure[Node](loop.body)(_.usesWithParamNodes filter (u => loop.body(u.block)))
    sumBy(nodes)(nonInvariantWeight)
  }

  private [scales] trait ScalesImpl {
    val Infinity = Double.PositiveInfinity

    def tauTestWeight(guard: Guard) = guard match {
      case CHABitGuard => 2 + getPutWeight + intCmpWeight
      case _: PointGuard => getPutWeight + /* addrCmpWeight */ 3
      case _: LevelGuard => 2 * getPutWeight + intCmpWeight
      case _: MethodGuard | _: MaxClosedConeGuard | _: OpenConeGuard => 2 * getPutWeight + /* addrCmpWeight */ 3
      case _ => shouldNotReachHere(guard)
    }

    // Arch dependent weight of common primitives
    def jmpWeight: Double
    def jccWeight: Double
    def ifWeight = jmpWeight + jccWeight
    def movWeight: Double

    def intCmpWeight: Double

    def execEnvWeight: Double
    def frameHeaderWeight: Double
    def getPutWeight: Double
    def trapCheckWeight: Double
    def clinitWeight: Double
    def fastTypeCheck: Double

    def directCallWeight(paramsCount: Int): Double
    def indirectCallWeight(paramsCount: Int): Double
    def getVirtualMethodAddrWeight: Double
    def getInterfMethodAddrWeight: Double

    def divisorCheckWeight: Double

    def checkedOpIntConstWeight(v: Long): Double
    def checkedOpIntDivRemWeight(n: Node): Double
    def checkedOpIntHighMulWeight(n: Node): Double
    def checkedOpIntLogicWeight: Double
    def checkedOpIntAddSubWeight: Double
    def checkedOpIntMulWeight: Double
    def checkedOpShiftWeight: Double

    def ldrLiteralWeight: Double

    /** Weight of one node as if method body is inlined into caller.
      * Double type used for `Infinity` value with operation `Infinity` + x resulting `Infinity`. */
    // TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
    @nowarn("msg=The outer reference in this type test cannot be checked at run time")
    def nodeWeight(n: Node): Double = n match {
      // Arch independent "empty" nodes
      case _: Block => 0
      case _: Param => 0
      case _: Phi => 0
      case _: Marker => 0
      case _: Projection => 0
      case _: Void => 0
      case _: RawValueRangeFilter => 0
      case _: AssignVar | _: ReadVar => 0

      case _: Return => 0
      case _: Throw => 0
      case _: Halt => 0

      case _: UnreachableBlockEnd => 0
      case _: UnreachableThrowing => 0

      case _: True | _: False => 0

      case _: LocalReachabilityShield => 0

      case _: IsComputableAtCompileTime | _: ComputeAtCompileTime => 0

      case _: SynchronizedRegion => 0

      case x: PureCheck if x.trusted => 0

      case _: EOPOperation | _: WeakCast => 0 // force zero-weight of EOP operations

      case ReinterpretCast(_, _, _) => 0
      case _: PublishRef | _: ConcealRef | _: SingletonObject => movWeight

      case _: DebugBreakpoint => 0

      case _: ExecEnvInvalidationPoint => 0

      // Arch independent heavy nodes
      case _: ArrayFill => Infinity
      case _: AJArrayFill => Infinity
      case _: Deferred => Infinity

      // Structural arch independent nodes
      case _: Goto => jmpWeight

      case _: Not | _: CondVal => 0 // in most cases it will be collapsed (i.e. usually used in Cmp)

      case _: GetField | _: PutField | _: LoadMemory | _: StoreMemory => getPutWeight

      case _: GetElementPtr => getPutWeight // TODO: calculate proper weight

      case n: ZeroRefs => 0 // TODO: consider `n.recordType.getRefFieldOffsets.size * getPutWeight` as weight

      case TauTest(guard, _, _) => tauTestWeight(guard)
      case n: TauSwitch => n.cases.foldLeft(0.0) { (acc, guard) => acc + ifWeight + tauTestWeight(guard) }

      case _: AssertNode => 0

      case _: PreparationCheck => 0 // TODO: use proper weight and rework weighting heuristics

      case _: ArrayIndexCheck => ifWeight + directCallWeight(0)
      case _: ArrayStoreCheck => directCallWeight(2)

      case _: InitializedTest => 0 // It's weight is included into clinit/init checks/asserts.
      case _: Clinit | _: PackageInit | _: PackageInitCheck => clinitWeight
      case _: New => directCallWeight(1)
      case _: NewString => directCallWeight(2)
      case n: NewArray => movWeight /* elem size */ + directCallWeight(1 + n.lengths.size)
      case _: NewArrayCopy | _: NewArrayFill => movWeight /* elem size */ + directCallWeight(5)
      case n: NewArrayCopyRT => directCallWeight(if (n.isCopyOfRange) 3 else 2)
      case _: NewArrayRT => directCallWeight(2)
      case n: NewArrayMimic => n.lengths.size * (ifWeight + directCallWeight(0))

      case _: InstanceOf             => fastTypeCheck
      case _: CheckCast              => fastTypeCheck + directCallWeight(1) /* throwing exception */

      case _: CheckCastTrustedDelayed => 0 // nop

      case n: StrConcat => directCallWeight(n.concatenatedArgs.size)

      case _: ExecEnv => execEnvWeight
      case _: FrameHeader => frameHeaderWeight
      case _: TrapCheck => trapCheckWeight

      case n @ AnyDirectCall(_) => directCallWeight(n.invokeArgs.size)
      case n @ AnyVirtualCall() => indirectCallWeight(n.invokeArgs.size) + (n.akind match {
        case MAK.VIRTUAL => getVirtualMethodAddrWeight
        case MAK.INTERFACE => getInterfMethodAddrWeight
        case x => shouldNotReachHere(x)
      })
      case n: Call => indirectCallWeight(n.invokeArgs.size)
      case n: ErrorRTSCall => directCallWeight(n.invokeArgs.size)

      case _: CallTarget => 0

      case n: SymbolAddress =>
        val callTarget = n.uses.forall {
          case DirectCall(_) => true
          case _ => false
        }
        if (callTarget) 0 else ldrLiteralWeight

      case _: BuiltInTypeInfo | _: ThisTypeInfo | _: FieldAddr => ldrLiteralWeight

      case _: VirtualMethodAddr => getVirtualMethodAddrWeight

      case _: DelayedInstanceMethodVNum => getVirtualMethodAddrWeight

      case _: DelayedInstanceFieldAddress | _: DelayedMethodAddr => 0

      case _: ThinInstanceOf => 16

      case _: ThinCheckCast => 16 + directCallWeight(2) /* fatal error */

      case _: ThinNew => getPutWeight /* put TD */ /* TODO-THIN zeroing */

      case _: GetFlatThinCheck => 0

      case _: GetFlatThin => 2

      case _: BoxedValue => directCallWeight(1)

      case _: GetClass => directCallWeight(1)

      case _: DivisorCheck => divisorCheckWeight

      case _: EscapeWriteBarrier.Instance => directCallWeight(2)
      case _: EscapeWriteBarrier.Static => directCallWeight(1)

      case x: TDBarrier =>
        (if (x.argMayBeNull) intCmpWeight + jccWeight else 0) +
          getPutWeight * 2
          // TODO: either scale arg deprive and rich merge or wait until there will be no rich TDBarriers (I prefer to wait).

      case _: BeginLocalUnmovable => 0
      case _: EndLocalUnmovable => 0

      case _: AcquireRawData => 0
      case _: ReleaseRawData => 0

      case _: IncHeldLocks => execEnvWeight + getPutWeight * 2
      case _: DecHeldLocks => execEnvWeight + getPutWeight * 2

      case _: StackDescriptor => execEnvWeight + getPutWeight

      case _: (CopyStructure | InitStringRecord) => 0 // TODO: rewise

      case _: InstanceDescriptorBy => getPutWeight
      case _: ThisTypeInfoBy => getPutWeight * 2

      case _: ConvertDomain => directCallWeight(2)

      case _: DelayedGet | _: DelayedPut => 0 // TODO: rewise

      case op: CheckedOp => op.kind match {
        case CheckedOp.Kind.ADD =>
          checkedOpIntAddSubWeight + ifWeight + (
            if (op.signed) checkedOpIntLogicWeight * 4 + checkedOpIntConstWeight(rightNBits64(op.width.nbits - 1))
            else           checkedOpIntAddSubWeight + checkedOpIntConstWeight(ULONG_MAX)
            )
        case CheckedOp.Kind.SUB =>
          checkedOpIntAddSubWeight + ifWeight + (
            if (op.signed) checkedOpIntLogicWeight * 4 + checkedOpIntConstWeight(rightNBits64(op.width.nbits - 1))
            else           0
            )
        case CheckedOp.Kind.DIV =>
          checkedOpIntDivRemWeight(op) + (
            if (op.signed) divisorCheckWeight + ifWeight * 2 + checkedOpIntConstWeight(rightNBits64(op.width.nbits - 1)) * 2
            else           0
            )
        case CheckedOp.Kind.MUL =>
          checkedOpIntMulWeight + ifWeight + checkedOpIntHighMulWeight(n) + (
            if (op.signed) checkedOpShiftWeight
            else           checkedOpIntConstWeight(0)
            )
      }

      case _: Evacuate => directCallWeight(1)

      case _: MutFuncArgNode => 0
      case _: MutFunc.Combine => movWeight

      case _: LightInterfCastCBC => 0

      case _ => shouldNotReachHere(s"Weighing of node is not supported: ${n.name}")
    }
  }
}
