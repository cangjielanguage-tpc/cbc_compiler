/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.Resource
import com.huawei.excelsior.jet.compiler.symlevel.{Field, Type as SymlevelType}
import com.huawei.excelsior.jet.compiler.util.{Maps, Sets}
import com.huawei.excelsior.jet.util.graph.Loop

/**
  * Created by conwor on 30.11.2015.
  */
//TODO: optimize
trait UniverseImplicitSetsAndMaps extends ImplicitSetsAndMaps { self: Universe =>
  implicit object NodeSetsAndMaps extends Sets.Default[Node] with Maps.Default[Node]
  implicit object EdgeSetsAndMaps extends Sets.Default[Edge] with Maps.Default[Edge]
  implicit object FloatingNodeSetsAndMaps extends Sets.Default[FloatingNode] with Maps.Default[FloatingNode]
  implicit object ControlNodeSetsAndMaps extends Sets.Default[ControlNode] with Maps.Default[ControlNode]
  implicit object UpperPointNodeSetsAndMaps extends Sets.Default[UpperPoint] with Maps.Default[UpperPoint]
  implicit object SpinalNodeSetsAndMaps extends Sets.Default[SpinalNode] with Maps.Default[SpinalNode]
  implicit object MemoryNodeSetsAndMaps extends Sets.Default[MemoryNode] with Maps.Default[MemoryNode]
  implicit object SpinalMemoryNodeSetsAndMaps extends Sets.Default[SpinalMemoryNode] with Maps.Default[SpinalMemoryNode]
  implicit object IdempotentSetsAndMaps extends Sets.Default[Idempotent] with Maps.Default[Idempotent]
  implicit object CallSetsAndMaps extends Sets.Default[Call] with Maps.Default[Call]
  implicit object GotoSetsAndMaps extends Sets.Default[Goto] with Maps.Default[Goto]
  implicit object IfSetsAndMaps extends Sets.Default[If] with Maps.Default[If]
  implicit object PhiSetsAndMaps extends Sets.Default[Phi] with Maps.Default[Phi]
  implicit object WeakCastSetsAndMaps extends Sets.Default[WeakCast] with Maps.Default[WeakCast]
  implicit object BBlockSetsAndMaps extends Sets.Default[BBlock] with Maps.Default[BBlock]
  implicit object XBlockSetsAndMaps extends Sets.Default[XBlock] with Maps.Default[XBlock]
  implicit object BlockSetsAndMaps extends Sets.Default[Block] with Maps.Default[Block]
  implicit object UseSitesSetsAndMaps extends Sets.Default[(Node, Int)] with Maps.Default[(Node, Int)]
  implicit object XPointsSetsAndMaps extends Sets.Default[XPoint] with Maps.Default[XPoint]
  implicit object MemBarrierSetsAndMaps extends Sets.Default[MemBarrier] with Maps.Default[MemBarrier]
  implicit object NullCheckSetsAndMaps extends Sets.Default[AbstractNullCheck] with Maps.Default[AbstractNullCheck]
  implicit object AddSetsAndMaps extends Sets.Default[Add] with Maps.Default[Add]
  implicit object EnrichSetsAndMaps extends Sets.Default[Enrich] with Maps.Default[Enrich]
  implicit object DepriveSetsAndMaps extends Sets.Default[Deprive] with Maps.Default[Deprive]
  implicit object BlocksLoopSetsAndMaps extends Sets.Default[Loop[Block]] with Maps.Default[Loop[Block]]
  implicit object VarSetsAndMaps extends Sets.Default[Var] with Maps.Default[Var]
  implicit object IfExitSetsAndMaps extends Sets.Default[If.Exit] with Maps.Default[If.Exit]
  implicit object SynchronizedRegionSetsAndMaps extends Sets.Default[SynchronizedRegion] with Maps.Default[SynchronizedRegion]
  implicit object MonitorEnterSetsAndMaps extends Sets.Default[MonitorEnter] with Maps.Default[MonitorEnter]
  implicit object GetInstanceFieldOperationSetsAndMaps extends Sets.Default[GetInstanceFieldOperation] with Maps.Default[GetInstanceFieldOperation]
  implicit object GetMemoryOperationSetsAndMaps extends Sets.Default[GetMemoryOperation] with Maps.Default[GetMemoryOperation]
  implicit object AnyMemoryAccessSetsAndMaps extends Sets.Default[AnyMemoryAccess] with Maps.Default[AnyMemoryAccess]
  implicit object CmpSetsAndMaps extends Sets.Default[Cmp] with Maps.Default[Cmp]
  implicit object ArrayIndexCheckSetsAndMaps extends Sets.Default[ArrayIndexCheck] with Maps.Default[ArrayIndexCheck]
  implicit object ControlNodeAndVarSetsAndMaps extends Sets.Default[(ControlNode, Var)] with Maps.Default[(ControlNode, Var)]
  implicit object BranchSetsAndMaps extends Sets.Default[Branch] with Maps.Default[Branch]
}

trait ImplicitSetsAndMaps {
  implicit object FieldSetsAndMaps extends Sets.Default[Field] with Maps.Default[Field]
  implicit object SymlevelTypeSetsAndMaps extends Sets.Default[SymlevelType] with Maps.Default[SymlevelType]
  implicit object XStringSetsAndMaps extends Sets.Default[XString] with Maps.Default[XString]
  implicit object ResourceSetsAndMaps extends Sets.Default[Resource] with Maps.Default[Resource]
}
