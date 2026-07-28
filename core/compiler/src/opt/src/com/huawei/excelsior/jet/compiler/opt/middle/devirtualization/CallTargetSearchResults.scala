/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.devirtualization

import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo.PGO
import com.huawei.excelsior.jet.compiler.types.References.Cone
import com.huawei.excelsior.jet.compiler.symlevel.MethodSearchError
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.types.Guards

object CallTargetSearchResults {

  sealed abstract class CallTargetSearchResult {
    def orElse(that: => CallTargetSearchResult): CallTargetSearchResult
  }


  /** Polymorphic call. */
  case object UnknownTarget extends CallTargetSearchResult {
    def orElse(that: => CallTargetSearchResult): CallTargetSearchResult = that
  }


  /** Unreachable or exceptional call. Following code is unreachable. */
  sealed abstract class NoTarget extends CallTargetSearchResult {
    def orElse(that: => CallTargetSearchResult): CallTargetSearchResult = this
  }

  case object UnreachableCall extends NoTarget

  case class ErroneousCall(error: MethodSearchError) extends NoTarget


  /** Probably unreachable or exceptional call. This code is cold. */
  case object ProbableNoTarget extends CallTargetSearchResult {
    def orElse(that: => CallTargetSearchResult): CallTargetSearchResult = if (that == UnknownTarget) this else that
  }


  sealed abstract class OneOrMultipleTargets extends CallTargetSearchResult

  sealed abstract class OneTarget extends OneOrMultipleTargets {
    def target: Method

    def orElse(that: => CallTargetSearchResult): CallTargetSearchResult = this
  }


  /** One direct target of virtual call. */
  case class OneDirectTarget(target: Method) extends OneTarget


  sealed trait OneOrMultipleGuardedTargets extends OneOrMultipleTargets {
    def guardedTargets: Seq[(Method, Guards.Guard)]
    def rcvType: Cone
    def info: TauInfo
  }


  case class OneGuardedTarget(target: Method, guard: Guards.Guard, info: TauInfo, rcvType: Cone) extends OneTarget with OneOrMultipleGuardedTargets {
    require(!target.getDeclaringClass.isThinClass)

    def guardedTargets: Seq[(Method, Guards.Guard)] = Seq((target, guard))
  }

  case class MultipleGuardedTargets(guardedTargets: Seq[(Method, Guards.Guard)], info: PGO, rcvType: Cone) extends OneOrMultipleGuardedTargets {
    require(guardedTargets.size > 1)

    def orElse(that: => CallTargetSearchResult): CallTargetSearchResult = this
  }

}
