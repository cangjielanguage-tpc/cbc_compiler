/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.options.NumOption.{SwitchMaxPlainChecks, SwitchMinTableJumpCases, SwitchMinTableJumpDensity}
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.types.Guards.{Guard, TypeGuard}
import com.huawei.excelsior.jet.util.ScalaCollections

import collection.mutable.ListBuffer

/**
 * Lowering of Switch operations.
 *
 * @author alexm
 */
private[lowering] trait Switches extends Toolbox with TypeChecks { self: Universe =>

  /**
   * Minimum density of cases to generate switch as table jump.
   */
  private val minTableJumpDensity = env.valueOf(SwitchMinTableJumpDensity).toDouble / 100.0

  /**
   * Minimum number of cases to generate switch as table jump.
   */
  private val minTableJumpCases = env.valueOf(SwitchMinTableJumpCases)

  /**
   * Max number of plain cases checks in a row.
   */
  private val maxPlainChecks = env.valueOf(SwitchMaxPlainChecks)


  /** Lowers Switch operation. */
  private[lowering] def lowerSwitch(switch: Switch): Unit = {
    def getMethod(node: Node) = Position.inlineContext(node.pos).map(_.method).getOrElse(rootMethod)

    val switchMethod = getMethod(switch)

    val genSwitchTable = switchMethod.isGenTableSwitch
    assert(!genSwitchTable || all[Switch].count(s => getMethod(s) == switchMethod) == 1, s"${all[Switch].filter(s => getMethod(s) == switchMethod).toSeq}")

    val cases = switch.cases.sorted
    val hotCases = env.getHotSwitchCases(rootMethod, switch.cases.length).toSeq

    // Generate "hot" and "cold" parts if specified in JCAdvice.
    // All the other switches are lowered as usual.
    val coldCases = if (hotCases.nonEmpty) {
      assert(hotCases.diff(cases).isEmpty)

      val hotChecksExit = genPlainCaseChecks(switch, hotCases)
      continue(hotChecksExit)

      ColdCodeMarker()

      cases.diff(hotCases)
    } else {
      cases
    }

    val coldDefaults =
      if (coldCases.isEmpty) {
        Seq(Goto())
      } else if (targetArch != CBC
        && (((coldCases.size >= minTableJumpCases) && (density(coldCases) >= minTableJumpDensity))
          || genSwitchTable)) {
        genTableJump(switch, coldCases)
      } else {
        genBisectedIfs(switch, coldCases)
      }

    val unifiedDefault = coldDefaults match {
      case Seq(df) => df
      case _ => continue(coldDefaults: _*); Goto()
    }

    switch.defaultExit.replaceUsesBy(unifiedDefault)

    assert (switch.exits forall (_.uses.isEmpty))
    decommit(switch)
  }

  /** Returns density of label values.
    *
    * @param labels Sorted sequence of label values.
    * @return Density of values.
    */
  private def density(labels: Seq[Int]): Double = {
    val num = labels.size
    assert (num >= 1)

    val minValue = labels.head
    val maxValue = labels.last

    val d = num.toDouble / (maxValue.toDouble - minValue.toDouble + 1.0)
    assert ((0.0 < d) && (d <= 1.0))

    d
  }

  /** Generates range check and TableJump.
    * Returns out edges for default case.
    */
  private def genTableJump(switch: Switch, labels: Seq[Int]): Seq[Branch.Exit] = {
    val valueBase = labels.head

    val selector = if (valueBase != 0) Sub(switch.selector, IConst(valueBase)) else switch.selector
    val tableSize = labels.last - valueBase + 1
    val tableSym = symbolLinker.makeDataSymbol()

    val checkRange = If(Cmp(IntType, Condition.ULT)(selector, IConst(tableSize)))

    continue(checkRange.trueExit)
    val jump = TableJump(tableSize, tableSym)(selector, SymbolAddress(tableSym))
    val defaults = new ListBuffer[Branch.Exit]

    for (i <- 0 until tableSize) {
      val out = jump.exits(i)
      val swCase = switch.outCtrl(valueBase + i)
      // In case some (hot) cases are generated already,
      // some uses might be empty
      if (swCase.isDefault || swCase.uses.isEmpty) {
        defaults += out
      } else {
        swCase.replaceUsesBy(out)
      }
    }

    (checkRange.falseExit +: defaults).toList
  }

  /** Generates sequence of Branches to check given labels.
    * If number of labels is not small makes bisection of labels range.
    * Returns out edges for default case.
    */
  private def genBisectedIfs(switch: Switch, labels: Seq[Int]): Seq[Branch.Exit] = {
    val n = labels.length
    assert(n > 0)
    val selector = switch.selector

    if (n > maxPlainChecks) {
      // Make bisection
      val mid = n/2
      val midVal = labels(mid)

      val branch = If(Cmp(IntType, Condition.LT)(selector, IConst(midVal)))

      continue(branch.trueExit)
      val df1 = genBisectedIfs(switch, labels.slice(0, mid))

      continue(branch.falseExit)
      val df2 = genBisectedIfs(switch, labels.slice(mid, n))

      (df1 ++ df2)

    } else {
      Seq(genPlainCaseChecks[Int](switch, labels))
    }
  }

  private def genPlainCaseChecks[C](switch: AnySwitch[C], cases: Seq[C]) = {
    var exit: Branch.Exit = null
    val last: C = cases.last

    for (label <- cases) {
      val caseCheck = switch.outCtrl(label) match {
        case ex: Switch.Exit => ex.genCaseCheck()
        case tse: TauSwitch.Exit => genTypeGuardTest(tse.caseValue.asInstanceOf[TypeGuard], switch.selector)
        // following extraction ruins generic erasure in scalac (see https://github.com/scala/bug/issues/12182)
        //case TauSwitch.Exit(Some(tg: TypeGuard)) => genTypeGuardTest(tg, switch.selector)
        case ex => shouldNotReachHere(ex)
      }
      val branch = If(caseCheck)
      switch.outCtrl(label).replaceUsesBy(branch.trueExit)

      exit = branch.falseExit
      if (label != last) continue(exit)
    }

    exit
  }

  private[lowering] def lowerTauSwitch(tauSwitch: TauSwitch): Unit = {
    val default = genPlainCaseChecks[Guard](tauSwitch, tauSwitch.cases)

    tauSwitch.defaultExit.replaceUsesBy(default)

    assert(tauSwitch.exits forall (_.uses.isEmpty))
    decommit(tauSwitch)
  }

}
