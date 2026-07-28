/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.options

import com.huawei.excelsior.jet.compiler.Environment
import com.huawei.excelsior.jet.compiler.options.BoolOption.SeaOfRTFields
import com.huawei.excelsior.jet.compiler.options.ConstRTFieldsValue.{initialize, initialized}
import com.huawei.excelsior.jet.compiler.options.StrOption.ConstRTFields

/** Categorization of constant run-time fields that are affected by [[BoolOption.SeaOfRTFields]].
  * Set of this enumeration is used as the value of [[StrOption.ConstRTFields]].
  *
  * @author liontiger
  */
enum ConstRTFieldsValue(private val description: String) {
  case UNCLASSIFIED extends ConstRTFieldsValue("Unclassified")

  case THIN_OBJ_TD extends ConstRTFieldsValue("ThinObj.td")

  case THIN_TD_COHEN       extends ConstRTFieldsValue("ThinTD.cohen[i]")
  case THIN_TD_COHEN_LEVEL extends ConstRTFieldsValue("ThinTD.cohenLevel")
  case THIN_TD_VMT         extends ConstRTFieldsValue("ThinTD.vmt[i]")

  case MANAGED_OBJ_TD           extends ConstRTFieldsValue("ManagedObj.td")
  case MANAGED_OBJ_TSWORD_CONST extends ConstRTFieldsValue("ManagedObj.tswordConst")

  case MANAGED_TD_IMT_SLOTS extends ConstRTFieldsValue("ManagedTD.imtSlots")
  case MANAGED_TD_VMT       extends ConstRTFieldsValue("ManagedTD.vmt[i]")

  case JAVA_TD_RTTI                extends ConstRTFieldsValue("JavaTD.rtti")
  case JAVA_TD_INLINED_COHEN       extends ConstRTFieldsValue("JavaTD.inlinedCohen[i]")
  case JAVA_TD_OUTLINED_COHEN      extends ConstRTFieldsValue("JavaTD.outlinedCohen")
  case JAVA_TD_OUTLINED_COHEN_DESC extends ConstRTFieldsValue("JavaTD.outlinedCohen[i]")
  case JAVA_TD_COHEN_LEVEL         extends ConstRTFieldsValue("JavaTD.cohenLevel")
  case JAVA_TD_IMT_SLOTS           extends ConstRTFieldsValue("JavaTD.imtSlots")
  case JAVA_TD_VMT                 extends ConstRTFieldsValue("JavaTD.vmt[i]")

  case JAVA_TD_ARRAY_BASE_TYPE extends ConstRTFieldsValue("JavaTD.arrayBaseType")

  case SCALA_TD_IMT_SLOTS extends ConstRTFieldsValue("ScalaTD.imtSlots")
  case SCALA_TD_VMT       extends ConstRTFieldsValue("ScalaTD.vmt[i]")

  case CANGJIE_TD_IMT_SLOTS extends ConstRTFieldsValue("CangjieTD.imtSlots")
  case CANGJIE_TD_VMT       extends ConstRTFieldsValue("CangjieTD.vmt[i]")

  case SCALA_TD_ARRAY_BASE_TYPE extends ConstRTFieldsValue("ScalaTD.arrayBaseType")

  // These fields aren't constant fields.
  // But they are constant from the point of view of thread executing with given EE.
  case EXEC_ENV_STACK_DESCRIPTOR extends ConstRTFieldsValue("ExecEnv.currentStackDescriptor")
  case EXEC_ENV_THREAD_ENV       extends ConstRTFieldsValue("ExecEnv.threadEnv")
  case EXEC_ENV_OWNER_IN_UNION   extends ConstRTFieldsValue("ExecEnv.ownerInUnion")

  case IMT extends ConstRTFieldsValue("IMT[i]")

  private var enabled = false // lazily initialized

  def isEnabled(env: Environment) = {
    if (!initialized) {
      initialize(env)
    }
    enabled
  }
}

object ConstRTFieldsValue {
  private val FIRST_ENABLED = THIN_OBJ_TD
  private val LAST_ENABLED  = EXEC_ENV_OWNER_IN_UNION

  def enabledByDefaultString: String = s"$FIRST_ENABLED-$LAST_ENABLED"

  private var initialized = false

  private def initialize(env: Environment): Unit = {
    assert(!initialized)

    if (env.enabled(SeaOfRTFields)) {
      try {
        val vals = values
        for (range <- env.valueOf(ConstRTFields).split(",")) {
          val args = range.split("-")

          if (args.length == 1) {
            valueOf(args(0)).enabled = true
          } else if (args.length == 2) {
            val start = valueOf(args(0)).ordinal
            val end = valueOf(args(1)).ordinal
            for (i <- start to end) {
              vals(i).enabled = true
            }
          } else {
            throw new NumberFormatException
          }
        }
        if (UNCLASSIFIED.enabled) {
          throw new NumberFormatException("UNCLASSIFIED cannot be enabled")
        }
      } catch {
        case e: Throwable =>
          env.forcePrintln()
          env.forcePrintln()
          printHelp(env)
          env.forcePrintln()
          throw e
      }
    }

    initialized = true
  }

  private def printHelp(env: Environment): Unit = {
    env.forcePrintln(s"${ConstRTFields.name} help:")
    val vals = values
    for (kind <- vals) {
      if (kind != UNCLASSIFIED) {
        env.forcePrintln(s"\t${kind.productPrefix}:\t${kind.description}")
      }
    }
    env.forcePrintln(s"Default value: $enabledByDefaultString")
    env.forcePrintln("Example values:")
    val name1 = vals(1).productPrefix
    val name2 = vals(2).productPrefix
    val name3 = vals(3).productPrefix
    val name4 = vals(4).productPrefix
    env.forcePrintln(s"\t$name1,$name2,$name3,$name4")
    env.forcePrintln(s"\t$name1-$name4")
    env.forcePrintln(s"\t$name1,$name2-$name3,$name4")
  }
}
