/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind.*

import scala.collection.mutable

/**
  This module provides usefull subclasses of
  Option and Equation classes
*/
object xOptionsModule {

  /** This option is set/unset bit in config tags depending on a value
  */
  class ConfigBitOption(name: String, val bit: env.CompilerOption, checked: SmartKind = Checked)
    extends env.Option(js.newJString(name), checked) {

    private[xOptionsModule] val config = env.config

    assert(bit.toByte >= 0.toByte)
    assert(bit.toByte.toShort < 31.toShort)
    // check that for the new option, the bit is not set yet
    // (there is no two options that share the same bit)
    assert(!(config.tags contains bit))

    // bit should be set on any value set, so we overrride setValue instead of verify 
    override def setValue(v: env.Value): Unit = {
      super.setValue(v)
      if (this.getBooleanValue) {
        this.config.tags += bit
      } else {
        this.config.tags -= bit
      }
    }

  }

  //--------------------------------------------------------------------- 


  /** This equation is immediately HALTs executaion of the compiler if set to non NIL */
  class DeniedEquation(name: String, err: ErrMsg, errArgs: Any*) extends env.Equation(js.newJString(name), Checked) {

    override def verify(): Unit = {
      env.errors.fault(err, errArgs:_*)
    }

  }

  /** This equation allows only limited set of values
   * (else HALTs execution)
   */
  class RestrictedEquation(name: String, err: ErrMsg, checked: SmartKind = Checked)
                          (_permittedValues: String*)
    extends env.Equation(js.newJString(name), checked) {

    // error to show, if the value is not in the list
    // of permitted values

    private[xOptionsModule] val permittedValues = mutable.HashSet.from[XString](_permittedValues.map(js.newJString))

    override def verify(): Unit = {
      val value = getStringValue
      if (!permittedValues.contains(value)) {
        env.errors.fault(err, value)
      }
    }

    // Values of Restricted equations are case insensitsive 
    override def preprocessValue(value: XString): XString = value.toUpperCase

  }

  /** Equation that hold file name that is stored in internal format
  */
  class FileEquation(name: String, checked: SmartKind = Checked) extends env.Equation(js.newJString(name), checked) {

    override def preprocessValue(value: XString): XString = FS.HOST.fromPlatform(value)

  }
}
