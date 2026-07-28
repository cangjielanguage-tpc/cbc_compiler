/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.lambda

import xscala.util.Feature
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, MethodReference, Type}

abstract class LambdaTypeGenerator {

  /** Returns a lambda class (generates or finds already generated) by given constant pool InvokeDynamic entry
    * of the host class.
    */
  def getLambdaClass(hostClass: ClassType, cpInvokeDynamicEntry: Int): Type

  /** Return constructor of a generated lambda class of given constant pool InvokeDynamic entry of the host class
    * to create lambda instances.
    */
  def getLambdaConstructor(hostClass: ClassType, cpInvokeDynamicEntry: Int): MethodReference
}

object LambdaTypeGenerator extends Feature[LambdaTypeGenerator]
