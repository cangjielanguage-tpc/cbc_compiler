/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.compiler.abi.FrameProperties
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, Method}

/** This class corresponds to the object-code outcome of compilation of the `method`.
  * Note, that the [[Method]] itself represents a source-code-level entity.
  *
  * Sub-classes of this class can provide compiler with various info about the `method`.
  * E.g. it can be used for specialization of that `method` as a separate version.
  *
  * Thus, for a single `method` there might exist several different code units generated in different contexts.
  *
  * @author ijorch
  */
abstract class CodeUnit(val method: Method) {

  def isVersionedMethod: Boolean

  /** Type of method's receiver parameter, or `null` if method doesn't have a receiver. */
  def getReceiverType(implicit tp: TypeProvider): ClassType

  /** Class containing all code and data generated for this code unit
    * (hence it will be accessible through the class' TD).
    */
  def getHostingClass: ClassType

  /** Index at which this code unit is present in its [[getHostingClass hosting class]]. */
  def getHostedIndex: Int

  /** Symbol associated with code segment generated for this code unit. */
  def getSymbol: Symbol

  def getFrameProperties: FrameProperties

  def getUniqueNumberInClass: Int

  /** Name of `method`, followed by description if `this.isVersionedMethod`. */
  def getName: String

  override final def toString = s"${getHostingClass.getName}.${this.getName}${method.getSignature.toJETSignature}"

  def equals(o: Any): Boolean

  def hashCode: Int
}

object CodeUnit {

  def of(_method: Method): CodeUnit = new CodeUnit(_method) {

    override def isVersionedMethod: Boolean = false

    override def getReceiverType(implicit tp: TypeProvider) = {
      val mt = method.getMethodType
      if (method.hasReceiverParameter) asClassType(mt.parameterType(mt.getReceiverArgIdx))
      else null
    }

    override def getHostingClass: ClassType = method.getDeclaringClass

    override def getHostedIndex: Int = method.getHostedIndex

    override def getSymbol: Symbol = method

    override def getFrameProperties: FrameProperties = method

    override def getUniqueNumberInClass: Int = method.getUniqueNumberInClass

    override def getName: String = method.getName

    override def equals(o: Any): Boolean = o match {
      case that: AnyRef if this eq that => true
      case null => false
      case that if this.getClass != that.getClass => false
      case codeUnit: CodeUnit => method.equals(codeUnit.method)
    }

    override def hashCode: Int = method.hashCode
  }
}
