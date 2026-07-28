/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.classfile

import com.huawei.excelsior.jet.common.XString

/** This interface provides basic rules of name and signature equality.
  *
  * The rules of equality are defined as follows:
  * Two name and signature pairs `(n1,s1)` and `(n2,s2)` are equal iff
  * `n1 == n2` and either `s1 == null` or `s2 == null` or `s1 == s2`.
  *
  * @author liontiger
  */
trait NameAndSigComparable {
  def getXName: XString
  def getXSignature: XString
  def equalName(name: XString) = name == getXName
  def equalSignature(sig: XString): Boolean = {
    if (sig == null){
      true
    } else {
      val thisSig = getXSignature
      thisSig == null || thisSig == sig
    }
  }
  def sameNameAndSig(that: NameAndSigComparable) =
    that.equalName(getXName) && that.equalSignature(getXSignature)
}

object NameAndSigComparable {

  /** Creates instance of [[NameAndSigComparable]] from [[XString]] representations. */
  def of(name: XString, signature: XString): NameAndSigComparable = new NameAndSigComparable() {
    override def getXName: XString = name
    override def getXSignature: XString = signature
    override def equals(obj: Any): Boolean = obj match {
      case that: AnyRef if this eq that => true
      case that: NameAndSigComparable => this.sameNameAndSig(that)
      case _ => false
    }
    override def hashCode: Int = name.##
  }
}
