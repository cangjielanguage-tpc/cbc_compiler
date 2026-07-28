/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.fields.representation

import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.symlevel.{JETSignatureParser, SignatureType, Field as SymField}
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env

/** Text representation of a field, doesn't depend on sym-level.
  *
  * @author ijorch
  */
private[fields] case class Field(isLambda: Boolean, classLoaderSID: String, host: String, name: String, tpe: String) {
  def toSymlevel = {
    val cls = env.getTypeProvider.getClassTypeByNameAndClassLoaderSID(host, classLoaderSID)
    val sig = try {
      JETSignatureParser.parse(tpe).asInstanceOf[SignatureType]
    } catch {
      case e: JETSignatureParser.Error => null
    }
    if (cls == null) null else cls.findDeclaredFieldOrNull(xstr(name), sig)
  }
  def getFullName = s"$host.$name"
}

private[fields] object Field {
  def fromSymlevel(sf: SymField): Field = {
    Field(sf.getDeclaringClass.isAnonymous, sf.getDeclaringClass.getClassLoaderSID, sf.getDeclaringClass.getName, sf.getName, sf.getSignature.toJETSignature)
  }
}
