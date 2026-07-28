/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.jprof.fields

import com.huawei.excelsior.jet.compiler.jprof.JProfManager
import com.huawei.excelsior.common.CodeHelpers.notImplemented
import com.huawei.excelsior.jet.compiler.opt.jprof.Profile.env
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.representation.JProf
import com.huawei.excelsior.jet.compiler.opt.jprof.fields.representation.Field
import com.huawei.excelsior.jet.compiler.symlevel.{Field => SymField}
import com.huawei.excelsior.jet.jprof.JProfFormat.KeyName.{CLASS_NAME, CLASSLOADER_SID, FIELD_NAME, FIELD_SIG, FIELD_VAL, RT_ANON}
import com.huawei.excelsior.jet.jprof.JProfFormat.{EntryType, ObjType, SectionType}

/** Compiler interface to the data gathered by fields' values profiler.
  *
  * @author ijorch
  */
private[jprof] object Fields {

  def valueOfStaticFinalPrimitive(field: SymField): Option[Number] = primValues.get(Field.fromSymlevel(field))

  /** Reports if fields recorded in .jprof became inconsistent, but only if `-decor=*w*` */
  def checkJProfDataSanity(): Unit = {
    val bad = primValues.keysIterator filterNot { x =>
      val f = x.toSymlevel
      f != null && f.isStatic && f.isFinal && f.getType.isPrimitive
    }
    if (bad.nonEmpty) {
      env.reportWarning("\nJProf Warning: following fields expected to be static final primitive (or were not found):\n\t" +
        bad.map(_.getFullName).toBuffer.sorted.mkString("\n\t")
      )
    }
  }

  private lazy val primValues: Map[Field, Number] = {
    val sections = JProfManager.main.getSectionsByType(SectionType.FIELDS_DATA)

    if (sections.size > 1) {
      notImplemented("Multiple sections of final fields dump")
    } else if (sections.isEmpty) {
      Map.empty
    } else {
      for {
        entry <- sections.head.entries
        clsObj :: fObjs = entry.objs.toList ensuring (entry.tpe == EntryType.FIELDS_ENTRY)
        attrs = JProf.collectAttributes(clsObj.attributes) ensuring (clsObj.tpe == ObjType.FIELDS_CLASS)
        isLambda = attrs.getOrElse(RT_ANON, false)
        clid = attrs.getOrElse[String](CLASSLOADER_SID, null)
        clsName = attrs[String](CLASS_NAME)
        fObj <- fObjs
        fAttrs = JProf.collectAttributes(fObj.attributes) ensuring (fObj.tpe == ObjType.FIELDS_STATIC_FINAL_PRIM)
        fName = fAttrs[String](FIELD_NAME)
        fSig = fAttrs[String](FIELD_SIG)
        fVal = fAttrs[Long](FIELD_VAL)

      } yield Field(isLambda, clid, clsName, fName, fSig) -> value(fSig, fVal)
    }.toMap
  }

  private def value(fSig: String, fVal: Long): Number = fSig match {
    case "Z" => fVal.toInt
    case "B" => fVal.toInt
    case "C" => fVal.toInt
    case "S" => fVal.toInt
    case "I" => fVal.toInt
    case "J" => fVal
    case "F" => java.lang.Float.intBitsToFloat(fVal.toInt)
    case "D" => java.lang.Double.longBitsToDouble(fVal)
  }
}
