/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.common.Language.SCALA
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.TypeMetaInfoGenerator
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule.ClassSet
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, JUtilModule as ju, pcJCAModule as pcJCA, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule as jcp
import com.huawei.excelsior.jet.compiler.o2lib.u.ClassID.{XScalaAnyRef, XScalaString}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{CacheAPIModule, xcMakeModule, xiFilesModule, JStringsModule as js, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.StringValue
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.verifier.VerificationError
import com.huawei.excelsior.jet.compiler.verifier.VerificationError.ExceptionKind.VerifyError
import com.huawei.excelsior.jet.util.Closure
import com.huawei.excelsior.o2s.runtime.*

import scala.collection.mutable

object ExtraPassModule {

  private var toPreExtra = new ClassSet

  def markToPreExtra(clazz: pcO.Class): Unit = toPreExtra += clazz

  def checkOverrideFinal(m: pcO.Method, t: pcO.Class): Boolean = {
    if (m.isFinal) {
      val str = js.format("class %S overrides final method %S", t.name, m.getReadableName(need_class_name = true))
      t.setClassDefinitionError(VerifyError, str)
      return true
    }
    false
  }

  /**
    Checks, if super throws ClassDefinitionError on load,
    including if super has not some of its super classes or super interfaces
    (superabsent).
    Returns FALSE, if does not throw.
  */
  def checkSuperForClassDefError(tpe: pcO.Class): Boolean = {
    assert(!tpe.isUnloadableOnClassDefStage)
    def supers(t: pcO.Class) = {
      // Can't access supertypes of unloadable type.
      if (t.isUnloadableOnClassDefStage) Iterator.empty else Option(t.getSuperClassO2).iterator ++ t.getSuperInterfacesO2
    }
    Closure.withPostAction(mutable.HashSet.empty, Seq(tpe))(supers) { t =>
      // Absent super must be direct supertype, so we must traverse the whole chain `t <: ... <: s`
      // and set absent super for all intermediate types as well.
      // Therefore we do it in post-action (starting from the deepest supertypes).
      for (s <- supers(t)) {
        if (s.isUnloadableOnClassDefStage) {
          if (s.isUnavailable) {
            t.setAbsentSuper(s)
          } else {
            t.copyVerifyErrorFrom(s)
          }
        }
      }
    }

    tpe.isUnloadableOnClassDefStage
  }

  private def hasMain(class0: pcO.Class): Boolean =
    class0.declaredMethods.exists(_.isMainMethod)

  private def detectOverloadedMembers(members: Iterator[pcO.Member]): Unit = {
    val byNames = mutable.HashMap.empty[XString, pcO.Member]
    for (member <- members) {
      byNames.get(member.name) match {
        case Some(alreadyNamed) =>
          alreadyNamed.markAsOverloaded()
          member.markAsOverloaded()
        case _ =>
          byNames(member.name) = member
      }
    }
  }

  private def detectOverloaded(class0: pcO.Class): Unit = {
    detectOverloadedMembers(class0.declaredMethods)
    detectOverloadedMembers(class0.declaredFields)
  }

  private def preExtraMethods(clazz: pcO.Class): Unit = {
    if (!clazz.isUnloadable) {
      // Assign indices of CFuncWrappers corresponding to @c-annotated methods.
      // We do that during the ExtraPass to allow serialization into symfiles,
      // but only for classes which can be loaded.
      // TODO: consider moving it to new symlevel and calculate lazily.
      var cFuncWrapperIndex = 0
      for (method <- clazz.declaredMethods if method.isCAnnotated) {
        method.setCFuncWrapperIndex(cFuncWrapperIndex)
        cFuncWrapperIndex += 1
      }
    }
  }

  def preExtra(clazz: pcO.Class): Unit = {
    if (!toPreExtra(clazz)) {
      return
    }
    toPreExtra -= clazz

    pc.withModule(clazz) {
      detectOverloaded(clazz)
      preExtraMethods(clazz)
      if (hasMain(clazz)) {
        clazz.markAsHasMain()
      }

      if (!clazz.isClassDefinitionError) {
        if (clazz.hasAbsentSuper) {
          preExtra(clazz.getAbsentSuper)

        } else {
          def processSuper(s: pcO.Class): Unit = {
            if (clazz.isInActiveEnvironment && !s.isInActiveEnvironment) {
              env.errors.fault(ErrMsg991, clazz.name, s.name)
            }
            if (clazz.isAJManagedType && !s.isAJManagedType) {
              env.errors.fault(ErrMsg997, clazz.name, s.name)
            }
            preExtra(s)
          }

          if (clazz.getSuperClassO2 != null) {
            processSuper(clazz.getSuperClassO2)
          }
          clazz.getSuperInterfacesO2 foreach processSuper
        }
      }
    }
  }

  private def checkJCAClass(c: pcO.Class): Unit = {
    if (pcJCA.isTurboClinited(c)) {
      c.markAsTurboClinited()
    }
  }

  /*
    check the information from jca file
    and set the corresponding persistent jca
    attribute for the given method
  */
  private def checkJCAInformation(proc: pcO.Method): Unit = {
    if (pcJCA.isJCANoInline(proc)) {
      proc.setNeverInline()
    } else if (pcJCA.isJCAInline(proc)) {
      proc.setJCAInlined()
    }
    if (pcJCA.isJCANoLocalGCPoints(proc)) {
      proc.markAsNoLocalGCPoints()
    }
    if (pcJCA.isJCAInlineWithContextPointTest(proc)) {
      proc.setJCAInlineWithContextPointTest()
    }
    if (pcJCA.isJCAUnrollLoops(proc)) {
      proc.setJCAUnrollLoops()
    }
    if (pcJCA.isJCACodeAddrTarget(proc)) {
      proc.setCodeAddrUsed()
    }
  }

  private def checkMethods(c: pcO.Class): Unit = {
    for (method <- c.declaredMethods) {
      method.initJcaKnownSafeInfo()
      if (pcJCA.isExternal(method)) {
        method.markAsExternal()
        method.markAsExported() // workaround for 'noreexport' mode
      }
      checkJCAInformation(method)
    }
  }

  // inspect field, and if it has constant string value, mark the field with its constant string index
  private def checkStrings(c: pcO.Class): Unit = {
    val table = c.getStringTable
    assert(table != null)

    for (field <- c.declaredFields) {
      field match {
        case sf: pcO.StaticField if sf.value != null => sf.value match {
          case str: StringValue =>
            sf.setConstStringValue(table.getIndexByString(str.value))
            sf.value = null
          case _ =>
        }
        case _ =>
      }
    }
  }

  private def checkLayout(c: pcO.Class): Unit = {
    if (CacheAPIModule.isThisClass(c, XScalaString)) {
      assert(CacheAPIModule.isThisClass(c.getSuperClassO2, XScalaAnyRef))
      import SignatureType.*
      c.declaredFields.map(_.sig).toSeq match {
        case Seq(JavaArray(UInt16, 1), Int32) => // Correct layout, expected at runtime, otherwise compile-time failure.
      }
    }
  }

  def passModule(c: pcO.Class): Unit = {
    preExtra(c)
    c.getImport foreach preExtra
    pcO.initializeAJReplaced(c)
    for (m <- c.declaredMethods; t <- m.getThrows) {
      preExtra(t)
    }

    pc.currentModule = c.mno

    if (c.isVerifiable) {
      checkJCAClass(c)
      checkMethods(c)
      checkStrings(c)
      checkLayout(c)
    }
  }

  def exi(): Unit = {
    toPreExtra = null
  }
}
