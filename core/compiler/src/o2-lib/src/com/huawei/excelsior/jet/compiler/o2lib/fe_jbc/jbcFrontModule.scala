/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe_jbc

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.Stage
import com.huawei.excelsior.jet.compiler.lambda.LambdaTypeGenerator
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, ExtraPassModule as ep, NumerateModule as Numerate, pcJCAModule as pcJCA, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule as jcp
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule.MethodInfo
import com.huawei.excelsior.jet.compiler.o2lib.u.ClassID.ScalaRefType
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{CacheAPIModule, ClassID, ReplacementLibrary, JStringsModule as js, PropertiesModule as Properties, xOptionsModule as opt, xcModesModule as xcModes, xiEnvModule as env, xiFilesModule as xfs, xmErrorsModule as xmErrors, xmZipModule as xmZip}
import com.huawei.excelsior.jet.compiler.o2lib.xjRTSModule as rts
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.BoolOption
import com.huawei.excelsior.jet.compiler.options.BoolOption.{AddImportFromConstantPool, BuildXKRN, XScala}
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind.*
import com.huawei.excelsior.jet.compiler.options.StrOption.NoneLangPackRTClasses
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.*
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.*
import com.huawei.excelsior.jet.compiler.symlevel.VersionedMarker
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{JavaVerifier, LightweightEnvironment, O2TypeProvider as TypeProvider}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import com.huawei.excelsior.jet.compiler.verifier.VerificationError
import com.huawei.excelsior.jet.compiler.verifier.VerificationError.ErrorKind.VERIFY_ERROR
import com.huawei.excelsior.jet.compiler.verifier.VerificationError.ExceptionKind.*
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.MathUtils.makeLong
import xscala.util.{Set32, UByte, UShort}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** This module retrieves symbolic information from bytecode and constructs tree of objects */
object jbcFrontModule {

  type accfKind = UByte
  private val accf_field: accfKind = UByte(0)
  private val accf_method: accfKind = UByte(1)
  private val accf_srctags: accfKind = UByte(2)
  private val accf_classtags: accfKind = UByte(3)

  val makeClassImpl: XString => pcO.Class = { name0 =>
    val name = JBCPreprocessor.movedScalaClassName(name0)
    val clazz = O2Env.stage(Stage.JBCFrontMakeClassImpl) { env.loadType(name) }
    if (clazz != null) {
      clazz
    } else {
      if (pcO.classAbsenceErr) {
        env.errors.fault(ErrMsg402, env.info.module.name, name)
      } else {
        pcO.makeAbsentClass(pcNames.newAbsentClassName(name), importedFromSym = false)
      }
    }
  }

  private val jstrUnsupported = js.internJString("Unsupported")
  var curMn: pc.MNO = _
  private var noJavaLanguagePack: Boolean = _
  //--------------------------------------------------------------------------
  private var rtJarLocaleClasesPrefix: XString = _
  private var noreplacements: Boolean = _
  private var noLocalVarsToMethParsConversion: Boolean = _

  private def maskModifiers(kind: accfKind, accf: Set32): Set32 = {
    kind match {
      case `accf_method` =>
        accf & rts.JMDF_METHOD_MASK
      case `accf_field` =>
        accf & rts.JMDF_FIELD_MASK
      case `accf_srctags` =>
        accf & rts.JMDF_TYPE_MASK
      case `accf_classtags` =>
        assert((accf & rts.ACC_FLAGS_MASK) == accf)
        accf & rts.ACC_FLAGS_MASK
    }
  }

  //--------------------------- VCF Exclude List -----------------------------

  private lazy val VCFExcludedClasses: collection.Set[XString] = {
    val res = mutable.HashSet.empty[XString]
    var excludeStr = env.config.equation("genvcfExcludeClasses")
    if (excludeStr != null) {
      res ++= env.splitString(excludeStr, ';')
    }
    excludeStr = Properties.getJCProperty("genvcfExcludeClasses")
    if (excludeStr != null) {
      res ++= env.splitString(excludeStr, ';')
    }
    res
  }

  private lazy val VCFExcludedJars: collection.Set[XString] = {
    val res = mutable.HashSet.empty[XString]
    var excludeStr = env.config.equation("genvcfExcludeJars")
    if (excludeStr != null) {
      res ++= env.splitString(excludeStr, ';')
    }
    excludeStr = Properties.getJCProperty("genvcfExcludeJars")
    if (excludeStr != null) {
      res ++= env.splitString(excludeStr, ';')
    }
    res
  }

  def isInVCFExcluded(classname: XString): Boolean = VCFExcludedClasses.contains(classname)

  /*
    Checks if VCF should not be generated for the given class.
  */
  private def isVCFExcluded(cls: pcO.Class, fd: xfs.FileDescriptor): Boolean = {
    assert(fd != null)
    if (pcNames.isLambdaClassName(cls.nameObj)) {
      return true
    }

    if (isInVCFExcluded(cls.name)) {
      return true
    }

    if (VCFExcludedJars.nonEmpty) fd match {
      case fd: xmZip.FileDescriptor =>
        val fname = FS.cutPath(FS.HOST.fromPlatform(fd.zname))
        if (VCFExcludedJars.contains(fname)) {
          return true
        }
      case _ =>
    }

    false
  }

  private lazy val hideDepecatedInCPMode: collection.Set[XString] = {
    env.convValueToSet(Properties.getJCProperty("hideDeprecatedInCpMode"))
  }

  private lazy val systemClassExcludes: collection.Set[XString] = {
    val systemClassExcludeList = env.config.equation("systemClassesExcludeList")
    if (systemClassExcludeList != null) {
      env.convValueToSet(systemClassExcludeList)
    } else Set.empty
  }

  def makeClassHead(this0: pcNames.NAME, srctags: Set32, classtags: Set32): pcO.Class = {
    assert(!xcModes.workerMode) // no class-files parsing should be at worker mode
    env.info.module = this0

    var modifiers = maskModifiers(accf_srctags, srctags).toSet32
    val accflags = maskModifiers(accf_classtags, classtags).toSet32

    if (!systemClassExcludes.contains(this0.name)) {
      if (env.config.option("SystemClasses")) {
        modifiers += pcO.xot_systemclass
      } else if (env.config.option("ExtensionClassLoader")) {
        modifiers += pcO.xot_extension_classloader
      }
      if (env.config.option("RuntimeClasses")) {
        modifiers += pcO.xot_jet_runtime
      }
    }

    if (env.config.option("OptimizeAggressively")) {
      modifiers += pcO.xot_optimized_aggressively
    }

    if (env.config.option("localecomponent") || rtJarLocaleClasesPrefix != null && this0.name.startsWith(rtJarLocaleClasesPrefix, 0)) {
      modifiers += pcO.xot_locale
    }

    val clazz = pcO.makeClassHeadTags(this0, modifiers, accflags)

    if (TypeProvider.isJavaLangObject(clazz) || TypeProvider.isAJObject(clazz) || TypeProvider.isThinType(clazz) || TypeProvider.isAJCompoundType(clazz)) {
      clazz.inclModifier(pcO.xot_hierarchy_root)
    }

    if (TypeProvider.isAJArray(clazz)) {
      clazz.markAsAJArray()
    }

    if (!clazz.isAJArray) {
      JavaVerifier { _.markToVerify(clazz) }
    }

    if (O2Env.env.enabled(BuildXKRN)) {
      clazz.markAsPlatformClass()
    }

    if (hideDepecatedInCPMode.contains(this0.name)) {
      clazz.markAsHideDeprecatedInCPMode()
    }

    env.info.module = this0

    clazz
  }

  def makeErrorClass(this0: pcNames.NAME, noclassdef: Boolean, unsupported: Boolean, verifyerror: Boolean): pcO.Class = {
    val t = makeClassHead(this0, Set32.empty, Set32.empty)
    pc.currentModule = t.mno
    if (noclassdef) {
      t.setClassDefinitionError(NoClassDefFoundError, t.name)
    } else if (unsupported) {
      t.setClassDefinitionError(UnsupportedClassVersionError, t.name)
    } else if (verifyerror) {
      t.setClassDefinitionError(VerifyError, if (jcp.error != null) jcp.error else js.newJString("error during class loading"))
    } else {
      t.setClassDefinitionError(ClassFormatError, t.name)
    }
    t
  }

  private def addClassFileImport(curClass: pcO.Class, s: XString): Unit = {
    if (s != curClass.name) {
      curClass.addImport(pcO.makeClass(s))
    }
  }

  private def addClassFileImportFromSig(curClass: pcO.Class, sig: XString): Unit = {
    var pos = sig.indexOf('L')
    while (pos != -1) {
      val end = sig.indexOf(';', pos + 1)
      val s = js.internSubstring(sig, pos + 1, end)
      addClassFileImport(curClass, s)
      pos = sig.indexOf('L', end + 1)
    }
  }

  private def makeClassFileImport(curClass: pcO.Class, C: jcp.PtrClassInfo): Unit = {
    if (O2Env.env.enabled(AddImportFromConstantPool)) {
      val pool = C.constantPool
      for (constant <- C.constants) {
        constant.constantType match {
          case jcp.TagClass =>
            val s = pool(constant.indexName.toInt).bufferPtr
            if (s.charAt(0) != '[') {
              addClassFileImport(curClass, s)
            } else {
              addClassFileImportFromSig(curClass, s)
            }
          case jcp.TagField | jcp.TagMethod | jcp.TagIMethod =>
            val s = pool(pool(constant.indexName.toInt).index.toInt).bufferPtr
            addClassFileImportFromSig(curClass, s)
          case _ =>
        }
      }
    }

    for (field <- C.fields) {
      val sig = jcp.getString(C, field.signatureIndex.toInt)
      addClassFileImportFromSig(curClass, sig)
    }
    for (method <- C.methods) {
      val sig = jcp.getString(C, method.signatureIndex.toInt)
      addClassFileImportFromSig(curClass, sig)
    }
  }

  def getSourceAccessFlags(C: jcp.PtrClassInfo, this0: XString): Set32 = {
    for (a <- jcp.getAttribute(C, C.attribute, C.attributeCount.toInt, jcp.jstrInnerClasses) if a.innerClasses != null) {
      val outer = if (a.innerClasses(0).outerClassInfoIndex != UShort(0)) {
        jcp.getString(C, C.constantPool(a.innerClasses(0).outerClassInfoIndex.toInt).indexName.toInt)
      } else {
        null
      }
      if (outer == null || outer != this0) {
        for (ic <- a.innerClasses) {
          val inner = jcp.getString(C, C.constantPool(ic.innerClassInfoIndex.toInt).indexName.toInt)
          if (inner == this0) {
            return ic.innerClassAccessFlags
          }
        }
      }
    }

    C.accessFlag
  }

  private def setEnclosingMethod(C: jcp.PtrClassInfo, curClass: pcO.Class, a: jcp.AttributeInfo): Unit = {
    var sig: XString = null
    var name: XString = null

    if (a.index == UShort(0)) {
      return
    }
    val clazz = jcp.getString(C, C.constantPool(a.index.toInt).indexName.toInt)
    if (a.index2 != UShort(0)) {
      name = jcp.getString(C, C.constantPool(a.index2.toInt).indexName.toInt)
      sig = jcp.getString(C, C.constantPool(a.index2.toInt).index.toInt)
    } else {
      name = null
      sig = null
    }
    val enClass = pcO.makeClass(clazz)
    assert(enClass != null)
    curClass.setEnclosingMethod(enClass, name, sig)
  }

  private def getValueByName(ann: jcp.PtrAnnotation, pairName: XString): jcp.PtrElementValue = {
    if (ann.pairs != null) {
      for (j <- ann.pairs.indices) {
        if (ann.pairs(j).name.equals(pairName)) {
          return ann.pairs(j).value
        }
      }
    }
    null
  }

  private def checkExternal(ann: jcp.PtrAnnotation, member: pcO.Member): Unit = {
    if (ann.type0.equals(jcp.jstrAjExternal)) {
      val value = getValueByName(ann, jcp.jstrAjExternalName)
      if (value != null && value.isInstanceOf[jcp.PtrStringElementValue]) {
        member.markAsExternal(value.asInstanceOf[jcp.PtrStringElementValue].value)
      } else {
        member.markAsExternal(member.name)
      }
    }
  }

  private def checkExport(ann: jcp.PtrAnnotation, member: pcO.Member): Unit = {
    if (ann.type0.equals(jcp.jstrAjExport)) {
      val value = getValueByName(ann, jcp.jstrAjExportId)
      if (value != null && value.isInstanceOf[jcp.PtrStringElementValue]) {
        member.markAsExported(value.asInstanceOf[jcp.PtrStringElementValue].value)
      } else {
        member.markAsExported()
      }
    }
  }

  private def checkData(ann: jcp.PtrAnnotation, member: pcO.StaticField): Unit = {
    if (ann.type0.equals(jcp.jstrAjData)) {
      val value = getValueByName(ann, jcp.jstrAjDataData)
      if (value != null && value.isInstanceOf[jcp.PtrStringElementValue]) {
        member.setDataInfo(value.asInstanceOf[jcp.PtrStringElementValue].value)
      } 
    }
  }

  private lazy val environmentsSet: collection.Set[XString] = {
    val environments = env.config.equation("environments")
    if (environments != null) {
      env.convValueToSet(environments)
    } else Set.empty
  }

  private def isActiveEnvironment(environment: XString): Boolean = environmentsSet.contains(environment)

  private def isAnnotInActiveEnvironment(ann: jcp.PtrAnnotation, annParam: XString): Boolean = {
    val value = getValueByName(ann, annParam)
    if (value == null) {
      true
    } else {
      assert(value.isInstanceOf[jcp.PtrStringElementValue])
      val env = value.asInstanceOf[jcp.PtrStringElementValue].value
      isActiveEnvironment(env)
    }
  }

  private def stringAttrSafe (ann: jcp.PtrAnnotation, name: XString): XString = {
    val value = getValueByName(ann, name).asInstanceOf[JavaClassParserModule.PtrStringElementValue]
    if (value != null) value.value else XString.empty
  }
  private def stringAttr (ann: jcp.PtrAnnotation, name: XString): XString                    = getValueByName(ann, name).asInstanceOf[jcp.PtrStringElementValue]  .value
  private def enumAttr   (ann: jcp.PtrAnnotation, name: XString): XString                    = getValueByName(ann, name).asInstanceOf[jcp.PtrEnumElementValue]    .constName
  private def arrayAttr  (ann: jcp.PtrAnnotation, name: XString): Array[jcp.PtrElementValue] = getValueByName(ann, name).asInstanceOf[jcp.PtrArrayElementValue]   .value
  private def intAttr    (ann: jcp.PtrAnnotation, name: XString): Int                        = getValueByName(ann, name).asInstanceOf[jcp.PtrIntElementValue]     .value

  private def processAjAnnotationForMethod(ann: jcp.PtrAnnotation, clazz: pcO.Class, m: pcO.Method): Unit = {
    val atype = ann.type0

    checkExternal(ann, m)
    checkExport(ann, m)

    if (atype.equals(jcp.jstrAjIntrinsic)) {

      var ajIntrinsicWithoutBody = false
      if (m.getDeclaringClass.isValueClass && m.name.startsWith(jcp.jstrAjProcedureTypeInvokePrefix, 0)) {
        ajIntrinsicWithoutBody = true
        m.markAsAjIndirectCall()
      }

      if (m.getDeclaringClass.isThinClass && m.name.startsWith(jcp.jstrAjThinTypeUncheckedCastPrefix, 0)) {
        ajIntrinsicWithoutBody = true
        m.markAsThinUncheckedCast()
      }

      if (m.getDeclaringClass.isThinClass && m.name.startsWith(jcp.jstrAjThinTypeGetFlatPrefix, 0)) {
        ajIntrinsicWithoutBody = true
        m.markAsGetFlatThinIntrinsic()
      }

      if (!ajIntrinsicWithoutBody) {
        if (methodByO2Object(m).getIntrinsicType != null) {
          ajIntrinsicWithoutBody = true

        } else if (methodByO2Object(m).getIntrinsicWithBodyType != null) {
          ajIntrinsicWithoutBody = false

        } else if (m.isDeclaredNative) {
          // aj-javac level intrinsic
          ajIntrinsicWithoutBody = true

        } else if (m.name == js.jstrInit) {
          // aj-javac level intrinsic constructor, no uses allowed
          ajIntrinsicWithoutBody = true

        } else {
          env.info.forcePrint("Bad intrinsic: %S from %S\\n", m.name, m.getDeclaringClass.name)
          shouldNotReachHere("there must not be other intrinsics")
        }
      }

      if (ajIntrinsicWithoutBody) {
        m.markAsNoCodeGen()
      }

    } else if (atype.equals(jcp.jstrAjInline)) {
      m.setAJInline()
      val value = getValueByName(ann, jcp.jstrAjInlineForced)
      assert(value != null && value.isInstanceOf[jcp.PtrIntElementValue])
      value.asInstanceOf[jcp.PtrIntElementValue].value match {
        case 1 => m.setAJInlineForced()
        case 0 => // nothing to do
      }
    } else if (atype.equals(jcp.jstrAjNoInline)) {
      m.setNeverInline()

    } else if (atype.equals(jcp.jstrAjCallConv)) {
      val calltype = enumAttr(ann, jcp.jstrAjCallConvValue)

      val callconv = if (calltype == jcp.jstrAjCallTypeStdCall) {
        STDCALL
      } else if (calltype == jcp.jstrAjCallTypeC) {
        CCALL
      } else if (calltype == jcp.jstrAjCallTypeVMCall) {
        VMCALL
      } else if (calltype == jcp.jstrAjCallTypeManaged) {
        MANAGED
      } else if (calltype == jcp.jstrAjCallTypeGCAware) {
        GCAWARE
      } else if (calltype == jcp.jstrAjCallTypeManual) {
        MANUAL
      } else if (calltype == jcp.jstrAjCallTypeUnmanaged) {
        UNMANAGED
      } else if (calltype == jcp.jstrAjCallTypeRTCall) {
        RTCALL
      } else {
        shouldNotReachHere()
      }

      m.setCallConv(callconv)

    } else if (atype == jcp.jstrAjCallToManaged) {
      val classSig   = stringAttr(ann, jcp.jstrAjCallToManagedClassName)
      val methodName = stringAttr(ann, jcp.jstrAjCallToManagedName)
      m.markAsAjCallToManaged(classSig, methodName)

    } else if ((atype == jcp.jstrAjReplacement) && !noreplacements) {
      val classSig   = stringAttrSafe(ann, jcp.jstrAjReplacementClassName)
      val methodName = stringAttr(ann, jcp.jstrAjReplacementName)
      val methodSig  = stringAttrSafe(ann, jcp.jstrAjReplacementSig)
      val isInActiveEnv = isAnnotInActiveEnvironment(ann, jcp.jstrAjReplacementEnvironment)

      // Replacements are also collected as separate preprocessing pass over class files.
      m.markAsAjReplacement(classSig, methodName, methodSig, isInActiveEnv)

    } else if (atype == jcp.jstrAjUncheckedCall) {
      val classSig   = stringAttr(ann, jcp.jstrAjUncheckedCallClassName)
      val methodName = stringAttr(ann, jcp.jstrAjUncheckedCallName)
      val methodSig  = stringAttr(ann, jcp.jstrAjUncheckedCallSig)
      m.markAsAjUncheckedCall(classSig, methodName, methodSig)

    } else if (atype == jcp.jstrAjUncheckedNew) {
      val classSig  = stringAttr(ann, jcp.jstrAjUncheckedNewClassName)
      val methodSig = stringAttr(ann, jcp.jstrAjUncheckedNewSig)
      m.markAsAjUncheckedNew(classSig, methodSig)

    } else if (atype == jcp.jstrAjHookInvoker) {
      m.markAsHookInvoker()

    } else if (atype == jcp.jstrAjCompilerHintMethod) {
      val array = arrayAttr(ann, jcp.jstrAjCompilerHintMethodValue)
      for (x <- array; hint = x.asInstanceOf[jcp.PtrStringElementValue].value) hint match {
        case jcp.jstrAjCompilerHintMethodNoEscape => m.markAsAjRTNoEscape()
        case jcp.jstrAjCompilerHintMethodRetThis => m.markAsRetThis()
        case jcp.jstrAjCompilerHintMethodAllocator => m.markAsAjRTAllocator()
        case jcp.jstrAjCompilerHintMethodNoReturn => m.markAsAjNoReturn()
      }

    } else if (atype == jcp.jstrAjInlineIfConstParamsIndices) {
      val array = arrayAttr(ann, jcp.jstrAjInlineIfConstParamsIndicesValue)
      val indices = array.map(_.asInstanceOf[jcp.PtrIntElementValue].value)
      m.setAJInlineIfConstParams(indices)

    } else if (atype == jcp.jstrAjCompilerHintStackCheckByCaller) {
      m.setStackCheckByCallerByteCount(intAttr(ann, jcp.jstrAjCompilerHintStackCheckByCallerValue))

    } else if (atype == jcp.jstrAjGCAware) {
      m.markAsAjGCAware()

    } else if (atype == jcp.jstrAjLongSafe) {
      m.markAsAjLongSafe()

    } else if (atype == jcp.jstrAjNoLocalGCPoints) {
      m.markAsNoLocalGCPoints()

    } else if (atype == jcp.jstrAjNoTracedRegsOnEntry) {
      m.markAsNoTracedRegsOnEntry()

    } else if (atype == jcp.jstrAjDirtyForClassGC) {
      m.markAsDirtyForClassGC()

    } else if (atype == jcp.jstrAjStrictMemory) {
      m.markAsAjStrictMemory()

    } else if (atype == jcp.jstrAjVersionedContext) {
      m.markAsAjVersionedContext()

    } else if (atype == jcp.jstrAjBootstrap) {
      assert(!clazz.isNonBootstrap)
      clazz.markAsBootstrap()

    } else if (atype == jcp.jstrAjThinConstructor) {
      m.markAsThinConstructor()

    } else if (atype == jcp.jstrAjInterpretationLoop) {
      m.markAsInterpretationLoop()

    } else if (atype == jcp.jstrGenTableSwitch) {
      m.markAsGenTableSwitch()

    } else if (atype == jcp.jstrAjDomain) {
      val domain = enumAttr(ann, jcp.jstrAjDomainValue)
      if (domain == jcp.jstrAjDomainTypeAj) {
        m.markAsAJDomain()
      } else if (domain == jcp.jstrAjDomainTypeJava) {
        m.markAsJavaDomain()
      } else if (domain == jcp.jstrAjDomainTypeCangjie) {
        m.markAsCangjieDomain()
      } else {
        shouldNotReachHere()
      }

    } else if (atype == jcp.jstrNonThrowing) {
      m.markAsNonThrowing()

    } else if (atype == jcp.jstrAjNoPreparationCheck) {
      m.markAsNoPreparationCheck()

    } else if (atype == jcp.jstrAjVersionedMarker) {
      val declaringClassNameGCAware   = stringAttr(ann, jcp.jstrAjVersionedMarkerDeclaringClassNameGCAware).replace('.', '/')
      val nameGCAware                 = stringAttr(ann, jcp.jstrAjVersionedMarkerNameGCAware)
      val declaringClassNameUnmanaged = stringAttr(ann, jcp.jstrAjVersionedMarkerDeclaringClassNameUnmanaged).replace('.', '/')
      val nameUnmanaged               = stringAttr(ann, jcp.jstrAjVersionedMarkerNameUnmanaged)
      m.markAsVersionedMarker(VersionedMarker(declaringClassNameGCAware, nameGCAware, declaringClassNameUnmanaged, nameUnmanaged))

    } else if (atype == jcp.jstrAjCallConvHead) {
      m.setCallConvHeadInLimit(intAttr(ann, jcp.jstrAjCallConvHeadInLimit))
      m.setCallConvHeadOutLimit(intAttr(ann, jcp.jstrAjCallConvHeadOutLimit))

    } else if (atype == jcp.jstrRecordInitializer) {
      m.markAsAJRecordInitializer()

    } else if (atype == jcp.jstrAjCallConvAltLocation) {
      m.markAsAltLocationResult()

    } else if (atype == jcp.jstrAjMethodInfoFrameDescriptorGetter) {
      m.markAsMethodInfoFrameDescriptorGetter()

    } else if (atype == jcp.jstrAjDelayedIntrinsic) {
      val className = stringAttr(ann, jcp.jstrAjDelayedIntrinsicDeclaringClassName)
      val methodName = stringAttr(ann, jcp.jstrAjDelayedIntrinsicName)
      m.markAsAJDelayedIntrinsic(className, methodName)
    }
  }

  private def processAjAnnotationForMethodParam(ann: jcp.PtrAnnotation, m: pcO.Method, param: Int): Unit = {
    val atype = ann.type0

    if (atype == jcp.jstrAjCallConvPreserved) {
      m.addCallConvPreservedParam(param)
    } else if (atype == jcp.jstrAjCallConvAltLocation) {
      m.addCallConvAltLocationParam(param)
    }
  }

  private def processAjAnnotationForFlatField(ann: jcp.PtrAnnotation, f: pcO.Field): Unit = {
    val atype = ann.type0

    if (atype == jcp.jstrAjFlat) {
      f.markAsAJFlat()

    } else if (atype == jcp.jstrAjLayoutInfoFlatField) {
      val size      = intAttr(ann, jcp.jstrAjLayoutInfoANYSize)
      val alignment = intAttr(ann, jcp.jstrAjLayoutInfoANYAlignment)
      f.setAJFlatInfo(size, alignment)
    }
  }

  private def processAjAnnotationForStaticField(ann: jcp.PtrAnnotation, f: pcO.StaticField): Unit = {
    checkExternal(ann, f)
    checkExport(ann, f)
    checkData(ann, f)
  }

  private def processAjAnnotationForInstanceField(ann: jcp.PtrAnnotation, f: pcO.InstanceField): Unit = {
    val atype = ann.type0

    if (atype == jcp.jstrAjLayoutInfoInstanceField) {
      f.setOffset(intAttr(ann, jcp.jstrAjLayoutInfoInstanceFieldOffset))
    }
  }

  private def processAjAnnotationForClass(ann: jcp.PtrAnnotation, clazz: pcO.Class): Unit = {
    val atype = ann.type0

    if (atype == jcp.jstrAjStruct) {
      clazz.markAsStructClass()

    } else if (atype == jcp.jstrAjThin) {
      clazz.markAsThinClass()

    } else if (atype == jcp.jstrAjValue) {
      clazz.markAsValueClass()

    } else if (atype == jcp.jstrAjPolyThin) {
      clazz.markAsPolyThinClass()

    } else if (atype == jcp.jstrAjNamespace) {
      clazz.markAsNamespace()

    } else if (atype == jcp.jstrAjManaged) {
      clazz.markAsAJManagedType()

    } else if (atype == jcp.jstrAjExtended) {
      clazz.markAsAJExtended()

    } else if (atype == jcp.jstrAjEnvironment) {
      if (!isActiveEnvironment(stringAttr(ann, jcp.jstrAjEnvironmentValue))) {
        clazz.markAsInInactiveEnvironment()
        clazz.setNotVerifiedCodeError(FatalError, js.format("AJ fatal error: class %S from inactive environment used", clazz.name))
      }

    } else if (atype == jcp.jstrAjLayoutInfoType) {
      clazz.size = intAttr(ann, jcp.jstrAjLayoutInfoANYSize)
      clazz.alignment = intAttr(ann, jcp.jstrAjLayoutInfoANYAlignment)

    } else if (atype == jcp.jstrAjBootstrap) {
      assert(!clazz.isNonBootstrap)
      clazz.markAsBootstrap()

    } else if (atype == jcp.jstrAjNonBootstrap) {
      assert(!clazz.isBootstrap)
      clazz.markAsNonBootstrap()

    } else if (atype == jcp.jstrAjInterpreterInternals) {
      clazz.markAsInterpreterInternals()
    }
  }

  private def processJetSpecificAnnotations(a: jcp.PtrAnnotationsAttr, clazz: pcO.Class, member: pcO.Member): Unit = {
    if (a.isMalformed) {
      return
    }

    var importArray: Array[jcp.PtrElementValue] = null

    for (i <- a.annotations.indices) {
      val atype = a.annotations(i).type0
      if (member != null && member.isInstanceOf[pcO.Field]) {
        processAjAnnotationForFlatField(a.annotations(i), member.asInstanceOf[pcO.Field])
      }
      if (member != null && member.isInstanceOf[pcO.Method]) {
        processAjAnnotationForMethod(a.annotations(i), clazz, member.asInstanceOf[pcO.Method])
      } else if (member != null && member.isInstanceOf[pcO.StaticField]) {
        processAjAnnotationForStaticField(a.annotations(i), member.asInstanceOf[pcO.StaticField])
      } else if (member != null && member.isInstanceOf[pcO.InstanceField]) {
        processAjAnnotationForInstanceField(a.annotations(i), member.asInstanceOf[pcO.InstanceField])
      } else if (clazz != null) {
        if (atype == jcp.jstrAjImplicitImport) {
          val importValue = getValueByName(a.annotations(i), jcp.jstrAjImplicitImportValue)
          assert(importValue != null && importValue.isInstanceOf[jcp.PtrArrayElementValue])
          importArray = importValue.asInstanceOf[jcp.PtrArrayElementValue].value
        } else {
          processAjAnnotationForClass(a.annotations(i), clazz)
        }
      }
    }

    if (member == null && clazz.isInActiveEnvironment && importArray != null) {
      for (i <- importArray.indices) {
        val importValue = importArray(i)
        assert(importValue.isInstanceOf[jcp.PtrStringElementValue])
        addClassFileImport(clazz, importValue.asInstanceOf[jcp.PtrStringElementValue].value.replace('.', '/'))
      }
    }
  }

  private def processJetSpecificParamAnnotations(a: jcp.PtrParameterAnnotationsAttr, method: pcO.Method): Unit = {
    if (a.isMalformed) {
      return
    }
    for {
      (annots, param) <- a.annotations.zipWithIndex
      anno <- annots
    } {
      processAjAnnotationForMethodParam(anno, method, param)
    }
  }

  private def isEligableForLocVarsToMethParsConversion(accflags: Set32): Boolean = (accflags & Set32.of(jcp.AccBridge.toUByte, jcp.AccSynthetic.toUByte)) == Set32.empty

       /**
    JET-5089: Try to convert local variables debug info into method parameters
    attribute. The code replicates the logic of
    spring-core/src/main/java/org/springframework/core/LocalVariableTableParameterNameDiscoverer.java
  */
  private def convertLocVarsToMethPars(C: jcp.PtrClassInfo, attrs: Array[jcp.AttributeInfo], attrsCount: Int, meth: pcO.Method): Unit = {
    val numOfPars = meth.getSignature.parameterTypes.size
    var nextIndex = if (meth.isStatic) 0 else 1

    if (numOfPars == 0) {
      // no parameters: nothing to convert
      return
    }

    val methPars = Array.fill[jcp.MethodParameter](numOfPars)(new jcp.MethodParameter())
    val parIndexToSlot = new Array[Int](numOfPars)
    var i = 0
    for (t <- meth.getSignature.parameterTypes) {
      parIndexToSlot(i) = nextIndex
      nextIndex += t.jbcKindErased.nslots
      i += 1
    }

    var seenLocVars = false
    for (k <- 0 until attrsCount) {
      if (jcp.jstrLocVarName.equals(jcp.getString(C, attrs(k).nameIndex.toInt))) {
        val locvars = attrs(k).localVariableTable
        if (locvars != null) {
          seenLocVars = true
          for (i <- locvars.indices) {
            for (j <- 0 until numOfPars) {
              if (parIndexToSlot(j) == locvars(i).slot.toInt) {
                methPars(j).name = jcp.getString(C, locvars(i).nameIndex.toInt)
              }
            }
          }
        }
      }
    }

    if (seenLocVars) {
      for (i <- 0 until numOfPars) {
        if (methPars(i).name == null) {
          // do not keep null name parameters
          methPars(i).name = js.format("arg%d", i)
        }
      }

      meth.setParameters(methPars, lvtConverted = true)
    }
  }

  private def set15Attrs(C: jcp.PtrClassInfo, attrs: Array[jcp.AttributeInfo], count: Int, clazz: pcO.Class, member: pcO.Member, methodInfo: MethodInfo): Unit = {
    assert(clazz != null)

    for (a <- jcp.getAttribute(C, attrs, count, jcp.jstrSignature)) {
      if (member != null) {
        member.setGenericSignature(jcp.getString(C, a.index.toInt))
      } else {
        clazz.setGenericSignature(jcp.getString(C, a.index.toInt))
      }
    }

    for (a <- jcp.getAttribute(C, attrs, count, jcp.jstrRuntimeVisibleAnnotations)) {
      if (member != null) {
        member.setAnnotations(a.annotation.asInstanceOf[jcp.PtrAnnotationsAttr], rtVisible = true)
      } else {
        clazz.setAnnotations(a.annotation.asInstanceOf[jcp.PtrAnnotationsAttr], rtVisible = true)
      }
      processJetSpecificAnnotations(a.annotation.asInstanceOf[jcp.PtrAnnotationsAttr], clazz, member)
    }

    for (a <- jcp.getAttribute(C, attrs, count, jcp.jstrRuntimeInvisibleAnnotations)) {
      if (member != null) {
        member.setAnnotations(a.annotation.asInstanceOf[jcp.PtrAnnotationsAttr], rtVisible = false)
      } else {
        clazz.setAnnotations(a.annotation.asInstanceOf[jcp.PtrAnnotationsAttr], rtVisible = false)
      }
      processJetSpecificAnnotations(a.annotation.asInstanceOf[jcp.PtrAnnotationsAttr], clazz, member)
    }

    for (a <- jcp.getAttribute(C, attrs, count, jcp.jstrRuntimeVisibleTypeAnnotations)) {
      if (member != null) {
        member.setTypeAnnotations(a.annotation.asInstanceOf[jcp.PtrTypeAnnotationsAttr], rtVisible = true)
      } else {
        clazz.setTypeAnnotations(a.annotation.asInstanceOf[jcp.PtrTypeAnnotationsAttr], rtVisible = true)
      }
    }

    for (a <- jcp.getAttribute(C, attrs, count, jcp.jstrRuntimeInvisibleTypeAnnotations)) {
      if (member != null) {
        member.setTypeAnnotations(a.annotation.asInstanceOf[jcp.PtrTypeAnnotationsAttr], rtVisible = false)
      } else {
        clazz.setTypeAnnotations(a.annotation.asInstanceOf[jcp.PtrTypeAnnotationsAttr], rtVisible = false)
      }
    }

    if (member == null) {
      for (a <- jcp.getAttribute(C, attrs, count, jcp.jstrEnclosingMethod)) {
        setEnclosingMethod(C, clazz, a)
      }
    }

    if (member != null && member.isInstanceOf[pcO.Method]) {
      val meth = member.asInstanceOf[pcO.Method]
      for (a <- jcp.getAttribute(C, attrs, count, jcp.jstrRuntimeVisibleParameterAnnotations)) {
        meth.setParameterAnnotations(a.annotation.asInstanceOf[jcp.PtrParameterAnnotationsAttr], rtVisible = true)
        processJetSpecificParamAnnotations(a.annotation.asInstanceOf[jcp.PtrParameterAnnotationsAttr], meth)
      }
      for (a <- jcp.getAttribute(C, attrs, count, jcp.jstrRuntimeInvisibleParameterAnnotations)) {
        meth.setParameterAnnotations(a.annotation.asInstanceOf[jcp.PtrParameterAnnotationsAttr], rtVisible = false)
        processJetSpecificParamAnnotations(a.annotation.asInstanceOf[jcp.PtrParameterAnnotationsAttr], meth)
      }
      for (a <- jcp.getAttribute(C, attrs, count, jcp.jstrAnnotationDefault)) {
        meth.setAnnotationDefault(a.annotation.asInstanceOf[jcp.PtrAnnotationDefaultAttr])
      }

      jcp.getAttribute(C, attrs, count, jcp.jstrMethodParameters) match {
        case Some(a) if a.methodParameters != null =>
          meth.setParameters(a.methodParameters, lvtConverted = false)
        case _ =>
          if (!noLocalVarsToMethParsConversion && isEligableForLocVarsToMethParsConversion(methodInfo.accessFlag)) {
            val code = jcp.getCode(C, methodInfo)
            if (code != null) {
              for (_ <- jcp.getAttribute(C, code.attribute, code.attributeCount.toInt, jcp.jstrLocVarName)) {
                convertLocVarsToMethPars(C, code.attribute, code.attributeCount.toInt, meth)
              }
            }
          }
      }
    }
  }

  private def setMemberAttrs(C: jcp.PtrClassInfo, o: pcO.Member, attrs: Array[jcp.AttributeInfo], count: Int): Unit = {
    if (jcp.getAttribute(C, attrs, count, jcp.jstrDeprecated).isDefined) {
      o.markAsDeprecated()
    }
  }

  private def checkCircularityErr(type0: pcO.Class, base: pcO.Class): Boolean = {
    if (base ne type0) {
      if (base == null || base.hasAbsentSuper || !checkCircularityErr(type0, base.getSuperClassO2)) {
        return false
      }
    }

    base.setClassDefinitionError(ClassCircularityError, base.name)
    true
  }

  private def checkCircularityInterf(type0: pcO.Class, intf: pcO.Class): Boolean = {
    if (intf eq type0) {
      intf.setClassDefinitionError(ClassCircularityError, intf.name)
      return true
    }
    for (clazz <- intf.getSuperInterfacesO2) {
      if (checkCircularityInterf(type0, clazz)) {
        intf.setClassDefinitionError(ClassCircularityError, intf.name)
        return true
      }
    }
    false
  }

  private def addBase(C: jcp.PtrClassInfo, type0: pcO.Class): Unit = {
    var base: pcO.Class = null

    val i = C.constantPool(C.superClass.toInt).indexName.toInt

    if (TypeProvider.isXScalaAnyRef(type0)) {
      base = CacheAPIModule.getClass(ScalaRefType)

    } else if (i != 0 && !(C.accessFlag contains jcp.AccInterface)) {
      val s = jcp.getString(C, i)
      base = pcO.makeClass(s)
      assert(base != null)
      if (base.isUnavailable) {
        type0.setAbsentSuper(base)
        return
      } else if (base.isClassDefinitionError) {
        type0.copyVerifyErrorFrom(base)
        return
      } else if (base.isInterface) {
        type0.setClassDefinitionError(IncompatibleClassChangeError, type0.name)
        return
      } else if (base.isFinal) {
        type0.setClassDefinitionError(VerifyError, js.format("class %S extends final class %S", type0.name, base.name))
        return
      } else if (checkCircularityErr(type0, base)) {
        return
      } else if (!base.isAccessibleFrom(type0)) {
        type0.setClassDefinitionError(IllegalAccessError, js.format("class %S cannot access its superclass %S", type0.name, base.name))
        return
      } else if (base.isNotVerifiedCode) {
        // JET-5271: if base has not verified code
        // we mark its child as not verified also
        // but we collect super class for it anyway,
        // as it maybe used for checking verification pairs
        // (assign comatibility do not check classes for not verified code)
        type0.copyVerifyErrorFrom(base)
      }
    } else if (i == 0 && !js.jstrObject.equals(type0.name) && !(C.accessFlag contains jcp.AccInterface)) {
      base = pcO.makeClass(js.jstrObject)
      assert(base != null)
    }

    if (base != null) {
      type0.setSuperClass(RefClassType(base.symType))
    }
  }

  private def addInterfaces(C: jcp.PtrClassInfo, type0: pcO.Class): Unit = {
    if (C.interfaceCount > UShort(0)) {
      val interfaces = ArrayBuffer.empty[RefInterfaceType]
      for (i <- 0 until C.interfaceCount.toInt) {
        // getting interface class and object
        val s = jcp.getString(C, C.constantPool(C.interface(i).toInt).indexName.toInt)

        val interface = pcO.makeClass(s)
        assert(interface != null)
        interfaces += RefInterfaceType(interface.symType)

        if (interface.isUnavailable) {
          if (!type0.hasAbsentSuper) {
            type0.setAbsentSuper(interface)
          }
          return
        } else if (!interface.isVerifiable) {
          type0.copyVerifyErrorFrom(interface)
          return
        } else {
          if (!interface.isInterface) {
            type0.setClassDefinitionError(IncompatibleClassChangeError, type0.name)
            return
          }
          if (checkCircularityInterf(type0, interface)) {
            return
          }
          if (!interface.isAccessibleFrom(type0)) {
            type0.setClassDefinitionError(IllegalAccessError, js.format("class %S cannot access its superinterface %S", type0.name, interface.name))
            return
          }
        }
      }
      type0.setSuperInterfaces(interfaces.toArray)
    }
  }

  private def numOfParameters(sig: XString): Int = {
    var argIter: jcp.SignatureIterator = new jcp.SignatureIterator()

    argIter.initEx(sig, 1, sig.lastIndexOf(')'))
    var np = 0
    while (argIter.hasNext) {
      np += 1
      argIter.next()
    }
    np
  }

  /** Estimates size of the virtual class file which could be generated for the given class.
    *
    * Estimation := Original class file size +
    *               Sum(Method code adjustment) for each method +
    *               Padding attribute overhead.
    *
    * Method code adjustment := - Length of code + Code stub size - Size of attributes of Code attribute + Size of converted from LVT method parameters.
    */
  private def estimateVCFSize(C: jcp.PtrClassInfo): Int = {
    val ATTR_HEADER: Int = 6
    val PADDING_ATTR_OVERHEAD: Int = ATTR_HEADER + "com.huawei.excelsior.padding".length + 2 + 1
    val CODE_STUB_SIZE: Int = 2
    val LOCAL_VARIABLE_SIZE: Int = 10

    var size = C.bytecodeSize

    for (method <- C.methods) {
      val code = jcp.getCode(C, method)
      if (code != null) {
        var hasLVT = false
        size = size - code.codeLength + CODE_STUB_SIZE
        for (j <- 0 until code.attributeCount.toInt) {
          if (code.attribute(j).localVariableTable != null && jcp.jstrLocVarName.equals(jcp.getString(C, code.attribute(j).nameIndex.toInt))) {
            hasLVT = true
          }
          size -= code.attribute(j).length.toInt
        }

        if (!noLocalVarsToMethParsConversion && hasLVT && isEligableForLocVarsToMethParsConversion(method.accessFlag)) {
          if (!jcp.getAttribute(C, method.attribute, method.attributeCount.toInt, jcp.jstrMethodParameters).exists(_.methodParameters != null)) {
            val paramCount = numOfParameters(jcp.getString(C, method.signatureIndex.toInt))
            if (paramCount != 0) {
              size = size + paramCount * LOCAL_VARIABLE_SIZE + ATTR_HEADER + 2
            }
          }
        }
      }
    }

    assert(size > 0)
    size += PADDING_ATTR_OVERHEAD
    size
  }

  private def allocateModule(info: jcp.PtrClassInfo, this0: XString): pcO.Class = {
    xmErrors.makeobjClassAmount += 1

    val fname = env.info.module
    val name = if (pcNames.isClassName(fname)) {
      pcNames.newClassName(JBCPreprocessor.movedScalaClassName(fname.name) ensuring (_ == this0))
    } else {
      fname
    }
    val clazz = makeClassHead(name, getSourceAccessFlags(info, this0), info.accessFlag)
    clazz.classInfo = info

    if (O2Env.env.enabled(XScala)) {
      clazz.markAsXScalaType()
    } else {
      for (a <- jcp.getAttribute(info, info.attribute, info.attributeCount.toInt, jcp.jstrSourceFile)) {
        val sourceFile = info.constantPool(a.index.toInt).bufferPtr
        clazz.setBCSourceName(sourceFile)

        if (languagePack == LanguagePack.SCALA) {
          // TODO: find a better way to distinguish XScala types
          if (sourceFile != null && (sourceFile.endsWith(XString(".scala")) ||
            (sourceFile.endsWith(XString(".java")) && O2Env.env.enabled(BuildXKRN)))) {
            clazz.markAsXScalaType()
          }
        }
      }
    }

    makeClassFileImport(clazz, info)
    addBase(info, clazz)
    addInterfaces(info, clazz)

    if (clazz.isUnloadable) {
      clazz.setSuperInterfaces(null) // JET-3356: clean super interfaces
      return clazz
    }

    set15Attrs(info, info.attribute, info.attributeCount.toInt, clazz, null, null)

    if (clazz.isUnloadable) {
      assert(!clazz.isInActiveEnvironment)
      return clazz
    }

    for (a <- jcp.getAttribute(info, info.attribute, info.attributeCount.toInt, jcp.jstrInnerClasses) if a.innerClasses != null) {
      // compute declaring class
      var i = 0
      var found = false
      while (i < a.innerClasses.length && !found) {
        if (a.innerClasses(i).innerClassInfoIndex != UShort(0)) {
          var s = jcp.getString(info, info.constantPool(a.innerClasses(i).innerClassInfoIndex.toInt).indexName.toInt)
          if (s.equals(this0)) {
            found = true
            if (a.innerClasses(i).outerClassInfoIndex != UShort(0)) {
              s = jcp.getString(info, info.constantPool(a.innerClasses(i).outerClassInfoIndex.toInt).indexName.toInt)
              clazz.outerClass = pcO.makeClass(s) ensuring (_ != null)
            }
          }
        }
        i += 1
      }

      // compute inner classes
      for (i <- a.innerClasses.indices) {
        if (a.innerClasses(i).innerClassInfoIndex != UShort(0)) {
          var s = jcp.getString(info, info.constantPool(a.innerClasses(i).innerClassInfoIndex.toInt).indexName.toInt)
          val res = pcO.makeClass(s)
          assert(res != null)
          if (a.innerClasses(i).outerClassInfoIndex != UShort(0)) {
            s = jcp.getString(info, info.constantPool(a.innerClasses(i).outerClassInfoIndex.toInt).indexName.toInt)
            if (s.equals(this0)) {
              clazz.addInnerClass(res, a.innerClasses(i).innerClassAccessFlags)
            }
          }
        }
      }
    }

    if (jcp.getAttribute(info, info.attribute, info.attributeCount.toInt, jcp.jstrDeprecated).isDefined) {
      clazz.markAsDeprecated()
    }

    clazz
  }

  private def getFieldValue(C: jcp.PtrClassInfo, i: Int): ConstValue = {
    val info = C.constantPool(i)
    info.constantType match {
      case jcp.TagInteger => IntValue     (info.low)
      case jcp.TagLong    => LongValue    (makeLong(info.low, info.high))
      case jcp.TagFloat   => FloatValue   (info.realVal)
      case jcp.TagDouble  => DoubleValue  (info.longRealVal)
      case jcp.TagString  => StringValue  (C.constantPool(info.index.toInt).bufferPtr)
    }
  }

  private def addField(C: jcp.PtrClassInfo, curclass: pcO.Class, field: jcp.FieldInfo, index: Int): Unit = {
    val name = jcp.getString(C, field.nameIndex.toInt)
    val sig = jcp.getString(C, field.signatureIndex.toInt)
    val o = curclass.newField(name, sig, maskModifiers(accf_field, field.accessFlag), addSignatureImport = true)
    o.setNumberInClassFile(index)
    setMemberAttrs(C, o, field.attribute, field.attributeCount.toInt)
    set15Attrs(C, field.attribute, field.attributeCount.toInt, curclass, o, null)

    o match {
      case o: pcO.StaticField =>
        for (a <- jcp.getAttribute(C, field.attribute, field.attributeCount.toInt, jcp.jstrCValue)) {
          // constant value attributes are allowed only for primitive types and strings
          assert(sig.charAt(0) != '[')
          assert(sig.charAt(0) != 'L' || sig.equals(js.jstrStringSig))
          o.value = getFieldValue(C, a.index.toInt)
          o.markAsHasInitialValue()
        }
      case _ =>
    }
    // InstanceField
  }

  private def addMethodThrows(C: jcp.PtrClassInfo, curClass: pcO.Class, method: MethodInfo, p: pcO.Method): Unit = {
    for (a <- jcp.getAttribute(C, method.attribute, method.attributeCount.toInt, jcp.jstrException)) {
      val e = a.exceptionIndexTable
      if (e != null) {
        val tarray = new Array[pcO.Class](e.length)
        for (j <- e.indices) {
          val s = jcp.getString(C, C.constantPool(e(j).toInt).indexName.toInt)
          val cls = pcO.makeClass(s)
          assert(cls != null)
          curClass.addImport(cls)
          tarray(j) = cls
        }
        p.setThrows(tarray)
      }
    }
  }

  private def collectConstantStrings(curClass: pcO.Class, C: jcp.PtrClassInfo): Unit = {
    val table = curClass.getStringTable
    val pool = C.constantPool
    for (c <- C.constants if c.constantType == jcp.TagString.toByte) {
      table.addString(pool(c.index.toInt).bufferPtr)
    }
  }

  private def addMethod(C: jcp.PtrClassInfo, clazz: pcO.Class, method: MethodInfo, index: Int): Unit = {
    val name = JBCPreprocessor.preprocessMethodName(jcp.getString(C, method.nameIndex.toInt), clazz)
    var sig = jcp.getString(C, method.signatureIndex.toInt)
    val isNative = method.accessFlag contains jcp.AccNative

    // Update signatures of AJ javac-level @Intrinsic methods that contains Java array of AJ Managed types,
    // as such arrays are prohibited and we don't want to parse it or create symlevel entities for it.
    // Note that it is safe to erase because AJ javac guarantees that these method has no uses.
    if (isNative) {
      if (clazz.isAJManagedEnum && name.equals2("values") && sig.equals(js.format("()[L%S;", clazz.name))) {
        // update signature of "values()" in AJ Managed enums
        sig = js.newJString("()[Ljava/lang/Object;")
      } else if (clazz.name.equals2("com/huawei/excelsior/aj/util/EnumSet") && name.equals2("__aj__of__Lcom_huawei_excelsior_aj_lang_AJManaged_2_3Lcom_huawei_excelsior_aj_lang_AJManaged_2__Lcom_huawei_excelsior_aj_util_EnumSet_2")) {
        // update signature of "com.huawei.excelsior.aj.util.EnumSet.of(E, E...)"
        sig = js.newJString("(Lcom/huawei/excelsior/aj/lang/AJObject;[Ljava/lang/Object;)Lcom/huawei/excelsior/aj/util/EnumSet;")
      } else if (clazz.name.equals2("com/huawei/excelsior/aj/lang/AJMSyntax") && sig.indexOf(js.newJString("[Lcom/huawei/excelsior/aj/lang/AJObject;")) != -1) {
        sig = js.newJString(sig.toString.replace("[Lcom/huawei/excelsior/aj/lang/AJObject;", "[Ljava/lang/Object;"))
      }
    }

    val p = clazz.newMethod(name, sig, maskModifiers(accf_method, method.accessFlag), addSignatureImport = true)
    if (JBCPreprocessor.ignoreMethod(name, sig, clazz)) {
      // marked method will not be compiled
      p.markAsNoCodeGen()
    }

    // Workaround for JET-17425 and JET-17444
    // Currently scalac produces unstable bytecode, because generation of forwarders is non-deterministic,
    // sometimes such methods are generated, sometimes not.
    // So we inline such methods, if there are any.
    if (JBCPreprocessor.isUnstableForwarder(name, clazz, method.accessFlag)) {
      p.setUnstableForwarder()
      p.setAJInline()
      p.setAJInlineForced()
    }

    p.setNumberInClassFile(index)
    addMethodThrows(C, clazz, method, p)
    setMemberAttrs(C, p, method.attribute, method.attributeCount.toInt)
    set15Attrs(C, method.attribute, method.attributeCount.toInt, clazz, p, method)
    for (a <- jcp.getAttribute(C, method.attribute, method.attributeCount.toInt, jcp.jstrCodeName)) {
      p.setBytecodeSize(a.code.codeLength)
    }
    if (p.isManaged) {
      clazz.markAsContainsManaged()
    }
    if (p.isExported && p.isManaged && clazz.hasManagedMetaInformation && !clazz.isNonBootstrap) {
      assert(!clazz.isNonBootstrap)
      clazz.markAsBootstrap()
    }
  }

  private def buildClassFromInfo(C: jcp.PtrClassInfo, fileName: XString): pcO.Class = {
    val this0 = jcp.getString(C, C.constantPool(C.thisClass.toInt).indexName.toInt)     /* current module name */
    if (this0 != env.info.module.name) {
      env.errors.fault(ErrMsg448, fileName, this0)
    }

    val thisName = JBCPreprocessor.movedScalaClassName(this0)
    val clazz = allocateModule(C, thisName)

    if (!clazz.isUnloadable) {
      for ((field, index) <- C.fields.zipWithIndex) {
        addField(C, clazz, field, index)
      }
      for ((method, index) <- C.methods.zipWithIndex) {
        addMethod(C, clazz, method, index)
      }

      collectConstantStrings(clazz, C)

      pcJCA.injectFields(clazz)
      pcJCA.markNonNullFields(clazz)
    }

    Numerate.preProcessBytecode(clazz)

    if (pc.currentModule == curMn) {
      // Parsing can be invoked recursively during ProcessClass
      // and pc.mod_cnt can increase. Process extra parsed classes as well.
      var mno = curMn
      while (mno < pc.modules.size) {
        Numerate.processClass(pcO.getClassRecord(mno))
        mno += 1
      }
      curMn = pc.INVALID_MNO
    }
    clazz
  }

  def parseClassFile(classFile: xfs.FileDescriptor): pcO.Class = O2Env.stage(Stage.JBCFrontParseClassFile) {
    var cls: pcO.Class = null

    val file = classFile.openSymFile()

    var (loadisok, verifyerror) = jcp.load(file)
    val classInfo = jcp.c

    var noclassdef = false
    var unsupported = false
    if (loadisok) {
      val this0 = jcp.getString(classInfo, classInfo.constantPool(classInfo.thisClass.toInt).indexName.toInt)
      val name = env.info.module.name
      loadisok = name.equals(this0)
      noclassdef = true
    }

    if (!loadisok) {
      if (O2Env.env.enabled(BuildXKRN)) {
        throw new VerificationError(
          XString(s"BAD CLASS IN XKRN: ${classFile.getName}"), VERIFY_ERROR, VerifyError
        )
      }

      //      jcp.C:=jcp.C.Next;
      if (jcp.error != null) {
        unsupported = jcp.error.startsWith(jstrUnsupported, 0)
      }
      val cur_mod = pc.currentModule
      cls = makeErrorClass(env.info.module, noclassdef, unsupported, verifyerror)
      if (curMn == pc.INVALID_MNO) {
        Numerate.processClass(cls)
      }
      pc.currentModule = cur_mod
    } else {
      val i = pc.modules.size
      val mod = env.info.module
      val cur_mod = pc.currentModule
      if (curMn == pc.INVALID_MNO) {
        curMn = i
      }
      assert(pcO.makeClass eq makeClassImpl)
      cls = buildClassFromInfo(classInfo, file.getName)
      assert(cls.mno == i)
      env.info.module = mod
      pc.currentModule = cur_mod
      cls.setBytecodeInfo(classInfo.versionMajor.toShort, classInfo.versionMinor.toShort, estimateVCFSize(classInfo))

      if (isVCFExcluded(cls, classFile)) {
        cls.markAsVCFExcluded()
      }

      if (isNoJavaClass(cls)) {
        cls.markAsNoJavaClass()
      }
    }

    file.close()
    cls.fileDescriptor = classFile

    cls
  }

  //--------------------------------------------------------------------
  def addAllImport(clazz: pcO.Class): Unit = {
    val info = clazz.classInfo
    val pool = info.constantPool

    for (i <- 0 until info.constantPoolCount.toInt) {
      pool(i).constantType match {
        case jcp.TagClass if O2Env.env.enabled(AddImportFromConstantPool) =>
          val t = clazz.resolveClassRef(info, i)
          assert(t != null)
        case jcp.TagInvokeDynamic =>
          assert(!classByO2Object(clazz).isAJManagedType, s"unexpected invokedynamic in AJManaged class ${clazz.name}")
          // try to recongize lambda creation in AOT compiler
          LambdaTypeGenerator(_.getLambdaClass(classByO2Object(clazz), i))
        case _ =>
      }
    }
  }

  def completeSymLevel(c: pcO.Class): Unit = {
    env.info.module = c.nameObj
    if (c.isUnloadable) {
      ep.preExtra(c)
      return
    }
    // TODO: try to remove (beware side-effects)
    c.classInfo
    addAllImport(c)
    ep.passModule(c)
    env.info.module = c.nameObj
  }

  def analyzeClassFile(classFile: xfs.FileDescriptor): Unit = O2Env.stage(Stage.JBCFrontPreprocessClassFile) {
    val file = classFile.openSymFile()
    var (loadisok, _) = try jcp.load(file) finally file.close()
    val classInfo = jcp.c

    if (loadisok) {
      for (m <- classInfo.methods) {
        val attrs = m.attribute
        val count = m.attributeCount.toInt
        for (a <- jcp.getAttribute(classInfo, attrs, count, jcp.jstrRuntimeVisibleAnnotations)) {
          collectAJReplacements(classInfo, m, a.annotation.asInstanceOf[jcp.PtrAnnotationsAttr])
        }
        for (a <- jcp.getAttribute(classInfo, attrs, count, jcp.jstrRuntimeInvisibleAnnotations)) {
          collectAJReplacements(classInfo, m, a.annotation.asInstanceOf[jcp.PtrAnnotationsAttr])
        }
      }
    }
  }

  private def collectAJReplacements(classInfo: jcp.PtrClassInfo, m: jcp.MethodInfo, a: jcp.PtrAnnotationsAttr): Unit = {
    if (a.isMalformed) {
      return
    }

    val className = jcp.getString(classInfo, classInfo.constantPool(classInfo.thisClass.toInt).indexName.toInt)
    val name = jcp.getString(classInfo, m.nameIndex.toInt)
    val sig = jcp.getString(classInfo, m.signatureIndex.toInt)

    val fullName = s"$className.$name$sig"

    for (ann <- a.annotations) {
      val atype = ann.type0
      if ((atype == jcp.jstrAjReplacement) && !noreplacements) {
        val classSig = stringAttrSafe(ann, jcp.jstrAjReplacementClassName)
        val methodName = stringAttr(ann, jcp.jstrAjReplacementName)
        val methodSig = stringAttrSafe(ann, jcp.jstrAjReplacementSig)
        isAnnotInActiveEnvironment(ann, jcp.jstrAjReplacementEnvironment)

        ReplacementLibrary.setStringReplacement(classSig, methodName, methodSig, fullName)
      }
    }
  }

  def set(): Unit = {
    curMn = pc.INVALID_MNO
    pcO.makeClass = makeClassImpl
    env.config.registerOption(new opt.ConfigBitOption("xCHECKINDEX", env.index_check, Checked), value = true)   /* enable array index checks    */
    env.config.registerOption(new opt.ConfigBitOption("xCHECKNULL", env.nil_check, Checked), value = true)     /* enable null pointer checks    */
    env.config.registerOption(new opt.ConfigBitOption("xCHECKARRSTORE", env.arrstore_check, Checked), value = true)
  }

  private lazy val noneLangPackRTClasses: ArrayBuffer[XString] = {
    if (noJavaLanguagePack && O2Env.env.enabled(BuildXKRN)) {
      val eqVal = XString(O2Env.env.valueOfOrNull(NoneLangPackRTClasses))
      if (eqVal != null && eqVal.nonEmpty) {
        env.splitString(eqVal, ',')
      } else null
    } else null
  }

  private def isNoJavaClass(c: pcO.Class): Boolean = {
    if (!O2Env.env.enabled(BuildXKRN)) {
      return noJavaLanguagePack
    }

    if (noneLangPackRTClasses == null) {
      return false
    }

    noneLangPackRTClasses.exists(x => c.name.startsWith(x))
  }

  def ini(): Unit = {
    noreplacements = env.config.option("noreplacements")
    noLocalVarsToMethParsConversion = env.config.option("nolocvarstomethparsconversion")
    noJavaLanguagePack = !languagePack.supports(JAVA)
    rtJarLocaleClasesPrefix = env.config.equation("rtJarLocaleClasesPrefix")
  }
}
