/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.common.{CodeHelpers, Language}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.abi.ABI
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker.ARRAY_SLICE_NAME
import com.huawei.excelsior.jet.compiler.cangjie.{CangjieSymLevelMaker, SymLevelBuilder}
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.*
import com.huawei.excelsior.jet.compiler.o2lib.u.xiFilesModule
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.{CCALL, MANAGED}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{CangjieArray, Int64, JBCReference, NonNullableWrapper}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.*
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ReferenceType, ClassType as RefClassType, InterfaceType as RefInterfaceType}
import com.huawei.excelsior.jet.compiler.{Env, RTConst, TypeProvider}
import xscala.io.stderr
import xscala.matching.Regex
import xscala.util.Set32
import xscala.util.StringOps.r

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// TODO-MODIFIERS: refactor this
class CangjieSymLevelBuilder(srcFD: xiFilesModule.FileDescriptor) extends SymLevelBuilder {

  private val src: XString = srcFD.getName

  private var dupClassToCheck: ClassType = _
  private var methodsToCheck: mutable.HashSet[Method] = _
  private var fieldsToCheck: mutable.HashSet[Field] = _

  private val classes = ArrayBuffer.empty[pcOModule.Class]
  private var currPackage: pcOModule.Class = _

  private var curClass: pcOModule.Class = _


  override def env: LightweightEnvironment = LightweightEnvironment.getInstance

  private implicit val typeProvider: TypeProvider = env

  override def getSource: String = src.toString

  override def build(): Type = {
    onPrevClassCompleted()

    pcOModule.withSymCacheGCProhibited {
      // We must delay symcache GC because it might be triggered in the
      // middle of symlevel writing process, when not all classes are written to disk.
      // This will invalidate references to already dumped members, which can still
      // be used in in-memory classes (e.g. in replacement FEXT).

      for (c <- classes) {
        SymLevelBuilderModule.preprocessClass(c)
      }
      for (c <- classes) {
        SymLevelBuilderModule.processClass(c)
      }
    }

    typeByO2Object(currPackage)
  }

  private def newClass(name: XString, modifiers: Modifiers, isCangjie: Boolean, isCangjieLambdaClass: Boolean = false): pcOModule.Class = {
    val clazz = SymLevelBuilderModule.newClass(pcNamesModule.newClassName(name), modifiers, srcFD)
    this.classes += clazz
    if (isCangjie) {
      clazz.markAsCangjieType()

      if (isCangjieLambdaClass) {
        clazz.markAsCangjieLambdaBaseClass()
        clazz.markAsEvacuatedType()
      }
    } else {
      clazz.markAsJavaAnnotatedCangjieClass()
    }
    if (!languagePack.supports(Language.JAVA)) {
      clazz.markAsNoJavaClass()
    }
    clazz
  }

  private def addClass0(pkg: Type, name: XString, modifiers: Modifiers, isCangjie: Boolean, isCangjieLambdaClass: Boolean = false,
                       genericInfo: GenericInfo): pcOModule.Class = {
    val clazz = newClass(name, modifiers, isCangjie, isCangjieLambdaClass)
    if (isCangjie) {
      clazz.cangjiePackage = typeToO2Class(pkg)
    }

    if (genericInfo != GenericInfo.none) {
      clazz.markAsUniversalGeneric()
      clazz.addGenericInfo(genericInfo)
    }

    clazz
  }

  override def addPackage(name: String, modifiers: Int): ClassType = {
    currPackage = newClass(XString(name), Modifiers(modifiers), isCangjie = true)
    currPackage.cangjiePackage = currPackage
    classByO2Object(currPackage) ensuring (findClass(name) == _)
  }

  override def addBitcodeDeferredPackage(name: String): ClassType = {
    val alreadyAdded = findClass(name)
    if (alreadyAdded != null) {
      assert(!alreadyAdded.isInterface)
      // There could be identical classes defined in different packages, it's OK.
      return asClassType(alreadyAdded)
    }

    val pkg = newClass(XString(name), Modifiers.EMPTY, isCangjie = true)
    pkg.cangjiePackage = pkg
    pkg.markAsBitcodeDeferred()
    classByO2Object(pkg) ensuring (findClass(name) == _)
  }

  override def addPackageField(name: String, sig: SignatureType, modifiers: Int): Field = {
    val f = SymLevelBuilderModule.addField(this.currPackage, XString(name), sig, Set32((Modifiers(modifiers) + STATIC).value))
    if (sig.isRecord) {
      f.markAsAJFlat()
    }
    fieldByO2Object(f)
  }

  private def setStaticFieldConstValue(sf: pcOModule.StaticField, initValue: Long): Unit = {
    import SignatureType.*
    assert(initValue != 0)

    // TODO: should we check that value fits into type?
    sf.value = ConstValues(sf.sig, initValue)

    // FIXME: having initial value doesn't mean that field is constant, we should correctly distinguish let and var
    sf.markAsHasInitialValue()
  }

  override def setStaticFieldConstValue(f: Field, initValue: Long): Unit = {
    val sf = fieldToO2Field(f).asInstanceOf[pcOModule.StaticField]
    setStaticFieldConstValue(sf, initValue)
  }

  private def addPackageMethodImpl(name: String, sig: MethodSignature, llvmIdx: Int, modifiers: Modifiers,
                                   genericInfo: GenericInfo = GenericInfo.none, hasUGDesc: Boolean = false,
                                   hasThisTypeInfoParam: Boolean = false, isCFunc: Boolean = false) = {
    val m = SymLevelBuilderModule.addMethod(this.currPackage, XString(name), sig, Set32((modifiers + STATIC).value),
      ABI.Description(None, hasUGDesc, hasThisTypeInfoParam, isCFunc))
    m.setLLVMIndex(llvmIdx)

    if (genericInfo != GenericInfo.none) {
      m.markAsUniversalGeneric()
      m.addGenericInfo(genericInfo)
    }

    m
  }

  private def markExportedIfNeeded(exportedName: String, m: pcOModule.Method): Unit = {
    if (exportedName != null) {
      m.markAsExported(XString(exportedName))
    }
  }

  override def addPackageMethod(name: String, sig: MethodSignature, exportedName: String, llvmIdx: Int, modifiers: Int, genericInfo: GenericInfo, hasUGDesc: Boolean, hasThisTypeInfoParam: Boolean, isCFunc: Boolean) = {
    val m = addPackageMethodImpl(name, sig, llvmIdx, Modifiers(modifiers), genericInfo, hasUGDesc, hasThisTypeInfoParam, isCFunc)
    markExportedIfNeeded(exportedName, m)
    methodByO2Object(m)
  }

  override def addPackageInit(name: String, sig: MethodSignature, llvmIdx: Int): Method = {
    val m = addPackageMethodImpl(name, sig, llvmIdx, Modifiers.EMPTY)
    m.markAsPackageInit()
    methodByO2Object(m)
  }

  override def addGlobalInit(name: String, sig: MethodSignature, llvmIdx: Int): Method = {
    val m = addPackageMethodImpl(name, sig, llvmIdx, Modifiers.EMPTY)
    m.markAsGlobalInit()
    methodByO2Object(m)
  }

  override def addExternalCMethod(name: String, sig: MethodSignature, vararg: Boolean, llvmIdx: Int) = {
    var modifiers = Modifiers(NATIVE)
    if (vararg) modifiers += VARARGS

    val m = addPackageMethodImpl(name, sig, llvmIdx, modifiers, isCFunc = true)

    m.markAsExternal(XString(name))
    m.setCallConv(CCALL)
    methodByO2Object(m)
  }

  override def addCMethod(name: String, sig: MethodSignature, vararg: Boolean, llvmIdx: Int) = {
    var modifiers = Modifiers.EMPTY
    if (vararg) modifiers += VARARGS

    val m = addPackageMethodImpl(name, sig, llvmIdx, modifiers, isCFunc = true)

    m.setCallConv(MANAGED)
    m.markAsCAnnotated()
    methodByO2Object(m)
  }

  override def addIntrinsicMethod(name: String, sig: MethodSignature, llvmIdx: Int, shouldBeGenerated: Boolean): Method = {
    // There are several places that check different condition for actual method generation:
    //   - CBC file generator looks at native flag to filter out mainly CJ foreign functions and some intrinsics without body.
    //   - Regular AOT generation filters out supported AJ intrinsics and has even more different logic from O2.
    // FIXME: fix this mess
    var modifiers = Modifiers.EMPTY
    if (!shouldBeGenerated) modifiers += NATIVE

    val m = addPackageMethodImpl(name, sig, llvmIdx, modifiers)

    if (!shouldBeGenerated) {
      m.markAsNoCodeGen()
    }
    methodByO2Object(m)
  }

  private def findClass(name: String): Type = {
    env.getTypeProvider.findClass(XString(name), loadPDB = true)
  }

  override def addRecord(pkg: Type, name: String, genericInfo: GenericInfo): ClassType = {
    val xname = XString(name)
    val alreadyAdded = findClass(name)
    if (alreadyAdded != null) {
      return asClassType(alreadyAdded)
    }

    val record = addClass0(pkg, xname, Modifiers(PUBLIC), isCangjie = true, genericInfo = genericInfo)
    record.markAsRecord()

    classByO2Object(record) ensuring (findClass(name) == _)
  }

  override def addArraySlice(pkg: Type, elemTypeOpt: Option[SignatureType]): ClassType = {
    val (arrayType, arraySliceName) = elemTypeOpt match {
      case None =>
        // Note that we cannot use core.Any here because it might not have been created yet.
        (NonNullableWrapper(JBCReference(typeProvider.getCangjieRefType)), ARRAY_SLICE_NAME)
      case Some(elemType) =>
        (SignatureType.CangjieArray(elemType), SignatureType.ArraySlice.name(elemType))
    }

    val arraySliceType = addRecord(pkg, arraySliceName, genericInfo = GenericInfo.none)
    startClassFilling(arraySliceType, null, null)

    // TODO: JET-15710 Cangjie rtexports
    addClassField("base", arrayType, Modifiers(FINAL).value)
    addClassField("start", Int64, Modifiers(FINAL).value)
    addClassField("size", Int64, Modifiers(FINAL).value)

    for (elemType <- elemTypeOpt) {
      typeToO2Class(arraySliceType).setCangjieArrayElementType(elemType)
    }

    arraySliceType
  }

  override def addRawArray(pkg: Type, elemType: SignatureType): ClassType = {
    val xname = XString(CangjieArray.name(elemType))
    val alreadyAdded = findClass(xname.toString)
    if (alreadyAdded != null) {
      // FIXME: Strings declaration are in all packages: keep the only one
      return asClassType(alreadyAdded)
    }
    val cls = addClass0(pkg, xname, Modifiers(PUBLIC), isCangjie = true, genericInfo = GenericInfo.none)

    // Note: elemType is intentionally erased here to correspond to erasure
    //       of cangjie arrays based on their symlevel element types.
    // TODO: do better
    cls.setCangjieArrayElementType(CangjieArray.erasedElemType(elemType))

    val klass = classByO2Object(cls)
    klass.setSourceFile(XString(getSourceForSymlevel))

    klass ensuring (findClass(xname.toString) == _)
  }

  override def addBox(pkg: Type, baseType: SignatureType): ClassType = {
    val xname = XString(CangjieSymLevelMaker.boxName(baseType))
    val alreadyAdded = findClass(xname.toString)
    if (alreadyAdded != null) {
      // FIXME: Strings declaration are in all packages: keep the only one
      return asClassType(alreadyAdded)
    }
    val cls = addClass0(pkg, xname, Modifiers.EMPTY, isCangjie = true, genericInfo = GenericInfo.none)
    cls.setCangjieBoxValueType(baseType)

    val boxType = classByO2Object(cls) ensuring (findClass(xname.toString) == _)

    env.addImport(pkg, boxType)

    startSyntheticClassFilling(boxType)
    addClassField(CangjieSymLevelMaker.BOX_FIELD_NAME, cls.getCangjieBoxValueType, Modifiers.EMPTY.value)

    boxType
  }

  override def addClass(pkg: Type, name: String, modifiers: Int, isCangjie: Boolean, isCangjieLambdaClass: Boolean, genericInfo: GenericInfo): ClassType = {
    val xname = XString(name)
    val alreadyAdded = findClass(name)
    if (alreadyAdded != null) {
      // There could be identical classes defined in different packages, it's OK.
      return asClassType(alreadyAdded)
    }

    classByO2Object(addClass0(pkg, xname, Modifiers(modifiers), isCangjie, isCangjieLambdaClass, genericInfo)) ensuring (findClass(name) == _)
  }

  override def addBitcodeDeferredType(pkg: Type, name: String, isCangjie: Boolean, isRecord: Boolean, isInterface: Boolean, genericInfo: GenericInfo): ClassType = {
    val xname = XString(name)
    val alreadyAdded = findClass(name)
    if (alreadyAdded != null) {
      assert(alreadyAdded.isInterface == isInterface)
      // There could be identical classes defined in different packages, it's OK.
      return asClassType(alreadyAdded)
    }

    val modifiers = if (isInterface) Modifiers(INTERFACE) else Modifiers.EMPTY
    val clazz = addClass0(pkg, xname, modifiers, isCangjie, genericInfo = genericInfo)
    clazz.markAsBitcodeDeferred()
    if (isRecord) {
      clazz.markAsRecord()
    }
    classByO2Object(clazz) ensuring (findClass(name) == _)
  }

  private def duplicateError(details: String): Unit = {
    stderr.println(
      s"ERROR: Duplicate class ${dupClassToCheck.getName} " +
        s"from module \"${nameOf(dupClassToCheck.getCangjiePackage)}\" " +
        s"does not conform the same class from module \"${nameOf(classByO2Object(currPackage))}\". $details")
    sys.exit(10)
  }

  private def nameOf(pkg: Type): String = if (pkg == null) "<none>" else pkg.getName

  private def onPrevClassCompleted(): Unit = {
    checkDuplicate()
    if (this.curClass != null) {
      this.curClass = null
    }
  }

  private def checkDuplicate(): Unit = {
    if (dupClassToCheck != null) {
      if (methodsToCheck.nonEmpty) {
        duplicateError("It has extra methods: " + methodsToCheck.mkString(", "))
      }
      if (fieldsToCheck.nonEmpty) {
        duplicateError("It has extra fields: " + fieldsToCheck.mkString(", "))
      }
      dupClassToCheck = null
      methodsToCheck = null
      fieldsToCheck = null
    }
  }

  private def startClassFillingImpl(_clazz: ClassType, superClass: RefClassType, superInterfaces: Array[RefInterfaceType]): pcOModule.Class = {
    val clazz = typeToO2Class(_clazz)
    if (superClass != null) {
      clazz.setSuperClass(superClass)
    }
    if (superInterfaces != null) {
      clazz.setSuperInterfaces(superInterfaces)
    }
    this.curClass = clazz
    clazz
  }

  override def startClassFilling(clazz: ClassType, superClass: RefClassType, superInterfaces: Array[RefInterfaceType]) = {
    assert(!clazz.isRecord || superClass == null)

    onPrevClassCompleted()
    implicit val tp = env.getTypeProvider // TODO super dirty
    if (clazz.getCangjiePackage != null && clazz.getCangjiePackage != classByO2Object(currPackage)) {
      dupClassToCheck = clazz
      methodsToCheck = mutable.HashSet.empty
      fieldsToCheck = mutable.HashSet.empty
      if (clazz.isInterface) {
        duplicateError("Duplicated class is interface")
      }
      if (clazz.getSuperClass != superClass) {
        duplicateError(s"Super classes differ: ${clazz.getSuperClass} and $superClass")
      }
      if (superInterfaces == null) {
        val superInterfIter = clazz.getDeclaredSuperInterfaces
        if (superInterfIter.hasNext) {
          duplicateError("Duplicated class does not implement " + superInterfIter.next().getName)
        }
      } else {
        // Basically `superInterfaces sameElements clazz.getDeclaredSuperInterfaces`, but this provides more details.
        val superInterfacesIter = superInterfaces.iterator
        clazz.getDeclaredSuperInterfaces.foreach { i =>
          if (!superInterfacesIter.hasNext) {
            duplicateError(s"Duplicated class does not implement ${i.getName}")
          }
          val superInterf = superInterfacesIter.next()
          if (i != superInterf) {
            duplicateError(s"Super interfaces differ: ${i.getName} and ${superInterf.getName}")
          }
        }
        if (superInterfacesIter.hasNext) {
          duplicateError(s"Duplicated class implements extra interface ${superInterfacesIter.next().getName}")
        }
      }
      methodsToCheck ++= clazz.getDeclaredMethods
      fieldsToCheck ++= clazz.getDeclaredFields
      false
    } else {
      startClassFillingImpl(clazz, superClass, superInterfaces)
      true
    }
  }

  override def startSyntheticClassFilling(clazz: ClassType): Boolean = {
    // TODO: use normal name instead of linkage name
    val rootOfHierarchy = RefClassType(findClass(CangjieSymLevelMaker.hierarchyRootLinkageName(env)))
    startClassFilling(clazz, rootOfHierarchy, null)
  }

  override def addClassField(name: String, sig: SignatureType, modifiers: Int): Field = {
    if (dupClassToCheck != null) {
      val field = findDuplicateField(name)
      if (field.getType != sig) {
        duplicateError(s"Field ${field.getFullName} has type ${field.getSignature.toJETSignature} (expected: ${sig.toJETSignature})")
      }
      if (sig.isRecord) {
        if (!field.isAJFlat) {
          duplicateError(s"Field ${field.getFullName} is scalar")
        }
      } else {
        if (field.isAJFlat) {
          duplicateError(s"Field ${field.getFullName} is aggregate")
        }
      }
      field
    } else {
      assert(this.curClass != null)
      val f = SymLevelBuilderModule.addField(this.curClass, XString(name), sig, Set32(modifiers))
      if (sig.isRecord) {
        f.markAsAJFlat()
      }
      fieldByO2Object(f)
    }
  }

  private def addClassMethod(name: String, sig: MethodSignature, modifiers: Int, genericInfo: GenericInfo, hasUGDesc: Boolean, hasThisTypeInfoParam: Boolean) = {
    assert(this.curClass != null)
    val m = SymLevelBuilderModule.addMethod(this.curClass, XString(name), sig, Set32(modifiers), None)

    if (genericInfo != GenericInfo.none) {
      m.markAsUniversalGeneric()
      m.addGenericInfo(genericInfo)
    }

    m
  }

  override def addClassMethod(name: String, sig: MethodSignature, exportedName: String, llvmIdx: Int, modifiers: Int, genericInfo: GenericInfo, hasUGDesc: Boolean, hasThisTypeInfoParam: Boolean) = {
    val method = if (dupClassToCheck != null) {
      findDuplicateMethod(name, sig, Modifiers(modifiers), exportedName)
    } else {
      val m = addClassMethod(name, sig, modifiers, genericInfo, hasUGDesc, hasThisTypeInfoParam)
      markExportedIfNeeded(exportedName, m)
      if (llvmIdx != CangjieSymLevelMaker.NO_LLVM_INDEX) {
        m.setLLVMIndex(llvmIdx)
      }
      methodByO2Object(m)
    }
    method
  }

  override def addInterface(pkg: Type, name: String, modifiers: Int, isCangjie: Boolean, genericInfo: GenericInfo): ClassType = {
    val xname = XString(name)
    val alreadyAdded = findClass(name)
    if (alreadyAdded == null) {
      classByO2Object(addClass0(pkg, xname, Modifiers(modifiers) + INTERFACE, isCangjie, genericInfo = genericInfo)) ensuring (findClass(name) == _)
    } else {
      // There could be identical classes defined in different packages, it's OK.
      asClassType(alreadyAdded)
    }
  }

  override def startInterfaceFilling(iface: ClassType, superinterfaces: Array[RefInterfaceType]) = {
    onPrevClassCompleted()
    if (iface.getCangjiePackage != null && iface.getCangjiePackage != classByO2Object(currPackage)) {
      dupClassToCheck = iface
      methodsToCheck = mutable.HashSet.empty
      fieldsToCheck = mutable.HashSet.empty
      if (!iface.isInterface) {
        duplicateError("Duplicated class is not interface")
      }
      if (superinterfaces == null) {
        val superInterfIter = iface.getDeclaredSuperInterfaces
        if (superInterfIter.hasNext) {
          duplicateError(s"Duplicated interface does not implement ${superInterfIter.next().getName}")
        }
      } else {
        // Basically `superInterfaces sameElements clazz.getDeclaredSuperInterfaces`, but this provides more details.
        val superInterfacesIter = superinterfaces.iterator
        iface.getDeclaredSuperInterfaces.foreach { i =>
          if (!superInterfacesIter.hasNext) {
            duplicateError(s"Duplicated interface does not implement ${i.getName}")
          }
          val superInterf = superInterfacesIter.next()
          if (i != superInterf) {
            duplicateError(s"Super interfaces differ: ${i.getName} and ${superInterf.getName}")
          }
        }
        if (superInterfacesIter.hasNext) {
          duplicateError(s"Duplicated interface implements extra interface ${superInterfacesIter.next().getName}")
        }
      }
      methodsToCheck ++= iface.getDeclaredMethods
      assert(iface.getDeclaredFields.isEmpty)
      false
    } else {
      startClassFillingImpl(iface, null, superinterfaces)
      true
    }
  }

  override def addImport(importer: Type, importee: Type): Unit = {
    if (importer != importee) {
      env.addImport(importer, importee)
    }
  }

  override def addCJAnnotationFactoryForClass(clazz: ClassType, factory: Method): Unit = {
    typeToO2Class(clazz).addCJAnnotationFactory(methodToO2Method(factory))
  }

  override def addCJAnnotationFactoryForMethod(method: Method, factory: Method): Unit = {
    methodToO2Method(method).addCJAnnotationFactory(methodToO2Method(factory))
  }

  override def addCJAnnotationFactoryForField(field: Field, factory: Method): Unit = {
    fieldToO2Field(field).addCJAnnotationFactory(methodToO2Method(factory))
  }

  override def addCJAnnotationFactoriesForParameters(method: Method, factories: Array[Method]): Unit = {
    val o2Factories = factories.map(f => if (f == null) null else methodToO2Method(f))
    methodToO2Method(method).addCJAnnotationFactoriesForParameters(o2Factories)
  }

  override def addVArray(pkg: Type, name: String, elemType: SignatureType): ClassType = {
    val alreadyAdded = findClass(name)
    if (alreadyAdded != null) {
      return asClassType(alreadyAdded)
    }

    val varray = addClass0(pkg, XString(name), Modifiers(PUBLIC), isCangjie = true, genericInfo = GenericInfo.none)
    varray.markAsRecord()
    varray.markAsVArray()

    varray.setCangjieArrayElementType(elemType)

    classByO2Object(varray) ensuring (findClass(name) == _)
  }

  private def findDuplicateField(name: String) = {
    val field = dupClassToCheck.findDeclaredFieldOrNull(XString(name))
    if (field == null) {
      duplicateError("Field " + name + " is not found")
    }
    fieldsToCheck.remove(field)
    field
  }

  private def findDuplicateMethod(name: String, sig: MethodSignature, modifiers: Modifiers, exportedName: String) = {
    val method = dupClassToCheck.findDeclaredMethodOrNull(XString(name), sig)
    if (method == null) {
      duplicateError(s"method $name${sig.toJETSignature} is not found")
    }
    // TODO: check all modifiers
    if (method.isAbstract ^ (modifiers contains ABSTRACT)) {
      duplicateError(s"method ${method.getFullName} is ${if (method.isAbstract) "" else "not "}abstract")
    }
    if (exportedName != null) {
      if (!method.isExported) {
        duplicateError(s"method ${method.getFullName} is not exported")
      }
      val mExName = method.getExportedName
      if (mExName == null || !mExName.equals2(exportedName)) {
        duplicateError(s"method ${method.getFullName} has different exported name: ${mExName}")
      }
    } else if (method.isExported) {
      duplicateError(s"method ${method.getFullName} is exported")
    }
    methodsToCheck.remove(method)
    method
  }
}
