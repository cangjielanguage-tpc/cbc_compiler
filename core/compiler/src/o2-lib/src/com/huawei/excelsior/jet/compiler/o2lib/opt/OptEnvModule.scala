/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.opt

import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.codeemitter.SymbolInfo
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.languagePack
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodeTypeKind, MethodAccessKind}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.{CodeDefModule as cd, opAttrsModule as at, opDefModule as opDef, opStdModule as opStd}
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, JUtilModule as ju, NumerateModule as Numerate, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.{JBCPreprocessor, JavaClassParserModule as jcp}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{ClassID, ReplacementLibrary, CacheAPIModule as CacheAPI, JStringsModule as js, xiEnvModule as xiEnv}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{BuildXKRN, IgnoreResolveErrors, LogUnresolvedErrors}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.LightweightEnvironment.{classByO2Object, getO2Method, methodByO2Object}
import com.huawei.excelsior.jet.compiler.symlevel.impl.light.{JavaVerifier, LightweightEnvironment}
import com.huawei.excelsior.jet.compiler.symlevel.{FindMethodImplResult, MethodAJCallKind, MethodSearchError, SigPolyMethodID, TypeKind, ClassType as SymClassType, Method as SymMethod}
import com.huawei.excelsior.jet.compiler.verifier.VerificationError
import com.huawei.excelsior.jet.compiler.verifier.VerificationError.ExceptionKind.*
import com.huawei.excelsior.jet.compiler.{Env, RTSGlobal, RTSProc}
import xscala.util.{MathUtils, UByte, UShort}

import scala.PartialFunction.cond

object OptEnvModule {

  abstract class CachedConstantPool {

    private[OptEnvModule] var env: Env = _
    /*RO*/ var klass: pcO.Class = _ // current class
    /*RO*/ var classInfo: jcp.PtrClassInfo = _
    private[OptEnvModule] var resolvedObjs: Array[Object] = _ // cache of resolved objects; indexed by cpEntry

    def getBootstrapMethodArgsIndexes(index: Int): Array[UShort] = {
      val C = this.classInfo
      val attr = jcp.getAttribute(C, C.attribute, C.attributeCount.toInt, jcp.jstrBootstrapMethods).get
      attr.bootstrapMethods(this.cpIndex(index).toInt).args
    }

    def getBootstrapMethodIndex(index: Int): Int = {
      val C = this.classInfo
      val attr = jcp.getAttribute(C, C.attribute, C.attributeCount.toInt, jcp.jstrBootstrapMethods).get
      attr.bootstrapMethods(this.cpIndex(index).toInt).methodIndex.toInt
    }

    def getSignaturePolymorphicMethodID(methodIdx: Int): SigPolyMethodID = {
      assert(this.cpTag(methodIdx) == jcp.TagMethod.toByte || this.cpTag(methodIdx) == jcp.TagIMethod.toByte)

      val t = this.getMethodRefClass(methodIdx)
      if (!t.isInstanceOf[pcO.Class]) {
        return SigPolyMethodID.NONE
      }

      val methodName = this.cpString(this.cpIndexName(this.cpIndexName(methodIdx).toInt).toInt)
      val declaringClass = t.asInstanceOf[pcO.Class]

      import SigPolyMethodID.*

      if (CacheAPI.isThisClass(declaringClass, ClassID.MethodHandle)) {
        if (methodName.equals2("invoke")) {
          return INVOKE
        } else if (methodName.equals2("invokeExact")) {
          return INVOKE_EXACT
        } else if (methodName.equals2("invokeBasic")) {
          return INVOKE_BASIC
        } else if (methodName.equals2("linkToVirtual")) {
          return LINK_TO_VIRTUAL
        } else if (methodName.equals2("linkToStatic")) {
          return LINK_TO_STATIC
        } else if (methodName.equals2("linkToSpecial")) {
          return LINK_TO_SPECIAL
        } else if (methodName.equals2("linkToInterface")) {
          return LINK_TO_INTERFACE
        }
      }
      NONE
    }

    // Get the class of MethodRef cp entry (method)
    def getMethodRefClass(idx: Int): pc.SymType.Reference = {
      val tag = this.cpTag(idx)
      assert(tag == jcp.TagMethod.toByte || tag == jcp.TagIMethod.toByte)
      this.getRefType(this.cpIndex(idx).toInt)
    }

    // Get the class of FieldRef cp entry (field)
    def getFieldRefClass(idx: Int): pc.SymType.Reference = {
      assert(this.cpTag(idx) == jcp.TagField.toByte)
      this.getRefType(this.cpIndex(idx).toInt)
    }

    // Get the basic type of field
    def getFieldTypeKind(idx: Int): BytecodeTypeKind = {
      import BytecodeTypeKind.*
      this.cpString(this.cpIndex(this.cpIndexName(idx).toInt).toInt).charAt(0) match {
        case 'B' => BYTE
        case 'C' => CHAR
        case 'F' => FLOAT
        case 'D' => DOUBLE
        case 'I' => INT
        case 'J' => LONG
        case 'S' => SHORT
        case 'Z' => BOOLEAN
        case 'L' => CLASS
        case '[' => ARRAY
      }
    }

    def methodAccess(methodIdx: Int, akind: MethodAccessKind): Object = {
      val tag = this.cpTag(methodIdx)
      assert(tag == jcp.TagMethod.toByte || tag == jcp.TagIMethod.toByte)

      lazy val refClassAccess = this.getClassAndCheck(this.cpIndex(methodIdx).toInt)

      var o = this.getResolvedEntry(methodIdx)
      if (o == null) {
        o = this.resolveMethodEntry(methodIdx, refClassAccess)

        if (this.getAccessResult(o) == DEFERRED) {
          val nameIdx = this.cpIndexName(methodIdx)
          val name = this.cpString(this.cpIndexName(nameIdx.toInt).toInt)
          val cpSig = this.cpString(this.cpIndex(nameIdx.toInt).toInt)
          val scope = this.getTypeFromAccess(o).asInstanceOf[pcO.Class]
          // Note: we must resolve signature types with correct "from" class.
          //       Otherwise, SignatureType.calcSymType will try to find corresponding type without "from" class,
          //       and either fail because absent type has not been created, or will find incorrect type.
          val sig = O2Env.env.resolveMethodSignature(cpSig, classByO2Object(klass))
          val m = ju.insertAbsentMethod(this.klass, scope, name, sig, akind == MethodAccessKind.STATIC)
          assert(m != null)
          return this.makeDeferredAccess(methodIdx, m) // do not cache absent members
        }

        this.setResolvedEntry(methodIdx, o)
      }

      this.getAccessResult(o) match {
        case ERROR =>
          o
        case DEFERRED =>
          throw new AssertionError // should not reach here
        case OK =>
          lazy val refClass = this.getTypeFromAccess(refClassAccess).asInstanceOf[pcO.Class]
          val m0 = this.getMethodFromAccess(o)
          var m = m0
          // need to check fields of m to prevent case when method still in use after cleanMethodList is called
          assert(m.nameObj != null)

          if (akind == MethodAccessKind.SPECIAL && !checkConstructorScope(m, methodIdx)) {
            val nameIdx = cpIndexName(methodIdx).toInt
            val name = cpString(cpIndexName(nameIdx).toInt)

            val message = if (js.jstrInit != name) {
              m.getReadableName(need_class_name = true)
            } else {
              val clazz = getTypeFromAccess(getClassAndCheck(cpIndex(methodIdx).toInt)).asInstanceOf[pcO.Class]
              val sig = cpString(cpIndex(nameIdx).toInt)
              XString(s"$clazz.$name$sig")
            }

            return this.makeErrorAccess(methodIdx, NoSuchMethodError, message)
          }

          if ((akind == MethodAccessKind.STATIC) != m.isStatic) {
            return this.incompatibleClassChange(methodIdx, refClass)
          }

          if (akind == MethodAccessKind.INTERFACE && m.isPrivate) {
            return this.incompatibleClassChange(methodIdx, refClass)
          }

          if (akind == MethodAccessKind.SPECIAL) {    // perform special lookup procedure for invokespecial
            assert(!m.isStatic)
            val res = findMethodForInvokespecial(refClass.symType, methodByO2Object(m), klass.symType)
            res match {
              case FindMethodImplResult.Found(found) =>
                m = getO2Method(found)
                assert(!m.isAbstract)
              case FindMethodImplResult.Error(MethodSearchError.ABSTRACT_METHOD) =>
                return this.makeErrorAccess(methodIdx, AbstractMethodError, m.getReadableName(need_class_name = true))
              case FindMethodImplResult.Error(MethodSearchError.INCOMPATIBLE_CLASS_CHANGE) =>
                return this.incompatibleClassChange(methodIdx, m.getDeclaringClass)
              case FindMethodImplResult.Error(MethodSearchError.ILLEGAL_ACCESS) =>
                assert(false)
            }
          }

          if (m eq m0) {
            o
          } else {
            this.makeNormalAccess(methodIdx, m)
          }
      }
    }

    /** Performs lookup procedure as described in JVMS "6.5 invokespecial".
      *
      * See [[com.huawei.excelsior.jet.runtime.classload.resolve.Resolver.lookupInvokeSpecialTarget]].
      */
    def findMethodForInvokespecial(refClass: SymClassType, p: SymMethod, fromClass: SymClassType): FindMethodImplResult = {
      def result(m: SymMethod): FindMethodImplResult = {
        if (m.isAbstract) {
          FindMethodImplResult.Error(MethodSearchError.ABSTRACT_METHOD)
        } else {
          FindMethodImplResult.Found(m)
        }
      }

      val clazz = if (!p.isConstructor && !refClass.isInterface && (refClass != fromClass) && refClass.isAssignableFrom(fromClass)) {
        // && fromClass.getAccessFlags().isSuper()
        //    According JVM 8 Spec "6.5 invokespecial" this check is needed.
        //    However, "4.1 The ClassFile Structure" reads:
        //      in Java SE 8 and above, the Java Virtual Machine considers
        //      the ACC_SUPER flag to be set in every class file, regardless of
        //      the actual value of the flag in the class file and the version of the class file.
        fromClass.getSuperClassSym
      } else {
        refClass
      }

      val name = p.getXName
      val sig = p.getSignature

      locally {
        val m = clazz.findDeclaredMethodOrNull(name, sig)
        if (m != null && !m.isStatic) {
          return result(m)
        }
      }

      if (clazz.isClass) {
        var superclass = clazz.getSuperClassSym
        while (superclass != null) {
          val m = superclass.findDeclaredMethodOrNull(name, sig)
          if (m != null && !m.isStatic) {
            return result(m)
          }
          superclass = superclass.getSuperClassSym
        }
      } else {
        val m = O2Env.env.getObjectType.findDeclaredMethodOrNull(name, sig)
        if (m != null && !m.isStatic && m.isPublic) {
          return result(m)
        }
      }

      clazz.findMostSpecificDefaultMethod(p)
    }

    private def checkConstructorScope(m: pcO.Method, methodIdx: Int): Boolean = {
      val classIdx = this.cpIndex(methodIdx)
      val nameIdx = this.cpIndexName(methodIdx)
      val name = this.cpString(this.cpIndexName(nameIdx.toInt).toInt)

      if (js.jstrInit == name) {
        val o = this.getClassAndCheck(classIdx.toInt)
        assert(this.getAccessResult(o) == OK)
        val scope = this.getTypeFromAccess(o).asInstanceOf[pcO.Class]
        return m.getDeclaringClass eq scope
      }
      true
    }

    def resolveMethodEntry(methodIdx: Int, refClassAccess: Object): Object = {
      this.getAccessResult(refClassAccess) match {
        case ERROR =>
          this.makeErrorAccess2(methodIdx, refClassAccess)
        case DEFERRED =>
          val scope0 = this.getTypeFromAccess(refClassAccess)
          scope0 match {
            case _: pc.SymType.Array =>
              // TODO: correctly support ArrayType here: method declaring type should be resolved on invoke*** first run
              this.resolveMethodInScope(methodIdx, this.env.getHierarchyRootClass)
            case _: pcO.Class =>
              refClassAccess // MethodAccess depends on access kind; so we return type's deferred access
          }
        case OK =>
          this.resolveMethodInScope(methodIdx, this.getTypeFromAccess(refClassAccess))
      }
    }

    def resolveMethodInScope(methodIdx: Int, scope0: pc.SymType): Object = {
      var scope: pcO.Class = null
      var m: pcO.Method = null

      val tag = this.cpTag(methodIdx)

      val nameIdx = this.cpIndexName(methodIdx)
      val name = this.cpString(this.cpIndexName(nameIdx.toInt).toInt)
      val cpSig = this.cpString(this.cpIndex(nameIdx.toInt).toInt)
      val sig = if (languagePack == LanguagePack.SCALA || (languagePack.supports(JAVA) && O2Env.env.enabled(BuildXKRN))) {
        // Need to resolve types with JBCPreprocessor,
        // otherwise, SignatureType.Reference("java/lang/Object") will be created which does not exist in Scala.
        O2Env.env.resolveMethodSignature(cpSig, classByO2Object(klass))
      } else {
        O2Env.env.parseMethodSignature(cpSig)
      }

      scope0 match {
        case _: pc.SymType.Array =>
          scope = this.env.getHierarchyRootClass
        case scope0: pcO.Class =>
          scope = scope0
      }

      if ((tag == jcp.TagIMethod.toByte) != scope.isInterface) {
        return this.incompatibleClassChange(methodIdx, scope)
      }

      if (tag == jcp.TagIMethod.toByte) {
        m = getO2Method(scope.symType.findDeclaredMethodOrNull(name, sig))
        if (m == null) {
          m = getO2Method(scope.symType.findMethodOrNull(name, sig, m => m.isPublic && !m.isStatic))
          if (m == null) {
            // This additional lookup in root is required, because symlevel.Type.getSuperClass returns null for interfaces.
            // So the previous lookup above could not find public methods in hierarchy root (java.lang.Object, xscala.AnyRef etc.)
            // TODO: fix this mess with superclass semantics between different entities!
            //   - symlevel.Type.getSuperClass
            //     returns null for java.lang.Object/xscala.AnyRef and interfaces
            //     and *some* AJ unmanaged types, like Namespace, but not for Struct or Value
            //   - com.huawei.excelsior.jet.runtime.typedesc.ClassFileInfo.getSuperclass
            //     returns null only for java.lang.Object, and for interfaces returns java.lang.Object
            //   - ReferenceTypes.ClassOrInterfaceType.superclass
            //     returns null only for AJObject (for classes superclass is the same as cohenSuper)
            //     for interfaces returns corresponding language root (java.lang.Object, xscala.AnyRef)
            //     except for Cangjie (!!), where it for some reason returns AJObject
            m = getO2Method(env.getHierarchyRootClass.symType.findMethodOrNull(name, sig, m => m.isPublic && !m.isStatic))
          }
        }
      } else {
        m = getO2Method(scope.symType.findMethodOrNull(name, sig))
      }

      if (m == null) {
        this.makeErrorAccess(methodIdx, NoSuchMethodError, js.format("%S.%S%S", scope.name, name, cpSig))
      } else if (!this.checkAccess(m, scope)) {
        this.makeErrorAccess(methodIdx, IllegalAccessError, js.format("tried to access method %S from class %S", m.getReadableName(need_class_name = true), this.klass.name))
      } else {
        this.makeNormalAccess(methodIdx, m)
      }
    }

    def fieldAccess(fieldIdx: Int, isStatic: Boolean, isWrite: Boolean): Object = {
      assert(this.cpTag(fieldIdx) == jcp.TagField.toByte)
      var o = this.getResolvedEntry(fieldIdx)
      if (o == null) {
        o = this.resolveFieldEntry(fieldIdx)

        if (this.getAccessResult(o) == DEFERRED) {
          val nameIdx = this.cpIndexName(fieldIdx)
          val name = this.cpString(this.cpIndexName(nameIdx.toInt).toInt)
          val cpSig = this.cpString(this.cpIndex(nameIdx.toInt).toInt)
          val scope = this.getTypeFromAccess(o).asInstanceOf[pcO.Class] // TODO: support ArrayType here
          // Note: we must resolve signature types with correct "from" class.
          //       Otherwise, SignatureType.calcSymType will try to find corresponding type without "from" class,
          //       and either fail because absent type has not been created, or will find incorrect type.
          val sig = O2Env.env.resolveSingleElementSignature(cpSig, classByO2Object(klass))
          val f = ju.insertAbsentField(this.klass, scope, name, sig, isStatic)
          assert(f != null)
          return this.makeDeferredAccess(fieldIdx, f) // do not cache absent members
        }

        this.setResolvedEntry(fieldIdx, o)
      }

      this.getAccessResult(o) match {
        case ERROR =>
          o
        case DEFERRED =>
          throw new AssertionError // should not reach here
        case OK =>
          val f = this.getFieldFromAccess(o)
          if (isStatic != f.isStatic) {
            return this.incompatibleClassChange(fieldIdx, f.getDeclaringClass)
          }

          if (isWrite && f.isFinal && (this.klass ne f.getDeclaringClass)) {
            return this.makeErrorAccess(fieldIdx, IllegalAccessError, js.format("tried to put to final field %S in class %S", f.getReadableName(need_class_name = true, need_full_sign = false), this.klass.name))
          }

          o
      }
    }

    def resolveFieldEntry(fieldIdx: Int): Object = {
      val classIdx = this.cpIndex(fieldIdx)
      val o = this.getClassAndCheck(classIdx.toInt)

      this.getAccessResult(o) match {
        case ERROR =>
          this.makeErrorAccess2(fieldIdx, o)
        case DEFERRED =>
          o // FieldAccess depends on access kind; so we return type's deferred access
        case OK =>
          val nameIdx = this.cpIndexName(fieldIdx)
          val name = this.cpString(this.cpIndexName(nameIdx.toInt).toInt)
          val cpSig = this.cpString(this.cpIndex(nameIdx.toInt).toInt)

          val scope = this.getTypeFromAccess(o)
          scope match {
            case _: pc.SymType.Array =>
              this.makeErrorAccess(fieldIdx, NoSuchFieldError, name)
            case scope: pcO.Class =>
              val sig = if (languagePack == LanguagePack.SCALA || (languagePack.supports(JAVA) && O2Env.env.enabled(BuildXKRN))) {
                // Need to resolve types with JBCPreprocessor,
                // otherwise, SignatureType.Reference("java/lang/Object") will be created which does not exist in Scala.
                O2Env.env.resolveSingleElementSignature(cpSig, classByO2Object(klass))
              } else {
                O2Env.env.parseSingleElementSignature(cpSig)
              }
              val f = scope.findField(name, sig)
              if (f == null) {
                this.makeErrorAccess(fieldIdx, NoSuchFieldError, name)
              } else if (!this.checkAccess(f, scope)) {
                this.makeErrorAccess(fieldIdx, IllegalAccessError, js.format("tried to access field %S from class %S", f.getReadableName(need_class_name = true, need_full_sign = false), this.klass.name))
              } else {
                this.makeNormalAccess(fieldIdx, f)
              }
          }
      }
    }

    def checkAccess(o: pcO.Member, refClass: pcO.Class): Boolean = jcp.relaxVerify && !this.klass.isAnonymous || pcO.isMemberAccessibleNew(o, refClass, this.klass)
    // JET-10454: check access for lambda classes (anonymous) in AOT-compilers
    // even if relaxVerify. In HotSpot, the check is performed before lambda class
    // generation during resolving a method reference in bootstrap method call
    // for lambda creation indy call-site that does not check relax verify flag.
    // In AOT we should either not generate a lambda-class for this case or
    // at least check accessibility within generated lambda-class.
    // The same check in JIT is not needed as lambda-classes won't be generated
    // if the method reference is inaccessible
    // (the same bootstrap method code (java.lang.invoke.LambdaMetafactory)
    // will be performed before lambda-class generation).

    def incompatibleClassChange(memberIdx: Int, c: pcO.Class): Object = this.makeErrorAccess(memberIdx, IncompatibleClassChangeError, c.name)

    def getClassAndCheck(classIdx: Int): Object = {
      assert(this.cpTag(classIdx) == jcp.TagClass.toByte)
      var o = this.getResolvedEntry(classIdx)
      if (o == null) {
        o = this.resolveClassEntry(classIdx)
        this.setResolvedEntry(classIdx, o)
      }
      o
    }

    def resolveClassEntry(classIdx: Int): Object = {
      val t = this.getRefType(classIdx)
      val c = pcO.getCoreClassType(t)

      if (c != null) {
        Numerate.processClass(c)
        JavaVerifier { _.verify(c) }
        val verr = ju.throwsVerifyErrorAtFirstUse(c)
        if (verr != null) {
          return this.makeErrorAccess(classIdx, verr.errcode, verr.errmsg)
        } else if (!jcp.relaxVerify && !c.isAccessibleFrom(this.klass)) {
          return this.makeErrorAccess(classIdx, IllegalAccessError, js.format("tried to access class %S from class %S", c.name, this.klass.name))
        } else if (ju.checkTypeForAbsence(c)) {
          return this.makeDeferredAccess(classIdx, t)
        }
      }

      this.makeNormalAccess(classIdx, t)
    }

    def getFieldFromAccess(access: Object): pcO.Field
    def getMethodFromAccess(access: Object): pcO.Method
    def getTypeFromAccess(access: Object): pc.SymType
    def getAccessResult(access: Object): AccessResult
    def makeNormalAccess(cpEntry: Int, obj: pc.SymLevelObject): Object
    def makeDeferredAccess(cpEntry: Int, obj: pc.SymLevelObject): Object
    def makeErrorAccess2(cpEntry: Int, cause: Object): Object

    def checkIgnoreResolveErrors(errCode: VerificationError.ExceptionKind, errmsg: XString): Unit = {
      if (O2Env.env.enabled(LogUnresolvedErrors)) {
        println(s"UNRESOLVED: $errmsg at ${O2Env.env.currentDebugPosition}")
      }

      if (!O2Env.env.enabled(IgnoreResolveErrors) && !JBCPreprocessor.ignoreDeferred(errmsg.toString, klass)) {
        val err = errCode match {
          case NoSuchMethodError => XString(s"method $errmsg not found")
          case NoSuchFieldError => XString(s"field $errmsg not found")
          case AbstractMethodError => XString(s"method $errmsg is abstract")
          case IncompatibleClassChangeError => XString(s"class $errmsg is changed incompatibly")
          case _ => errmsg
        }

        xiEnv.errors.fault(ErrMsg020, err)
      }
    }

    def makeErrorAccess(cpEntry: Int, errCode: VerificationError.ExceptionKind, errmsg: XString): Object = {
      throw new AssertionError
    }

    def getRefType(idx: Int): pc.SymType.Reference = {
      this.klass.resolveClassRef(this.classInfo, idx)
    }

    // Get index in StringTable for constant string entry
    def getConstStringNumber(cpidx: Int): Int = {
      assert(this.cpTag(cpidx) == jcp.TagString.toByte)
      val str = this.cpString(this.cpIndex(cpidx).toInt)
      this.klass.getStringTable.getIndexByString(str)
    }

    // Get the double constant from the constant pool
    def getDouble(idx: Int): Double = {
      assert(idx >= 0)
      assert(this.cpTag(idx) == jcp.TagDouble.toByte)
      this.classInfo.constantPool(idx).longRealVal
    }

    // Get the long constant from the constant pool
    def getLong(idx: Int): Long = {
      assert(idx >= 0)
      assert(this.cpTag(idx) == jcp.TagLong.toByte)
      MathUtils.makeLong(this.classInfo.constantPool(idx).low, this.classInfo.constantPool(idx).high)
    }

    // Get the float constant from the constant pool
    def getFloat(idx: Int): Float = {
      assert(idx >= 0)
      assert(this.cpTag(idx) == jcp.TagFloat.toByte)
      this.classInfo.constantPool(idx).realVal
    }

    def getMethodHandleRefKind(idx: Int): Int = {
      assert(idx >= 0)
      assert(this.cpTag(idx) == jcp.TagMethodHandle.toByte)
      this.classInfo.constantPool(idx).low
    }

    // Get the int constant from the constant pool
    def getInt(idx: Int): Int = {
      assert(idx >= 0)
      assert(this.cpTag(idx) == jcp.TagInteger.toByte)
      this.classInfo.constantPool(idx).low
    }

    def getCodeAttribute(m: pcO.Method): jcp.PtrCodeInfo = {
      assert(!m.isAjReplaced && !m.isDeclaredNative)

      val C = this.classInfo
      jcp.getCode(C, C.method(m.getNumberInClassFile))
    }

    def cpString(cpEntry: Int): XString = {
      assert(cpEntry >= 0)
      assert(this.cpTag(cpEntry) == jcp.TagUtf8.toByte)
      this.classInfo.constantPool(cpEntry).bufferPtr
    }

    def cpIndexName(cpEntry: Int): UShort = {
      assert(cpEntry >= 0)
      this.classInfo.constantPool(cpEntry).indexName
    }

    def cpIndex(cpEntry: Int): UShort = {
      assert(cpEntry >= 0)
      this.classInfo.constantPool(cpEntry).index
    }

    def cpTag(cpEntry: Int): Byte = {
      assert(cpEntry >= 0)
      this.classInfo.constantPool(cpEntry).constantType
    }

    def setResolvedEntry(cpIdx: Int, obj: Object): Unit = {
      this.resolvedObjs(cpIdx) = obj
    }

    //------------------ CachedConstantPool ---------------------------
    def getResolvedEntry(cpIdx: Int): Object = this.resolvedObjs(cpIdx)

    def initConstantPool(env: Env, klass: pcO.Class): Unit = {
      this.env = env
      this.klass = klass

      this.classInfo = klass.classInfo
      assert(this.classInfo != null)

      this.resolvedObjs = new Array[Object](this.classInfo.constantPoolCount.toInt)
    }
  }


  class Env {
    private[OptEnvModule] var curClass: pcO.Class = _ // current class
    private[OptEnvModule] var objectClass: pcO.Class = _ // java.lang.Object class

    def getTypeKind(t: pc.SymType): TypeKind = {
      import TypeKind.*

      t match {
        case t: pc.SymType.JBC.Primitive => t.typeKind

        case _: pc.SymType.Array => ARRAY

        case t: pcO.Class =>
          if (t.isInterface) INTERFACE
          else if (t.isThinClass) THIN
          else if (t.isAJArray || t.isCangjieArray) ARRAY
          else if (t.isRecord) RECORD
          else CLASS
      }
    }

    //-------------------------------------------------------------
    def getAJReplacement(m: pcO.Method): pcO.Method = ReplacementLibrary.getReplacement(m).orNull

    def getTypeThrowMessage(t: pc.SymType): XString = {
      val vererr = this.getTypeVerifyError(t)
      vererr.errmsg
    }

    def getTypeThrowProc(t: pc.SymType): RTSProc = opStd.stdExceptionProc(getTypeVerifyError(t).errcode)

    def getTypeVerifyError(t: pc.SymType): pcO.VerifyError = {
      val c = pcO.getCoreType(t).asInstanceOf[pcO.Class]
      assert(c.isClassDefinitionError)
      c.getVerifyError
    }

    def isTypeErroneous(t: pc.SymType): Boolean = cond(pcO.getCoreType(t)) {
      case c: pcO.Class => !c.isShielded && c.isClassDefinitionError
    }

    def isTypeDeferred(tPar: pc.SymType): Boolean = cond(pcO.getCoreType(tPar)) {
      case c: pcO.Class => c.isShielded
    }

    //-------------------------------------------------------------
    def getAJCallKind(m: pcO.Method): MethodAJCallKind = {
      import MethodAJCallKind.*

      // TODO: uncomment: ASSERT(~m.getDeclaringClass().isShielded());
      if (m.getDeclaringClass.isShielded) {
        return NORMAL
      }

      if (m.isAjIndirectCall) {
        INDIRECT_CALL
      } else if (m.isThinUncheckedCast) {
        THIN_UNCHECKED_CAST
      } else if (m.isGetFlatThinIntrinsic) {
        GET_FLAT_THIN_INTRINSIC
      } else if (m.isAjCallToManaged) {
        CALL_TO_MANAGED
      } else if (m.isAjUncheckedCall) {
        UNCHECKED_CALL
      } else if (m.isAjUncheckedNew) {
        UNCHECKED_NEW
      } else if (methodByO2Object(m).getIntrinsicType != null) {
        INTRINSIC_CALL
      } else if (methodByO2Object(m).getIntrinsicWithBodyType != null) {
        INTRINSIC_WITH_BODY_CALL
      } else {
        NORMAL
      }
    }

    def getAccessKind(): SymbolInfo.AccessKind = {
      SymbolInfo.AccessKind.DIRECT
    }

    def getBuiltInFieldOffset(field: BuiltInField): Int = {
      val (classID, fieldName) = field match {
        case BuiltInField.METHOD_HANDLE_FORM      => (ClassID.MethodHandle, js.newJString("form"))
        case BuiltInField.LAMBDA_FORM_VMENTRY     => (ClassID.LambdaForm,   js.newJString("vmentry"))
        case BuiltInField.MEMBER_NAME_ENTRY_POINT => (ClassID.MemberName,   js.newJString("entryPoint"))
      }

      val cl = CacheAPI.getClass(classID)
      assert(cl != null)

      val f = cl.findField(fieldName, null)
      assert(f != null)

      f.asInstanceOf[pcO.InstanceField].getOffset
    }

    def getObjForStdSym(sym: RTSGlobal): pc.Symbol = opStd.dataSymbol(sym)

    def getObjForRTSProc(proc: RTSProc): pcO.Method = CacheAPI.getRTSProc(proc)

    def getHierarchyRootClass: pcO.Class = {
      if (this.objectClass == null) {
        this.objectClass = if (languagePack == LanguagePack.SCALA) {
          CacheAPI.getClass(ClassID.XScalaAnyRef)
        } else {
          CacheAPI.getClass(ClassID.Object)
        }
        assert(this.objectClass != null && !this.objectClass.isUnavailable)
      }
      this.objectClass
    }

    def getTypeByNameAndClassLoaderSID(name: XString, clsid: XString): pcO.Class = pcO.findClass(name, tryAbsent = true, clsid, tryLambda = true)

    // Makes a new data segment with given contents and places it into read-only data.
    def makeConstData(value: Array[Byte], length: Int, align: Int): pc.Symbol = {
      val obj = at.newUnsizedConst(js.format("$$constData"))
      at.setSegment(obj, cd.makeSeg(align, value, length))
      obj
    }

    // Make new zero terminated string encoded in UTF-8 or UTF-16 form and placed into read-only data
    def makeConstStringData(str: XString, bstr: Boolean): pc.Symbol = opDef.getStrConst(str, bstr)

    def isBytecodeAvailable(c: pcO.Class): Boolean = pcO.canLoadClass(c)

    def exitClass(): Unit = {
      this.curClass = null
      this.objectClass = null
    }

    def enterClass(class0: pcO.Class): Unit = {
      this.curClass = class0
      assert(this.curClass.equals(at.currClass))
      this.objectClass = null
    }
  }

  type AccessResult = UByte
  val OK: AccessResult = UByte(0)
  val ERROR: AccessResult = UByte(1)
  val DEFERRED: AccessResult = UByte(2)


  //  =============================================
  //  S y m   l e v e l   s e r i a l i z a t i o n
  //  =============================================

  abstract class O2SymlevelWriter {

    def writeClassSymRef(klass: pcO.Class, contextClass: pcO.Class): Unit = {
      if (contextClass.equals(klass)) {
        putInt(CURR_CLASS_FLAG)
      } else {
        assert(klass != null)

        val importNumber = contextClass.getPersistentImportIndex(klass)
        if (importNumber == -1) {
          putInt(FULL_NAME_FLAG)
          pcNames.writeName(klass.nameObj)(putInt, putXString)

        } else {
          assert(importNumber >= 0)
          putInt(FROM_IMPORT_FLAG)
          putInt(importNumber)
        }
      }
    }

    def putXString(x: XString): Unit
    def putInt(x: Int): Unit
  }


  abstract class O2SymlevelReader {

    def readClassSymRef(contextClass: pcO.Class, allowAbsenceOfExternalRefs: Boolean): pcO.Class = {
      val referenceType = this.nextInt()
      referenceType match {
        case CURR_CLASS_FLAG =>
          contextClass
        case FROM_IMPORT_FLAG =>
          val klass = contextClass.getClassByPersistentImportIndex(this.nextInt())
          assert(klass != null && (klass ne contextClass))
          klass
        case FULL_NAME_FLAG =>
          val name = pcNames.readName(nextInt, nextXString)
          var klass = pcO.findClassByNameObject(name)
          if (klass == null) {
            klass = O2Env.env.findO2Class(name.name, loadPDB = true)
          }

          assert(allowAbsenceOfExternalRefs || klass != null,
            // Note: currently runtime classes that implement Cangjie intrinsics must be listed
            //       in HLIRSymleveBuilder.runtimeImplTypes as a workaround for JET-15600.
            s"could not find class ${name.name} in context $contextClass")
          klass
      }
    }

    def nextXString(): XString
    def nextInt(): Int
  }

  //-------------------------------------------------------------

  // Constants used both in symlevel writer & reader.
  private val CURR_CLASS_FLAG: Int = 0
  private val FROM_IMPORT_FLAG: Int = 1
  private val FULL_NAME_FLAG: Int = 2

  //-------------------------------------------------------------
  def hostClassloaderID(hostClass: pcO.Class): XString = hostClass.nameObj.getClassloaderID

  def findLambdaClass(hostClass: pcO.Class, clazz: XString): pcO.Class = {
    val lambdaClass = pcO.findClassByNameObject(pcNames.newLambdaClassName(clazz, hostClassloaderID(hostClass)))
    assert(lambdaClass == null || lambdaClass.hostClass == hostClass)
    lambdaClass
  }

  def reportStatus(stage: XString, methodName: XString): Unit = {
    if (xiEnv.decor contains xiEnv.dc_header) {
      val str = js.format("%S %S", stage, methodName)

      xiEnv.printWithErasingPrevious(str)
    }
  }

  def reportPGOFailure(methodName: XString, fatal: Boolean): Unit = {
    xiEnv.mute { xiEnv.printWithErasingPrevious(js.jstrEmpty) }

    if (fatal) {
      xiEnv.errors.fault(ErrMsg684, methodName)
    } else if (xiEnv.decor contains xiEnv.dc_warnings) {
      xiEnv.errors.silentMessage(ErrMsg683, methodName)
    }
  }

  def reportWarning(str: XString): Unit = {
    if (xiEnv.decor contains xiEnv.dc_warnings) {
      xiEnv.info.forcePrint(js.TODO2(str))
    }
  }
}
