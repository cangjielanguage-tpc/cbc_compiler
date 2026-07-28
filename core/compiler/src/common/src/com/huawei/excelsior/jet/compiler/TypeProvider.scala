/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.common.Language.{JAVA, SCALA}
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.assembler.AsmType
import com.huawei.excelsior.jet.classfile.SignatureParser
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{addressSize, isStandalone, languagePack}
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{JavaArray, Primitive, Record}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.{BYTE, CHAR, DOUBLE, FLOAT, INT, LONG, SHORT}
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, MethodSignature, SignatureType, Type, TypeKind}

trait TypeProvider {

  /** Return iterator over all classes. */
  def getAllClasses: Iterator[ClassType]

  /** Returns primitive type of given kind. */
  def getPrimitiveType(typeKind: TypeKind): Type

  def getPrimitiveType(asm: AsmType): Type = {
    val typeKind = (asm: @unchecked) match {
      case AsmType.I8 |
           AsmType.U8  => BYTE
      case AsmType.F16 | // Because F16 is treated like IntType in IR types, and we don't have it's own SymType
           AsmType.I16 => SHORT
      case AsmType.U16 => CHAR
      case AsmType.U32 |
           AsmType.I32 => INT
      case AsmType.U64 |
           AsmType.I64 => LONG
      case AsmType.F32 => FLOAT
      case AsmType.F64 => DOUBLE
      case AsmType.PTR => addressSize match {
        case 4 => INT
        case 8 => LONG
      }
    }

    getPrimitiveType(typeKind)
  }

  def getVoidType: Type = getPrimitiveType(TypeKind.VOID)

  def get1DimArrayType(elemTypeKind: TypeKind): ClassType = {
    assert(languagePack.supports(JAVA) || languagePack.supports(SCALA))
    val elemType = if (elemTypeKind.isPrimitive) {
      getPrimitiveType(elemTypeKind)
    } else {
      if (languagePack == LanguagePack.SCALA) {
        getXScalaAnyRef
      } else {
        getObjectType
      }
    }
    getArrayType(elemType, 1)
  }

  /** Returns array type with given base type and number of dimensions. */
  def getArrayType(baseType: Type, dimNum: Int): ClassType
  def getArraySigType(baseType: Type, dimNum: Int): SignatureType = SignatureType.fromSymType(getArrayType(baseType, dimNum))
  def getObjectType: ClassType
  def getAJObjectType: ClassType
  def getFinalizableType: ClassType
  def getLockableAJObjectType: ClassType
  def getAJStringType: ClassType
  def getAJThrowableType: ClassType
  def getAJIteratorType: ClassType
  def getCloneableType: ClassType
  def getSerializableType: ClassType
  def getStringType: ClassType
  def getClassType: ClassType
  def getThrowableType: ClassType
  def getReferenceType: ClassType
  def getIteratorType: ClassType
  def getScalaIteratorType: ClassType
  def getScalaBoxesRunTimeType: ClassType
  
  def getJavaRefType: ClassType
  def getScalaRefType: ClassType
  def getCangjieRefType: ClassType

  def getXScalaAnyRef: ClassType
  def getXScalaString: ClassType
  def getXScalaClass: ClassType
  def getXScalaSerializable: ClassType

  def getAJArrayType(kind: BytecodeTypeKind): ClassType

  def getThinTypeType: ClassType
  def getPolyThinTypeType: ClassType
  def getParameterPassingLocationsType: ClassType

  def getCVarArgListDescType: ClassType

  def getBacktraceType: ClassType

  def isCangjieIterator(`type`: ClassType): Boolean

  def isCangjieWeakRef(`type`: Type): Boolean

  def getManagedEopType: ClassType

  def getAJWeakRefType: ClassType

  def getCompilerInterfaceType: ClassType

  /** Return `true` if given `type` resembles iterator type.
    *
    * Currently supported types comprise classes that implement [[java.util.Iterator]],
    * [[scala.collection.Iterator]], `aj.util.Iterator` and Cangjie iterators.
    * TODO: support more types and maybe move it somewhere else?
    */
  def isIteratorLike(`type`: Type): Boolean = {
    val scalaIteratorType = getScalaIteratorType
    `type`.isClassOrInterface &&
      (`type`.doesImplement(getIteratorType) || `type`.doesImplement(getAJIteratorType) ||
        (scalaIteratorType != null && `type`.doesImplement(scalaIteratorType)) ||
        isCangjieIterator(Type.asClassType(`type`)))
  }

  def isManagedEopUnderlyingType(sigType: SignatureType): Boolean = {
    !isStandalone && isManagedEopUnderlyingType(sigType.symType(this))
  }

  def isManagedEopUnderlyingType(symType: Type): Boolean = {
    !isStandalone && symType == getManagedEopType
  }

  /** Returns symlevel object of a type corresponding to given `name`
    * loaded by class loader with StringID `clsid`, which should be `null` for standard class loaders.
    */
  def getClassTypeByNameAndClassLoaderSID(name: String, clsid: String): ClassType

  /** Resolves Java type (array, class or interface) from `refType` by `name`,
    * which is either an array signature (potentially in '.'-separated form
    * or a proper class name in '/'-separated form.
    */
  def resolveJavaTypeByName(refType: ClassType, name: XString): ClassType = {
    if (name.charAt(0) == '[') {
      asClassType(resolveSingleElementSignature(name.replace('.', '/'), refType))(this)
    } else {
      resolveTypeByName(refType, name)
    }
  }

  /** Resolves type from `refType` by `name` in '/'-separated form. */
  def resolveTypeByName(refType: ClassType, name: XString): ClassType

  final def findClass(name: XString): ClassType = findClass(name, loadPDB = false)

  def findClass(name: XString, loadPDB: Boolean): ClassType

  def resolveTypeByName(refType: ClassType, name: XString, ignoreLinkageErrors: Boolean): ClassType = {
    resolveTypeByName(refType, name)
  }

  /** Returns [[SignatureParser]] over types in given signature.
    * If `refType` is `None`, then all types are parsed without resolve.
    * If `refType` is `Some(null)`, then all class entries are erased to [[java.lang.Object]].
    * Otherwise, class entries are resolved using [[resolveTypeByName]].
    */
  private def parseSignature(sig: XString, refTypeOpt: Option[ClassType]): SignatureParser[SignatureType] = new SignatureParser[SignatureType](sig) {

    override protected def parsePrimitive(arrayDim: Int, sigChar: Byte) = {
      val t = Primitive(TypeKind.fromBCSignatureChar(sigChar))
      if (arrayDim > 0) JavaArray(t, arrayDim) else t
    }

    override protected def parseClass(arrayDim: Int, name: XString): SignatureType = {
      def fromNameSig(): SignatureType = {
        SignatureType.JBCReference(name.toString)
      }
      val t = refTypeOpt match {
        case Some(refType) =>
          // we should not throw verify errors on signature parsing
          val symType = if (refType == null) getObjectType else resolveTypeByName(refType, name, ignoreLinkageErrors = true)
          if (symType == null) fromNameSig() else SignatureType.fromSymType(symType)
        case None =>
          fromNameSig()
      }
      if (arrayDim > 0) JavaArray(t, arrayDim) else t
    }
  }

  final def parseSingleElementSignature(sig: XString): SignatureType = {
    parseSignature(sig, None).singleElement
  }

  final def resolveSingleElementSignature(sig: XString, refType: ClassType): SignatureType = {
    parseSignature(sig, Some(refType)).singleElement
  }

  final def parseMethodSignature(sig: XString): MethodSignature = {
    val (paramTypes, returnType) = parseSignature(sig, None).asMethodSig
    MethodSignature(returnType, paramTypes)
  }

  final def eraseMethodSignature(sig: XString): MethodSignature = {
    val (paramTypes, returnType) = parseSignature(sig, Some(null)).asMethodSig
    MethodSignature(returnType, paramTypes)
  }

  final def resolveMethodSignature(sig: XString, refType: ClassType): MethodSignature = {
    val (paramTypes, returnType) = parseSignature(sig, Some(refType)).asMethodSig
    MethodSignature(returnType, paramTypes)
  }

  /** @see com.huawei.excelsior.jet.compiler.symlevel.Type#isTurboClinited
    */
  def getTurboClinitedClasses: Iterator[ClassType] = {
    getAllClasses.filter(_.isTurboClinited)
  }
}
