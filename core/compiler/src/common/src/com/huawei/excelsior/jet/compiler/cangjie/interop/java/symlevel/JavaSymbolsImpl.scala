/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie.interop.java.symlevel

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.cangjie.interop.java.JavaSymbols
import com.huawei.excelsior.jet.compiler.cangjie.interop.java.JavaSymbols.MethodType
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.Ref.Annotation
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.ConstValue
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{JBCSignature, SignatureType, Field as SymField, Member as SymMember, Method as SymMethod, Type as SymType}

class JavaSymbolsImpl(typeProvider: TypeProvider) extends JavaSymbols {
  override type Type = JavaSymbolsImpl.Type
  override type Class = JavaSymbolsImpl.Class
  override type Array = JavaSymbolsImpl.Array

  override def objectType: Class = {
    JavaSymbolsImpl.Class(typeProvider.getObjectType)(typeProvider)
  }

  override def arrayType(baseType: Type, dimNum: Int = 1): Array = {
    new Array(SignatureType.JavaArray(baseType.sigType, dimNum), typeProvider)
  }
}

object JavaSymbolsImpl {
  abstract class Type(val sigType: SignatureType, protected[JavaSymbolsImpl] implicit val typeProvider: TypeProvider) extends JavaSymbols.Type {
    assert(sigType != null)

    final def descriptor: String = JBCSignature(sigType)

    override final def equals(other: Any): Boolean = other match {
      case that: AnyRef if this eq that => true
      case that: Type => sigType == that.sigType
      case _ => false
    }

    override final def hashCode() = sigType.##

    override final def toString = sigType.toString
  }

  object Type {

    def apply(tpe: SymType)(implicit typeProvider: TypeProvider): JavaSymbols.Type = if (tpe != null) apply(SignatureType.fromSymType(tpe)) else null
    def apply(tpe: SignatureType)(implicit typeProvider: TypeProvider): JavaSymbols.Type = {
      if (tpe != null) {
        if (typeProvider != null) {
          assert(!tpe.isThinClass, tpe)
          assert(!tpe.isArray || tpe.symType.isJavaArray, tpe)
        }
        SignatureType.Wrapper.skip(tpe) match {
          case array: SignatureType.JavaArray => new Array(array, typeProvider)
          case primitive: SignatureType.Primitive =>
            // Erase Cangjie primitive to Java primitive.
            // Otherwise method or field signatures will not match with Java ones.
            new Primitive(SignatureType.Primitive(primitive.jbcKind), typeProvider)
          case ref: SignatureType.JBCReference => new Class(ref, typeProvider)
          case _: SignatureType.Record |
               _: SignatureType.CangjieReference |
               _: SignatureType.ArraySlice |
               _: SignatureType.CangjieArray |
               SignatureType.BString |
               _: SignatureType.CPointer |
               _: SignatureType.VArray |
               _: SignatureType.InstantiatedReference |
               _: SignatureType.InstantiatedRecord |
               _: SignatureType.Tuple |
               _: SignatureType.Box |
               _: SignatureType.TypeVariable |
               SignatureType.ThisTypeInfo =>
            shouldNotReachHere(tpe)
        }
      } else {
        null
      }
    }
  }

  sealed trait NoHLIRInfo extends JavaSymbols.HasAnnotations with JavaSymbols.HasSignatureAttribute {
    override final protected def annotations: IndexedSeq[Annotation] = IndexedSeq.empty
    override final def signature = None
  }

  final class Array private[JavaSymbolsImpl](_sigType: SignatureType.JavaArray, _typeProvider: TypeProvider) extends Type(_sigType, _typeProvider) with JavaSymbols.Array

  final class Primitive private[JavaSymbolsImpl](_sigType: SignatureType.Primitive, _typeProvider: TypeProvider) extends Type(_sigType, _typeProvider) with JavaSymbols.Primitive

  final class Class private[JavaSymbolsImpl](_sigType: SignatureType.JBCReference, _typeProvider: TypeProvider) extends Type(_sigType, _typeProvider) with JavaSymbols.Class with NoHLIRInfo {

    override type Class = JavaSymbolsImpl.Class
    override type Method = JavaSymbolsImpl.Method
    override type Field = JavaSymbolsImpl.Field

    private[JavaSymbolsImpl] lazy val sym = asClassType(_sigType)

    assert(sigType.isClass || sigType.isInterface)

    // TODO: support classloaders: equality, hashCode
    assert(sym.getClassLoaderSID == null)

    override lazy val name = sym.getName
    override lazy val packageName = sym.getPackageName
    override lazy val accessFlags = sym.getAccessFlags
    override def isInterface = sym.isInterface
    override def superClass = Class(sym.getSuperClassSig)
    override def declaredSuperInterfaces = (sym.getDeclaredSuperInterfacesSym map (Class(_))).toSeq
    override lazy val declaredMethods = sym.getDeclaredMethods.map[Method](new MemberMethod(this, _)).toSeq
    override lazy val declaredFields = (sym.getDeclaredFields map (new Field(this, _))).toSeq
  }

  object Class {

    def apply(tpe: SymType)(implicit typeProvider: TypeProvider): JavaSymbolsImpl.Class = Type(tpe).asInstanceOf[Class] // TODO super remove later
    def apply(tpe: SignatureType)(implicit typeProvider: TypeProvider): JavaSymbolsImpl.Class = Type(tpe).asInstanceOf[Class]
  }

  abstract class Member[T <: SymMember](override val declaringClass: Class, val sym: T) extends JavaSymbols.Member with NoHLIRInfo {

    override type Class = JavaSymbolsImpl.Class

    assert(declaringClass != null)
    assert(sym != null)

    protected implicit val typeProvider: TypeProvider = declaringClass.typeProvider

    assert(sym.getDeclaringClass == declaringClass.sym)

    override val name: String = sym.getName

    override val javaModifiersValue: Int = sym.getJavaModifiersValue
  }

  trait Method extends JavaSymbols.Method

  case class RawMethod(declaringClass: Class, name: String, methodType: MethodType, javaModifiersValue: Int) extends Method with NoHLIRInfo {
    override type Class = JavaSymbolsImpl.Class
  }

  private final class MemberMethod private[JavaSymbolsImpl](declaringClass: Class, sym: SymMethod) extends Member[SymMethod](declaringClass, sym) with Method {

    override lazy val methodType: MethodType =
      val receiverIndex = if (sym.hasReceiverParameter) sym.getReceiverArgIdx else -1
      MethodType(
        paramTypes = (0 until sym.getParamsCount) collect { case i if i != receiverIndex => Type(sym.getParamType(i)) },
        returnType = Type(sym.getReturnType)
      )
  }

  final class Field private[JavaSymbolsImpl](declaringClass: Class, sym: SymField) extends Member[SymField](declaringClass, sym) with JavaSymbols.Field {

    override def tpe = Type(sym.getType)

    override lazy val constValue: Option[ConstValue] = if (sym.hasInitialValue) Some(sym.getInitialValue) else None

  }
}
