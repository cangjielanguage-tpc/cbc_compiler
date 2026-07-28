/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.hlir.interop.java

import com.huawei.excelsior.common.CodeHelpers.{notImplemented, shouldNotReachHere}
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.cangjie.interop.java.JavaSymbols
import com.huawei.excelsior.jet.compiler.cangjie.interop.java.symlevel.JavaSymbolsImpl
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.Ref
import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.Ref.{Annotation, JavaAnnotation, JavaAnnotations}
import com.huawei.excelsior.jet.compiler.hlir.{HLIRErrorReporter, HLIRMetadata, HLIRSymLevelResolver}
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{ABSTRACT, INTERFACE, STATIC}
import com.huawei.excelsior.jet.compiler.symlevel.{ConstValues, MethodSignature, SignatureType}

import scala.annotation.nowarn
import scala.collection.mutable.ListBuffer

class HLIRJavaSymbols(hlir: HLIRMetadata, resolver: HLIRSymLevelResolver)(implicit val typeProvider: TypeProvider, reporter: HLIRErrorReporter)
  extends JavaSymbols { self =>

  private val symlevelJavaSymbols = new JavaSymbolsImpl(typeProvider)

  override type Type = JavaSymbols.Type
  override type Class = JavaSymbols.Class
  override type Array = JavaSymbols.Array

  override def objectType: Class = {
    symlevelJavaSymbols.objectType
  }

  override def arrayType(baseType: Type, dimNum: Int = 1): Array = baseType match {
    case baseType: JavaSymbolsImpl.Type => symlevelJavaSymbols.arrayType(baseType, dimNum)
    case baseType: TypeImpl => notImplemented("java array of HLIR ref", baseType.ref.md)
  }

  abstract class TypeImpl(val ref: Ref.Type) extends JavaSymbols.Type {
    override def descriptor: String = SignatureType.toJBCSignature(resolver.symType(ref).get)
  }

  object TypeImpl {
    @nowarn("msg=match may not be exhaustive")
    def apply(ref: Ref.Type): TypeImpl = ref match {
      case ref: Ref.JavaClass => new JavaClassImpl(ref)
      case ref: Ref.JavaInterface => new JavaInterfaceImpl(ref)
    }
  }

  sealed trait HasRefAnnotations extends JavaSymbols.HasAnnotations {
    def ref: Ref

    protected override final def annotations: IndexedSeq[Ref.Annotation] = ref match {
      case withAnnotations: Ref.HasAnnotations => withAnnotations.annotations.toIndexedSeq
      case _ => IndexedSeq.empty
    }
  }

  abstract class ClassOrInterfImpl(ref: Ref.Type) extends TypeImpl(ref) with JavaSymbols.Class with HasRefAnnotations {
    require(ref.isInstanceOf[Ref.JavaClass] || ref.isInstanceOf[Ref.JavaInterface])

    override type Class = JavaSymbols.Class
    override type Method = MethodImpl
    override type Field = FieldImpl

    override def name: String = resolver.symName(ref.asInstanceOf[Ref.HasName])
    override def packageName: String = {
      val idx = name.lastIndexOf('.')
      if (idx > 0) name.substring(0, idx) else ""
    }
    override def signature = ref.asInstanceOf[Ref.HasJavaSignatureAttribute].javaSignatureAttribute.getOption
  }

  def classByRef(ref: Ref): JavaSymbols.Class = ref match {
    case ref: Ref.JavaClass if ref.classDef.initialized => new JavaClassImpl(ref)
    case ref: Ref.JavaInterface if ref.interfaceDef.initialized => new JavaInterfaceImpl(ref)
    case _ => JavaSymbolsImpl.Class(resolver.symType(ref).get)
  }

  def methodSignature(ref: Ref.HasSignature): MethodSignature = {
    resolver.functionSignature(ref, vararg = false)
  }

  def typeSignature(ref: Ref.HasSignature): SignatureType = {
    resolver.typeSignature(ref)
  }

  class JavaClassImpl(ref: Ref.JavaClass) extends ClassOrInterfImpl(ref) {
    private val classDef = ref.classDef.get

    override def accessFlags: Int =
      (resolver.symModifiers(classDef.modifiers, allowOpen = true) & Modifiers.JBC.publicClassMask).value

    override def isInterface: Boolean = false
    override def declaredMethods: Seq[MethodImpl] = classDef.members collect { case ref: Ref.MethodDef => new MethodImpl(ref) }
    override def declaredFields: Seq[FieldImpl] = classDef.members collect { case ref: Ref.Field => new FieldImpl(ref) }
    override def superClass: JavaSymbols.Class = classDef.superclass.map(classByRef).orNull
    override def declaredSuperInterfaces: Seq[JavaSymbols.Class] = classDef.superinterfaces.map(classByRef)
  }

  class JavaInterfaceImpl(ref: Ref.JavaInterface) extends ClassOrInterfImpl(ref) {
    private val interfaceDef = ref.interfaceDef.get

    override def accessFlags: Int =
      ((resolver.symModifiers(interfaceDef.modifiers, allowOpen = true, allowFinal = false) & Modifiers.JBC.publicInterfaceMask) +
        INTERFACE + ABSTRACT).value

    override def isInterface: Boolean = true
    override def declaredMethods: Seq[MethodImpl] = interfaceDef.members collect { case ref: Ref.MethodDef => new MethodImpl(ref) }
    override def declaredFields: Seq[FieldImpl] = Seq.empty
    override def superClass: JavaSymbols.Class = null
    override def declaredSuperInterfaces: Seq[JavaSymbols.Class] = interfaceDef.superinterfaces.map(classByRef)
  }

  class MemberImpl[T <: Ref.MemberDef](val ref: T) extends JavaSymbols.Member with HasRefAnnotations {
    override type Class = JavaSymbols.Class

    override def name: String = resolver.symName(ref)
    override def javaModifiersValue: Int = {
      val allowOpen = this.isInstanceOf[JavaSymbols.Method]
      val allowFinal = !declaringClass.isInterface
      val modifiers = resolver.symModifiers(ref.modifiers.get, allowOpen, allowFinal)
      (ref match {
        case _: Ref.StaticMethod | _: Ref.StaticField => modifiers + STATIC
        case _ => modifiers
      }).value
    }

    override def declaringClass: JavaSymbols.Class = classByRef(ref.refType)
    override def signature = ref.javaSignatureAttribute.getOption
  }

  class MethodImpl(_ref: Ref.MethodDef) extends MemberImpl(_ref) with JavaSymbols.Method {
    override def methodType: JavaSymbols.MethodType = {
      val sig = resolver.functionSignature(ref, vararg = false)
      JavaSymbols.MethodType(sig.parameterTypes.map(JavaSymbolsImpl.Type.apply), JavaSymbolsImpl.Type(sig.returnType))
    }

    final def parameters: collection.Seq[Ref.Parameter] = ref.parameters
  }

  class FieldImpl(_ref: Ref.Field) extends MemberImpl(_ref) with JavaSymbols.Field {
    override def constValue: Option[ConstValues.ConstValue] = {
      for {
        global <- resolver.global(ref)
        if global.ty.isInteger || global.ty.isFloatingPoint
        initValue <- hlir.module.getConstValue(global.initVarIdx)
      } yield ConstValues(resolver.typeSignature(ref), initValue)
    }

    override def tpe: JavaSymbols.Type = JavaSymbolsImpl.Type(resolver.typeSignature(ref))
  }
}
