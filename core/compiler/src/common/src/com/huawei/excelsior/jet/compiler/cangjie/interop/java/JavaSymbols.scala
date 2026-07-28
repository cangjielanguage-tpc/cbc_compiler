/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie.interop.java

import com.huawei.excelsior.jet.compiler.hlir.HLIRMetadata.Ref.{Annotation, JavaAnnotation, JavaAnnotations}
import com.huawei.excelsior.jet.compiler.ir.Modifiers
import com.huawei.excelsior.jet.compiler.ir.Modifiers.Modifier.{ABSTRACT, PRIVATE, PROTECTED, PUBLIC, STATIC}
import com.huawei.excelsior.jet.compiler.symlevel.ConstValues.ConstValue

trait JavaSymbols {
  type Type <: JavaSymbols.Type
  type Class <: JavaSymbols.Class & Type
  type Array <: JavaSymbols.Array & Type

  def objectType: Class
  def arrayType(baseType: Type, dimNum: Int = 1): Array
}

object JavaSymbols {
  trait Type {
    def descriptor: String // as defined in JVMS-4.3
  }

  trait HasAnnotations {
    protected def annotations: IndexedSeq[Annotation]

    final def javaAnnotations: Seq[(Boolean, JavaAnnotation)] = annotations.flatMap {
      case x @ JavaAnnotations(_, values) => values.map((x.isRuntimeVisible, _))
      case _ => Iterable.empty
    }
  }

  trait HasSignatureAttribute {
    def signature: Option[String] // as defined in JVMS-4.7.9
  }

  trait Array extends Type

  trait Primitive extends Type

  trait Class extends Type with HasAnnotations with HasSignatureAttribute {

    type Class <: JavaSymbols.Class
    type Method <: JavaSymbols.Method
    type Field <: JavaSymbols.Field

    /** Returns type name, packages are '/'-separated. Example: `java/util/List$Iterator` */
    def name: String
    /** Returns '/'-separated package name, or empty string if the class is not part of any package. */
    def packageName: String
    def accessFlags: Int
    def isInterface: Boolean

    def declaredMethods: Seq[Method]
    def declaredFields: Seq[Field]
    def superClass: JavaSymbols.Class
    def declaredSuperInterfaces: Seq[JavaSymbols.Class]
  }

  trait Member extends HasAnnotations with HasSignatureAttribute {

    type Class <: JavaSymbols.Class

    def name: String
    def javaModifiersValue: Int
    def javaModifiers: Modifiers = Modifiers(javaModifiersValue)
    def declaringClass: Class

    final def isPublic    : Boolean = javaModifiers contains PUBLIC
    final def isProtected : Boolean = javaModifiers contains PROTECTED
    final def isPrivate   : Boolean = javaModifiers contains PRIVATE
    final def isStatic    : Boolean = javaModifiers contains STATIC
  }

  trait Field extends Member {
    def constValue: Option[ConstValue]

    def tpe: Type

    override def toString = s"$tpe $name"
  }

  trait Method extends Member {
    def methodType: MethodType

    final def isAbstract: Boolean = javaModifiers contains ABSTRACT

    override def toString = s"$name$methodType"
  }

  case class MethodType(paramTypes: Seq[Type], returnType: Type) {
    assert(!paramTypes.contains(null))
    assert(returnType != null)

    def descriptor: String = { // as defined in JVMS-4.3.3
      s"(${paramTypes.map(_.descriptor).mkString})${returnType.descriptor}"
    }

    override def toString = s"(${paramTypes.mkString(", ")})$returnType"
  }
}
