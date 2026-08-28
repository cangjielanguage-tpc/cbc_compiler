/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.google.flatbuffers.Table
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.*

import java.nio.ByteBuffer
import scala.reflect.ClassTag


case class ClassDefImpl() extends CHIR.ClassDef {
  override def packageName(): String = ???

  override def identifier(): String = ???

  override def srcCodeIdentifier(): String = ???

  override def instanceVars(): Seq[CHIR.InstanceVar] = ???

  override def staticVars(): Seq[CHIR.GlobalVar] = ???

  override def methods(): Seq[CHIR.Func] = ???

  override def attributes(): Seq[CHIR.Attribute] = ???

  override def tpe(): CHIR.ClassType = ???

  override def annotations(): Seq[CHIR.Annotation] = ???

  override def vTables(): Seq[CHIR.VTable] = ???

  override def isClass(): Boolean = ???

  override def implementedInterfaces(): Seq[CHIR.ClassType] = ???

  override def superClass(): Option[CHIR.ClassType] = ???
}

final case class FuncTypeImpl() extends CHIR.Type

/** [[CHIRPackage]] with caching of core indexed entities:
  *  - Types
  *  - Values
  *  - Exprs
  *  - Defs
  */
class PackageImpl(source: String) extends CHIR.Package {

  val pkg: CHIRPackage = {
    // TODO: fix performance regression of XScala IO compared to JDK IO
    val bytes = java.nio.file.Files.readAllBytes(java.io.File(source).toPath)
    val buf = ByteBuffer.wrap(bytes)
    CHIRPackage.getRootAsCHIRPackage(buf)
  }

  private lazy val _types: Seq[CHIR.Type]  = {
    for (i <- 0 until pkg.defsLength()) yield {
      val obj = pkg.typesType(i) match {
        case TypeElem.Type => new Type
        case TypeElem.RawArrayType => new RawArrayType
        case TypeElem.VArrayType => new VArrayType
        case TypeElem.FuncType => new FuncType
        case TypeElem.CustomType => new CustomType
        case TypeElem.GenericType => new GenericType
      }
      val t = pkg.types(obj, i)
      FuncTypeImpl()
    }
  }
  
  private val _values = Array.fill[Table](pkg.valuesLength)(null)
  private val exprs  = Array.fill[Table](pkg.exprsLength)(null)
  private lazy val _typeDefs: Seq[CHIR.CustomTypeDef] = {
    for (i <- 0 until pkg.defsLength()) yield {
      val obj = pkg.defsType(i) match {
        case CustomTypeDefElem.EnumDef => new EnumDef
        case CustomTypeDefElem.StructDef => new StructDef
        case CustomTypeDefElem.ClassDef => new ClassDef
        case CustomTypeDefElem.ExtendDef => new ExtendDef
      }
      val d = pkg.defs(obj, i)
      ClassDefImpl()
    }
  }
//
//  /** Returns cached Type or null if id is zero or negative. */
//  def getType[T >: Null <: Table : ClassTag](id: Long): T = {
//    if (id <= 0) {
//      null
//    } else {
//      val i = id.toInt - 1
//      if (types(i) == null) {
//        val obj = pkg.typesType(i) match {
//          case TypeElem.Type => new Type
//          case TypeElem.RawArrayType => new RawArrayType
//          case TypeElem.VArrayType => new VArrayType
//          case TypeElem.FuncType => new FuncType
//          case TypeElem.CustomType => new CustomType
//          case TypeElem.GenericType => new GenericType
//        }
//        types(i) = pkg.types(obj, i)
//      }
//      types(i).asInstanceOf[T]
//    }
//  }

  /** Returns cached Value or null if id is zero or negative. */
  def getValue[T >: Null <: Table : ClassTag](id: Long): T = {
    if (id <= 0) {
      null
    } else {
      val i = id.toInt - 1
      if (_values(i) == null) {
        val obj = pkg.valuesType(i) match {
          case ValueElem.BoolLiteral => new BoolLiteral
          case ValueElem.RuneLiteral => new RuneLiteral
          case ValueElem.StringLiteral => new StringLiteral
          case ValueElem.IntLiteral => new IntLiteral
          case ValueElem.FloatLiteral => new FloatLiteral
          case ValueElem.UnitLiteral => new UnitLiteral
          case ValueElem.NullLiteral => new NullLiteral
          case ValueElem.Parameter => new Parameter
          case ValueElem.LocalVar => new LocalVar
          case ValueElem.GlobalVar => new GlobalVar
//          case ValueElem.Function => new Function
          case ValueElem.Block => new Block
          case ValueElem.BlockGroup => new BlockGroup
        }
        _values(i) = pkg.values(obj, i)
      }
      _values(i).asInstanceOf[T]
    }
  }

  def getValueID(x: Table): Long = {
    _values.indexOf(x) + 1
  }

  /** Returns cached Expr or null if id is zero or negative. */
  def getExpr[T >: Null <: Table : ClassTag](id: Long): T = {
    if (id <= 0) {
      null
    } else {
      val i = id.toInt - 1
      if (exprs(i) == null) {
        val obj = pkg.exprsType(i) match {
          case ExpressionElem.Expression => new Expression
          case ExpressionElem.AllocateBase => new AllocateBase
          case ExpressionElem.ApplyBase => new ApplyBase
          case ExpressionElem.BinaryExpressionBase => new BinaryExpressionBase
          case ExpressionElem.Branch => new Branch
          case ExpressionElem.Debug => new Debug
          case ExpressionElem.Field => new Field
          case ExpressionElem.FieldByName => new FieldByName
          case ExpressionElem.GetElementByName => new GetElementByName
          case ExpressionElem.GetElementRef => new GetElementRef
          case ExpressionElem.GetInstantiateValue => new GetInstantiateValue
          case ExpressionElem.GetRTTIStatic => new GetRTTIStatic
          case ExpressionElem.InstanceOf => new InstanceOf
          case ExpressionElem.IntrinsicBase => new IntrinsicBase
          case ExpressionElem.InvokeBase => new InvokeBase
          case ExpressionElem.Lambda => new Lambda
          case ExpressionElem.MultiBranch => new MultiBranch
          case ExpressionElem.NumericCastBase => new NumericCastBase
          case ExpressionElem.RawArrayAllocateBase => new RawArrayAllocateBase
          case ExpressionElem.SpawnBase => new SpawnBase
          case ExpressionElem.StoreElementByName => new StoreElementByName
          case ExpressionElem.StoreElementRef => new StoreElementRef
          case ExpressionElem.UnaryExpressionBase => new UnaryExpressionBase
        }
        exprs(i) = pkg.exprs(obj, i)
      }
      exprs(i).asInstanceOf[T]
    }
  }

  override def typeDefs(): Seq[CHIR.CustomTypeDef] = _typeDefs

  override def name(): String = pkg.name()

  override def packageInitFunc(): CHIR.Func = ???

  override def packageInitLiteralFunc(): CHIR.Func = ???

  override def values(): Seq[CHIR.Value] = ???

  override def function(idx: Int): CHIR.Func = ???
}
