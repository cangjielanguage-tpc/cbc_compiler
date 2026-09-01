/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.v1_0.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v1_0.CHIRUtils.*

import java.nio.ByteBuffer
import scala.reflect.ClassTag

trait CHIRItemProvider {
  def getType[T >: Null <: CHIR.Type : ClassTag](id: Long): T
  def getValue[T >: Null <: CHIR.Value : ClassTag](id: Long): Option[T]
  def getExpr[T >: Null <: CHIR.Expression : ClassTag](id: Long): T
  def getDef[T >: Null <: CHIR.CustomTypeDef : ClassTag](id: Long): Option[T]
}

/** [[CHIRPackage]] with caching of core indexed entities:
 *  - Types
 *  - Values
 *  - Exprs
 *  - Defs
 */
final class PackageImpl(source: String) extends CHIR.Package with CHIRItemProvider {

  private lazy val pkg: CHIRPackage = {
    // TODO: fix performance regression of XScala IO compared to JDK IO
    val bytes = java.nio.file.Files.readAllBytes(java.io.File(source).toPath)
    val buf = ByteBuffer.wrap(bytes)
    CHIRPackage.getRootAsCHIRPackage(buf)
  }
  private lazy val _types = Array.fill[CHIR.Type](pkg.typesLength)(null)
  private lazy val _values = Array.fill[CHIR.Value](pkg.valuesLength)(null)
  private lazy val _exprs = Array.fill[CHIR.Expression](pkg.exprsLength)(null)
  private lazy val _customDefs = Array.fill[CHIR.CustomTypeDef](pkg.defsLength)(null)
  private lazy val _pkgInit = getValue[CHIR.Func](pkg.packageInitFunc()).get
  private lazy val _pkgLiteralInit = getValue[CHIR.Func](pkg.packageLiteralInitFunc()).get

  /** Returns cached Type or null if id is zero or negative. */
  override def getType[T >: Null <: CHIR.Type : ClassTag](id: Long): T = {
    if (id <= 0) {
      null
    } else {
      val i = id.toInt - 1
      if (_types(i) == null) {
        val obj = pkg.typesType(i) match {
          case TypeElem.Type => new Type
          case TypeElem.RawArrayType => new RawArrayType
          case TypeElem.VArrayType => new VArrayType
          case TypeElem.FuncType => new FuncType
          case TypeElem.CustomType => new CustomType
          case TypeElem.GenericType => new GenericType
        }
        val t = pkg.types(obj, i)
        t match {
          case t: Type =>


        }

//        _types(i) =
      }
      _types(i).asInstanceOf[T]
    }
  }

  /** Returns cached Value or null if id is zero or negative. */
  override def getValue[T >: Null <: CHIR.Value : ClassTag](id: Long): Option[T] = {
    if (id <= 0) {
      None
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
          case ValueElem.Function => new Function
          case ValueElem.Block => new Block
          case ValueElem.BlockGroup => new BlockGroup
        }
        given provider: CHIRItemProvider = this
        _values(i) = pkg.values(obj, i) match {
          case v: BoolLiteral => BoolLiteralImpl(v)
          case v: RuneLiteral => RuneLiteralImpl(v)
          case v: StringLiteral => StringLiteralImpl(v)
          case v: IntLiteral => IntLiteralImpl(v)
          case v: FloatLiteral => FloatLiteralImpl(v)
          case _: UnitLiteral => CHIR.UnitLiteral
          case v: NullLiteral => NullLiteralImpl(v)
          case v: Parameter => ParameterImpl(v)
          case v: LocalVar => LocalVarImpl(v)
          case v: GlobalVar => GlobalVarImpl(v, id)
          case v: Function => FuncImpl(v, id)
          case v: Block => BlockImpl(v)
          case v: BlockGroup => BlockGroupImpl(v)
        }
      }
      Some(_values(i)).collect {
        case t: T => t
      }
    }
  }

  /** Returns cached Expr or null if id is zero or negative. */
  override def getExpr[T >: Null <: CHIR.Expression : ClassTag](id: Long): T = {
    if (id <= 0) {
      null
    } else {
      val i = id.toInt - 1
      if (_exprs(i) == null) {
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
//        _exprs(i) = pkg.exprs(obj, i)
      }
      _exprs(i).asInstanceOf[T]
    }
  }

  /** Returns cached Def or null if id is zero or negative. */
  override def getDef[T >: Null <: CHIR.CustomTypeDef : ClassTag](id: Long): Option[T] = {
    if (id <= 0) {
      None
    } else {
      val i = id.toInt - 1
      if (_customDefs(i) == null) {
        val obj = pkg.defsType(i) match {
          case CustomTypeDefElem.EnumDef => new EnumDef
          case CustomTypeDefElem.StructDef => new StructDef
          case CustomTypeDefElem.ClassDef => new ClassDef
          case CustomTypeDefElem.ExtendDef => new ExtendDef
        }
        given provider: CHIRItemProvider = this
        _customDefs(i) = pkg.defs(obj, i) match {
          case t: EnumDef => EnumDefImpl(t)
          case t: ClassDef => ClassDefImpl(t)
          case t: StructDef => StructDefImpl(t)
          case t: ExtendDef => ExtendDefImpl(t)
        }
      }
      Some(_customDefs(i)).collect {
        case t: T => t
      }
    }
  }

  override def typeDefs: Iterator[CHIR.CustomTypeDef] = {
    (1 to pkg.defsLength()).iterator.map { id =>
      getDef[CHIR.CustomTypeDef](id).get
    }
  }

  override def name: String = pkg.name

  override def packageInitFunc: CHIR.Func = _pkgInit

  override def packageInitLiteralFunc: CHIR.Func = _pkgLiteralInit

  override def values: Iterator[CHIR.Value] = {
    (1 to pkg.valuesLength()).iterator.map { id =>
      getValue[CHIR.Value](id).get
    }
  }

  override def function(idx: Int): CHIR.Func = {
    getValue[CHIR.Func](idx).get
  }
}

