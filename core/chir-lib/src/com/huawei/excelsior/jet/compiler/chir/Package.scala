/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

import com.google.flatbuffers.IntVector
import com.huawei.excelsior.jet.compiler.chir.CHIR.GetException
import com.huawei.excelsior.jet.compiler.chir.CHIRUtils.*
import com.huawei.excelsior.jet.compiler.chir.PackageFormat.*

import java.nio.ByteBuffer
import scala.reflect.ClassTag

trait CHIRItemProvider {
  def getType[T >: Null <: CHIR.Type : ClassTag](id: Long): Option[T]
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
  private val _types = Array.fill[CHIR.Type](pkg.typesLength)(null)
  private val _values = Array.fill[CHIR.Value](pkg.valuesLength)(null)
  private val _exprs = Array.fill[CHIR.Expression](pkg.exprsLength)(null)
  private val _customDefs = Array.fill[CHIR.CustomTypeDef](pkg.defsLength)(null)

  /** Returns cached Type or null if id is zero or negative. */
  override def getType[T >: Null <: CHIR.Type : ClassTag](id: Long): Option[T] = {
    if (id <= 0) {
      None
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
        given provider: CHIRItemProvider = this
        _types(i) = pkg.types(obj, i) match {
          case t: GenericType => GenericTypeImpl(t)
          case t: FuncType => FuncTypeImpl(t)
          case t: RawArrayType => RawArrayTypeImpl(t)
          case t: VArrayType => VArrayTypeImpl(t)
          case t: CustomType => t.base.kind match {
            case CHIRTypeKind.CLASS => ClassTypeImpl(t)
            case CHIRTypeKind.STRUCT => StructTypeImpl(t)
            case CHIRTypeKind.ENUM => EnumTypeImpl(t)
          }
          case t: Type => t.kind match {
            case CHIRTypeKind.INT8 => CHIR.BuiltinType.Int8
            case CHIRTypeKind.INT16 => CHIR.BuiltinType.Int16
            case CHIRTypeKind.INT32 => CHIR.BuiltinType.Int32
            case CHIRTypeKind.INT64 => CHIR.BuiltinType.Int64
            case CHIRTypeKind.INT_NATIVE => CHIR.BuiltinType.IntNative
            case CHIRTypeKind.UINT8 => CHIR.BuiltinType.UInt8
            case CHIRTypeKind.UINT16 => CHIR.BuiltinType.UInt16
            case CHIRTypeKind.UINT32 => CHIR.BuiltinType.UInt32
            case CHIRTypeKind.UINT64 => CHIR.BuiltinType.UInt64
            case CHIRTypeKind.UINT_NATIVE => CHIR.BuiltinType.UIntNative
            case CHIRTypeKind.FLOAT16 => CHIR.BuiltinType.Float16
            case CHIRTypeKind.FLOAT32 => CHIR.BuiltinType.Float32
            case CHIRTypeKind.FLOAT64 => CHIR.BuiltinType.Float64
            case CHIRTypeKind.RUNE => CHIR.BuiltinType.Rune
            case CHIRTypeKind.BOOLEAN => CHIR.BuiltinType.Boolean
            case CHIRTypeKind.UNIT => CHIR.BuiltinType.Unit
            case CHIRTypeKind.NOTHING => CHIR.BuiltinType.Nothing
            case CHIRTypeKind.VOID => CHIR.BuiltinType.Void
            case CHIRTypeKind.C_POINTER => new CPointerTypeImpl(t)
            case CHIRTypeKind.C_STRING => CHIR.BuiltinType.CString
            case CHIRTypeKind.REFTYPE => new RefTypeImpl(t)
            case CHIRTypeKind.BOXTYPE => new BoxTypeImpl(t)
            case CHIRTypeKind.TUPLE => new TupleTypeImpl(t)
            case CHIRTypeKind.THIS => CHIR.BuiltinType.This
          }
        }
      }
      Some(_types(i)).collect {
        case t: T => t
      }
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

        def hasBlockOperand(operandsVector: IntVector): Boolean = {
          operandsVector.iterator.exists(getValue[CHIR.Block](_).nonEmpty)
        }

        given provider: CHIRItemProvider = this
        _exprs(i) = pkg.exprs(obj, i) match {
          case e: AllocateBase if hasBlockOperand(e.base.operandsVector) => new TryAllocateImpl(e)
          case e: AllocateBase => new AllocateImpl(e)
          case e: ApplyBase if hasBlockOperand(e.base.base.operandsVector) => new TryApplyImpl(e)
          case e: ApplyBase => new ApplyImpl(e)
          case e: BinaryExpressionBase if hasBlockOperand(e.base.operandsVector) => new TryBinaryImpl(e)
          case e: BinaryExpressionBase => new BinaryImpl(e)
          case e: Branch => new BranchImpl(e)
          case e: Debug => new DebugImpl(e)
          case e: Field => new FieldImpl(e)
          case e: GetElementRef => new GetElementRefImpl(e)
          case e: GetRTTIStatic => new GetRTTIStaticImpl(e)
          case e: InstanceOf => new InstanceOfImpl(e)
          case e: IntrinsicBase if hasBlockOperand(e.base.base.operandsVector) => new TryIntrinsicImpl(e)
          case e: IntrinsicBase => new IntrinsicImpl(e)
          case e: InvokeBase if hasBlockOperand(e.base.base.operandsVector) => new TryInvokeImpl(e)
          case e: InvokeBase => new InvokeImpl(e)
          case e: MultiBranch => new MultiBranchImpl(e)
          case e: NumericCastBase if hasBlockOperand(e.base.operandsVector) => new TryNumericCastImpl(e)
          case e: NumericCastBase => new NumericCastImpl(e)
          case e: RawArrayAllocateBase if hasBlockOperand(e.base.operandsVector) => new TryRawArrayAllocateImpl(e)
          case e: RawArrayAllocateBase => new RawArrayAllocateImpl(e)
          case e: SpawnBase if hasBlockOperand(e.base.operandsVector) => new TrySpawnImpl(e)
          case e: SpawnBase => new SpawnImpl(e)
          case e: StoreElementRef => new StoreElementRefImpl(e)
          case e: UnaryExpressionBase if hasBlockOperand(e.base.operandsVector) => new TryUnaryImpl(e)
          case e: UnaryExpressionBase => new UnaryImpl(e)
          case e: Expression => e.kind match {
            case CHIRExprKind.Goto => new GotoImpl(e)
            case CHIRExprKind.Exit => new ExitImpl(e)
            case CHIRExprKind.RaiseException => new RaiseExceptionImpl(e)
            case CHIRExprKind.StaticCast => new StaticCastImpl(e)
            case CHIRExprKind.Box => new BoxImpl(e)
            case CHIRExprKind.UnboxToValue => new UnboxToValueImpl(e)
            case CHIRExprKind.CastToConcrete => new CastToConcreteImpl(e)
            case CHIRExprKind.CastToGeneric => new CastToGenericImpl(e)
            case CHIRExprKind.Load => new LoadImpl(e)
            case CHIRExprKind.Store => new StoreImpl(e)
            case CHIRExprKind.RawArrayLiteralInit => new RawArrayLiteralInitImpl(e)
            case CHIRExprKind.RawArrayInitByValue => new RawArrayInitByValueImpl(e)
            case CHIRExprKind.Constant => new ConstantImpl(e)
            case CHIRExprKind.Tuple => new TupleImpl(e)
            case CHIRExprKind.GetException => GetException
            case CHIRExprKind.GetRtti => new GetRTTIImpl(e)
          }
        }
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

  override def packageInitFunc: CHIR.Func = getValue[CHIR.Func](pkg.packageInitFunc).get

  override def packageInitLiteralFunc: CHIR.Func = getValue[CHIR.Func](pkg.packageLiteralInitFunc).get

  override def values: Iterator[CHIR.Value] = {
    (1 to pkg.valuesLength()).iterator.map { id =>
      getValue[CHIR.Value](id).get
    }
  }

  override def function(idx: Int): CHIR.Func = {
    getValue[CHIR.Func](idx).get
  }
}

