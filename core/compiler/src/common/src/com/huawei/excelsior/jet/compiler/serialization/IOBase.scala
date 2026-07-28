/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.serialization

import com.huawei.excelsior.jet.assembler.{AsmType, Width}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.common.XString.xstr
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.{SpecialParamSet, SpecialParameter}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{ArraySlice, Box, BString, CPointer, CangjieArray, CangjieEnumWrapper, ClassTypeVariable, InstantiatedRecord, InstantiatedReference, JavaArray, LocalTypeVariable, NameBased, NamedRecord, NonNullableWrapper, NullableWrapper, Primitive, Record, Reference, SymRecord, SymTypeBased, ThisTypeInfo, Tuple, VArray}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.{BitcodeFieldReference, BitcodeMethodReference, BytecodeMethodReference, CallConv, CallKind, ConstraintCallMethodReference, Field, FrameDescSymbol, InstantiatedMethodReference, Method, MethodReference, MethodReferenceAccessKind, MethodSignature, MethodType, Signature, SignatureType, SymlevelReader, SymlevelWriter, TypeKind, Type as SymType}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ClassType, ReferenceType}
import com.huawei.excelsior.jet.compiler.types.References.*

import scala.Function.tupled
import scala.annotation.nowarn
import scala.collection.mutable
import com.huawei.excelsior.jet.compiler.symlevel.CangjieFieldReference
import com.huawei.excelsior.jet.compiler.types.CompiledType

trait IOBase {

  abstract class Writer(env: Environment) extends SymlevelWriter.StreamWriter {

    protected implicit def typeProvider: TypeProvider = env.getTypeProvider

    type Buffer

    def withBuffering(action: => Unit): Buffer

    def bufferPosition: Int

    def writeHeader(): Unit

    def writeBuffer(buffer: Buffer): Unit

    def enumeration(x: scala.reflect.Enum): Unit

    def arithOp(op: ArithOp): Unit

    def width(width: Width): Unit

    def asmType(asmType: AsmType): Unit

    def methodRefAccessKind(kind: MethodReferenceAccessKind): Unit

    def preparationKind(kind: PreparationKind): Unit

    def callConv(cc: CallConv): Unit

    def callKind(ck: CallKind): Unit

    def xstring(str: XString): Unit

    def bool(value: Boolean): Unit

    def number(num: Int): Unit

    def unsignedNumber(num: Int): Unit

    def longNumber(num: Long): Unit

    def floatNumber(num: Float): Unit

    def doubleNumber(num: Double): Unit

    def domain(domain: Domain): Unit

    def delimiter(): Unit

    protected def symlevelWriter: SymlevelWriter

    def tkind(tkind: TypeKind): Unit = symlevelWriter.tkind(tkind)
    def symType(symType: SymType): Unit = symlevelWriter.tpe(symType)
    def field(field: Field): Unit = symlevelWriter.field(field)
    def method(method: Method): Unit = symlevelWriter.method(method)
    def constString(constString: symlevel.ConstString): Unit = symlevelWriter.constString(constString)
    def frameDesc(fd: FrameDescSymbol): Unit = symlevelWriter.frameDesc(fd)

    def compiledType(t: CompiledType): Unit = {
      if (t.symType.isCangjieType) {
        number(0)
        sigType(t.sigType)
      } else {
        number(1)
        symType(t.symType)
      }
    }

    def cangjieFieldReference(fieldRef: CangjieFieldReference): Unit = {
      longNumber(fieldRef.idx)
      option(fieldRef.field)(field)
      sigType(fieldRef.refType)
      sigType(fieldRef.fieldType)
    }

    def option[T](option: Option[T])(f: T => Unit): Unit = {
      option match {
        case Some(value) =>
          bool(true)
          f(value)
        case None =>
          bool(false)
      }
    }

    def iterable[T](xs: Iterable[T], f: T => Unit): Unit = {
      unsignedNumber(xs.size)
      xs foreach f
    }

    def seq[T](xs: Seq[T])(f: T => Unit): Unit = iterable(xs, f)
    def set[T](xs: Set[T])(f: T => Unit): Unit = iterable(xs, f)
    def map[K, V](map: collection.Map[K, V])(keyValue: (K, V) => Unit): Unit = iterable(map, tupled(keyValue))

    @nowarn("msg=match may not be exhaustive")
    def anyNumber(kind: TypeKind, value: Number): Unit = (kind, value) match {
      case (TypeKind.INT,    value: java.lang.Integer) => number(value.intValue)
      case (TypeKind.LONG,   value: java.lang.Long   ) => longNumber(value.longValue)
      case (TypeKind.FLOAT,  value: java.lang.Float  ) => floatNumber(value.floatValue)
      case (TypeKind.DOUBLE, value: java.lang.Double ) => doubleNumber(value.doubleValue)
    }

    def signature(sig: Signature): Unit = sig match {
      case sig: SignatureType =>
        number(0)
        sigType(sig)
      case sig: MethodSignature =>
        number(1)
        methodSignature(sig)
    }

    def sigType(t: SignatureType): Unit = t match {
      case t: Primitive =>
        number(0)
        number(t.id)
      case JavaArray(baseType, dimNum) =>
        number(1)
        sigType(baseType)
        number(dimNum)
      case t: Reference with NameBased =>
        number(2)
        xstring(XString(t.name))
        bool(t.jbc)
      case t: NamedRecord =>
        number(3)
        xstring(XString(t.name))
      case t: Reference with SymTypeBased =>
        number(4)
        symType(t.symType)
      case t: SymRecord =>
        number(5)
        symType(t.symType)
      case BString =>
        number(6)
      case CPointer(pointee) =>
        number(7)
        signature(pointee)
      case ArraySlice(elemType) =>
        number(8)
        sigType(elemType)
      case CangjieEnumWrapper(baseType, name) =>
        number(9)
        sigType(baseType)
        xstring(xstr(name))
      case VArray(elemType, length) =>
        number(10)
        sigType(elemType)
        longNumber(length)
      case t: InstantiatedReference =>
        number(11)
        xstring(XString(t.name))
        seq(t.instantiatedTypeParameters)(sigType)
      case t: InstantiatedRecord =>
        number(12)
        xstring(XString(t.name))
        seq(t.instantiatedTypeParameters)(sigType)
      case t: LocalTypeVariable =>
        number(13)
        number(t.idx)
      case CangjieArray(elemType) =>
        number(14)
        sigType(elemType)
      case ThisTypeInfo =>
        number(15)
      case NullableWrapper(baseType) =>
        number(16)
        sigType(baseType)
      case NonNullableWrapper(baseType) =>
        number(17)
        sigType(baseType)
      case t: ClassTypeVariable =>
        number(18)
        number(t.idx)
      case t: Tuple =>
        number(19)
        seq(t.params)(sigType)
      case t: Box =>
        number(20)
        sigType(t.base)
    }

    def methodSignature(sig: MethodSignature): Unit = {
      sigType(sig.returnType)
      seq(sig.parameterTypes.toSeq)(sigType)
    }

    def specialParameters(specialParams: SpecialParamSet): Unit = {
      seq(specialParams.elements.toSeq)(enumeration(_))
    }

    def referenceType(tpe: ReferenceType): Unit = {
      symType(tpe.symType)
    }

    def typeApproximation(t: ReferenceApprox): Unit = {
      def typeUpperBounded(t: UpperBounded): Unit = {
        referenceType(t.root)
        bool(t.mayBeNull)
      }

      t match {
        case t: Point => number(0); typeUpperBounded(t)
        case t: OpenCone => number(1); typeUpperBounded(t)
        case t: ClosedCone => number(2); typeUpperBounded(t); unsignedNumber(t.height)
        case RefNull => number(3)
        case RefEmpty => number(4)
      }
      val pt = if (t.hasRefinedProbableType) Some(t.probableType) else None
      option(pt)(typeApproximation)
    }

    def methodType(mt: MethodType): Unit = {
      methodSignature(mt.signature)
      callConv(mt.callConv)
      callKind(mt.callKind)
      specialParameters(mt.specialParameters)
      bool(mt.isVarArgs)
      assert(!mt.isVarArgs || mt.areVarArgsInitialized)
      number(mt.firstVarArg)
      number(mt.headInLimit)
      number(mt.headOutLimit)
      number(mt.preservedParameterMask)
      bool(mt.altLocationInfo.methodHasAltLocationResult)
      number(mt.altLocationInfo.altLocationParameterMask)
    }

    def methodRef(methodRef: MethodReference): Unit = {
      if (methodRef.hasRefClass) {
        if (methodRef.hasMethod) {
          val m = methodRef.method
          if (methodRef.methodType == m.getMethodType) {
            number(0)
            method(m)
          } else {
            // methodType was patched
            number(1)
            method(m)
            methodType(methodRef.methodType)
          }
        } else {
          // deferred method reference
          number(2)
          methodType(methodRef.methodType)
        }
        compiledType(methodRef.refType)

      } else {
        // raw method reference for indirect call
        number(3)
        methodType(methodRef.methodType)
        assert(!methodRef.hasMethod)
      }

      assert(!methodRef.hasPermanentMethod)
      methodRefAccessKind(methodRef.accessKind)
      option(methodRef.explicitVNum)(number)

      methodRef match {
        case x: InstantiatedMethodReference =>
          number(4)
          seq(x.instantiatedTypeParameters)(sigType)
        case x: ConstraintCallMethodReference =>
          number(3)
          methodSignature(x.sourceSig)
          xstring(x.methodName)
        case x: BitcodeMethodReference =>
          number(2)
          methodType(x.sourceMethodType)
          xstring(x.methodName)
          option(x.linkageName)(xstring)
        case x: BytecodeMethodReference =>
          number(1)
          bool(x.isMemberNameInvoke)
          number(x.cpIndex)
        case _ =>
          number(0)
      }
    }

    def fieldRef(fieldRef: BitcodeFieldReference): Unit = {
      sigType(fieldRef.refType)
      sigType(fieldRef.fieldType)
      xstring(fieldRef.fieldName)
      bool(fieldRef.isWrite)
      bool(fieldRef.isStatic)
    }
  }

  abstract class Reader(env: Environment) extends SymlevelReader.StreamReader {

    protected implicit def typeProvider: TypeProvider = env.getTypeProvider

    def readHeader(): Unit

    def enumeration[T <: scala.reflect.Enum](fromOrdinal: Int => T): T

    def arithOp(): ArithOp

    def width(): Width
    def asmType(): AsmType

    def methodRefAccessKind(): MethodReferenceAccessKind

    def preparationKind(): PreparationKind

    def callConv(): CallConv

    def callKind(): CallKind

    def xstring(): XString

    def bool(): Boolean

    def number(): Int

    def unsignedNumber(): Int

    def longNumber(): Long

    def floatNumber(): Float

    def doubleNumber(): Double

    def domain(): Domain

    def delimiter(): Unit

    def isEOF: Boolean

    def skip(n: Int): Unit

    protected def symlevelReader: SymlevelReader

    def tkind(): TypeKind = symlevelReader.tkind()
    def symType(): SymType = symlevelReader.tpe()
    def field(): Field = symlevelReader.field()
    def method(): Method = symlevelReader.method()
    def constString(): symlevel.ConstString = symlevelReader.constString()
    def frameDesc(): FrameDescSymbol = symlevelReader.frameDesc()

    def compiledType(): CompiledType = number() match {
      case 0 => CompiledType(sigType())
      case 1 => CompiledType(symType())
    }

    def cangjieFieldReference(): CangjieFieldReference = {
      CangjieFieldReference(longNumber(), option(field), sigType(), sigType())
    }

    def option[T](f: () => T): Option[T] = {
      if (bool()) {
        Some(f())
      } else {
        None
      }
    }

    private def iterable[T, CC](f: () => T, builder: mutable.Builder[T, CC]): CC = {
      val size = unsignedNumber()
      builder.sizeHint(size)
      for (_ <- 0 until size) {
        builder += f()
      }
      builder.result()
    }

    def seq[T](f: () => T) = iterable(f, Seq.newBuilder[T])
    def set[T](f: () => T) = iterable(f, Set.newBuilder[T])
    def map[K, V](keyValue: () => (K, V)) = iterable(keyValue, Map.newBuilder[K, V])

    @nowarn("msg=match may not be exhaustive")
    def anyNumber(kind: TypeKind): Number = kind match {
      case TypeKind.INT    => number()
      case TypeKind.LONG   => longNumber()
      case TypeKind.FLOAT  => floatNumber()
      case TypeKind.DOUBLE => doubleNumber()
    }

    def signature(): Signature = number() match {
      case 0 => sigType()
      case 1 => methodSignature()
    }

    def sigType(): SignatureType = number() match {
      case 0 => Primitive.byID(number())
      case 1 => JavaArray(sigType(), number())
      case 2 => Reference(xstring().toString, bool())
      case 3 => Record(xstring().toString)
      case 4 => Reference(asClassType(symType()))
      case 5 => Record(asClassType(symType()))
      case 6 => BString
      case 7 => CPointer(signature())
      case 8 => ArraySlice(sigType())
      case 9 => CangjieEnumWrapper(sigType().asInstanceOf[CangjieEnumWrapper.Base], xstring().toString)
      case 10 => VArray(sigType(), longNumber())
      case 11 => InstantiatedReference(xstring().toString, seq(sigType))
      case 12 => InstantiatedRecord(xstring().toString, seq(sigType))
      case 13 => LocalTypeVariable(number())
      case 14 => CangjieArray(sigType())
      case 15 => ThisTypeInfo
      case 16 => NullableWrapper(sigType().asInstanceOf[NullableWrapper.Base])
      case 17 => NonNullableWrapper(sigType().asInstanceOf[NonNullableWrapper.Base])
      case 18 => ClassTypeVariable(number())
      case 19 => Tuple(seq(sigType))
      case 20 => Box(sigType())
    }

    def methodSignature(): MethodSignature = {
      MethodSignature(sigType(), seq(sigType))
    }

    def specialParameters(): SpecialParamSet = {
      SpecialParamSet(seq(() => enumeration[SpecialParameter](SpecialParameter.fromOrdinal)))
    }

    def referenceType(): ReferenceType = {
      ReferenceType(asClassType(symType()))
    }

    def typeApproximation(): ReferenceApprox = {
      val t = number() match {
        case 0 => Point(referenceType(), bool())
        case 1 => OpenCone(referenceType(), bool())
        case 2 => ClosedCone.withHeight(referenceType().asInstanceOf[ClassType], bool(), unsignedNumber())
        case 3 => RefNull
        case 4 => RefEmpty
      }
      val pt = option { () => typeApproximation() }
      t.withProbableType(pt.orNull)
    }

    def methodType(): MethodType = {
      val sig = methodSignature()
      val cc = callConv()
      val ck = callKind()
      val specialParams = specialParameters()
      val isVarArgs = bool()
      val firstVarArg = number()
      val inLimit = number()
      val outLimit = number()
      val preservedParameterMask = number()
      val altLocationInfo = {
        val methodIsAltLocation = bool()
        val altLocationParameterMask = number()
        MethodType.AltLocationInfo(methodIsAltLocation, altLocationParameterMask)
      }

      MethodType(sig, cc, ck, specialParams, isVarArgs, firstVarArg, inLimit, outLimit, preservedParameterMask, altLocationInfo)
    }

    def methodRef(): MethodReference = {
      val (m, mt, refClass) = number() match {
        case 0 => val m = method(); (m, m.getMethodType, compiledType())
        case 1 => (method(), methodType(), compiledType())
        case 2 => (null, methodType(), compiledType())
        case 3 => (null, methodType(), null)
      }

      val methodRef = new MethodReference(mt, methodRefAccessKind(), m, null, refClass, option(number))

      number() match {
        case 0 => methodRef
        case 1 => methodRef.toBytecodeMethodReference(isMemberNameInvoke = bool(), number())
        case 2 => methodRef.toBitcodeMethodReference(methodType(), xstring(), option(xstring))
        case 3 => methodRef.toConstraintCallMethodReference(methodSignature(), xstring())
        case 4 => methodRef.toInstantiatedMethodReference(seq(sigType), refClass.sigType)
      }
    }

    def fieldRef(): BitcodeFieldReference = BitcodeFieldReference(sigType(), sigType(), xstring(), bool(), bool())
  }
}
