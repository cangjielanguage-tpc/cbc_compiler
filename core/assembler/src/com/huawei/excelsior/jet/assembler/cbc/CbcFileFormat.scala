/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.*
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.cbc.CbcFileEncoder.Index
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.TypeEnumKind.NotEnum
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector.LiveState
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.FlowAnalyzer

import scala.annotation.targetName
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CbcFileFormat {

  def newBuilder(): Builder = new CbcFileFormatBuilder()

  case class BytecodeReferenceSymbol(ref: BytecodeReference) extends Symbol
  sealed trait BytecodeReference
  case class StringLiteral(s: String) extends BytecodeReference
  case class RawData(data: ArraySeq[Byte]) extends BytecodeReference

  sealed trait Signature extends BytecodeReference {
    def isReference: Boolean
  }

  enum BuiltinSignature(val id: Int) extends Signature {
    case Nil extends BuiltinSignature(0x00)
    case Void extends BuiltinSignature(0x01)
    case Unit extends BuiltinSignature(0x02)
    case Nothing extends BuiltinSignature(0x03)
    case Boolean extends BuiltinSignature(0x04)
    case I8 extends BuiltinSignature(0x5)
    case U8 extends BuiltinSignature(0x6)
    case I16 extends BuiltinSignature(0x07)
    case U16 extends BuiltinSignature(0x08)
    case I32 extends BuiltinSignature(0x09)
    case U32 extends BuiltinSignature(0x0a)
    case UChar32 extends BuiltinSignature(0x0b)
    case I64 extends BuiltinSignature(0x0c)
    case U64 extends BuiltinSignature(0x0d)
    case IAddr extends BuiltinSignature(0x0e)
    case UAddr extends BuiltinSignature(0x0f)
    case BString extends BuiltinSignature(0x10)
    case F16 extends BuiltinSignature(0x11)
    case F32 extends BuiltinSignature(0x12)
    case F64 extends BuiltinSignature(0x13)

    override def isReference = false
  }

  case class TypeSignature(name: String, args: Seq[Signature], isReference: Boolean) extends Signature
  case class AotTypeSignature(name: String, args: Seq[Signature], isReference: Boolean) extends Signature
  case class OptionSignature(name: String, args: Seq[Signature], isReference: Boolean) extends Signature
  case class PrimitiveEnum(name: String, args: Seq[Signature]) extends Signature {
    override def isReference = false
  }
  case class UnionEnum(name: String, args: Seq[Signature]) extends Signature {
    override def isReference = false
  }
  case class CangjieArray(tpe: Signature) extends Signature {
    override def isReference = true
  }
  case class Tuple(args: Seq[Signature]) extends Signature {
    override def isReference = false
  }
  case class Functional(args: Seq[Signature], result: Signature) extends Signature {
    override def isReference = true
  }
  case class Nullable(sig: Signature) extends Signature { // FIXME: remove
    override def isReference = sig.isReference
  }
  case class NonNullable(sig: Signature) extends Signature {
    override def isReference = sig.isReference
  }
  case class VArray(sig: Signature, length: Long) extends Signature {
    override def isReference = false
  }
  case class CPointer(sig: Signature) extends Signature {
    override def isReference = false
  }
  case class Box(sig: Signature) extends Signature {
    override def isReference = true
  }
  case class Fst(sig: Signature) extends Signature {
    override def isReference = false
  }

  sealed trait TypeVariable extends Signature {
    override def isReference = false
  }
  case class FuncTypeVariable(id: Int) extends TypeVariable
  case class ClassTypeVariable(id: Int) extends TypeVariable

  object BuiltinSignature {
    def count: Int = BuiltinSignature.F64.id + 1
    def unapply(sig: BuiltinSignature): Option[Int] = Some(sig.id)
  }

  object TypeSignature {
    def ref(name: String) = TypeSignature(name, Seq.empty, isReference = true)
    def rec(name: String) = TypeSignature(name, Seq.empty, isReference = false)
  }

  object AotTypeSignature {
    def ref(name: String) = AotTypeSignature(name, Seq.empty, isReference = true)
    def rec(name: String) = AotTypeSignature(name, Seq.empty, isReference = false)
  }

  sealed trait Flag {
    def mask: Int
  }

  sealed trait Flags[F <: Flag] {
    def mask: Int
    def contains(f: F) = (f.mask & this.mask) != 0
  }

  object Flags {
    sealed trait Companion[F <: Flag, FS <: Flags[F]] {
      def apply(mask: Int): FS
      def apply(flags: IterableOnce[F]): FS = apply(flags.iterator.map(_.mask).fold(0)(_ | _))
      def empty: FS = apply(Seq.empty)
    }
  }

  object TypeFlags extends Flags.Companion[TypeFlag, TypeFlags]
  case class TypeFlags(mask: Int) extends Flags[TypeFlag] {
    override def toString: String =
      TypeFlag.values.filter(contains).mkString("[", ", ", "]")
  }

  object MethodFlags extends Flags.Companion[MethodFlag, MethodFlags]
  case class MethodFlags(mask: Int) extends Flags[MethodFlag] {
    override def toString: String =
      MethodFlag.values.filter(contains).mkString("[", ", ", "]")
  }

  object MethodRefFlags extends Flags.Companion[MethodRefFlag, MethodRefFlags]
  case class MethodRefFlags(mask: Int) extends Flags[MethodRefFlag] {
    override def toString: String =
      MethodRefFlag.values.filter(contains).mkString("[", ", ", "]")
  }

  object FieldFlags extends Flags.Companion[FieldFlag, FieldFlags]
  case class FieldFlags(mask: Int) extends Flags[FieldFlag] {
    override def toString: String =
      FieldFlag.values.filter(contains).mkString("[", ", ", "]")
  }

  // TODO move access kinds out of flags
  enum TypeFlag(val mask: Int) extends Flag {
    // TODO: Currently, uses masks from original format that tries to share the masks between types, fields and methods.
    //       That is unreasonable, since we can always reorder bits in runtime.
    //       The flag bits should be more compactly allocated.
    case PUBLIC    extends TypeFlag(0x0001)
    case FINAL     extends TypeFlag(0x0002)
    case ABSTRACT  extends TypeFlag(0x0004)
    case SEALED    extends TypeFlag(0x0008)
    case INTERFACE extends TypeFlag(0x0010)
    case LAMBDA    extends TypeFlag(0x0020)
    case RECORD    extends TypeFlag(0x0040)
    case AOT       extends TypeFlag(0x0080)
    case PATCH     extends TypeFlag(0x0100)
    case ENUM      extends TypeFlag(0x0200)
  }

  // TODO move access kinds out of flags
  enum MethodFlag(val mask: Int) extends Flag {
    case PUBLIC    extends MethodFlag(0x0001)
    case PRIVATE   extends MethodFlag(0x0002)
    case PROTECTED extends MethodFlag(0x0003)

    case STATIC   extends MethodFlag(0x0004)
    case FINAL    extends MethodFlag(0x0008)
    case FOREIGN  extends MethodFlag(0x0010)
    case ABSTRACT extends MethodFlag(0x0020)
    case MUT      extends MethodFlag(0x0040) // has MUT <=> has extra parameter (not just source-level indicator)
    case VIRTUAL  extends MethodFlag(0x0080)
    case AOT      extends MethodFlag(0x0100)
    case PKG_INIT extends MethodFlag(0x0200)
    case LIT_INIT extends MethodFlag(0x0400)

    case SRET         extends MethodFlag(0x0800)
    case HAS_THIS_TI  extends MethodFlag(0x1000)
    case HAS_OUTER_TI extends MethodFlag(0x2000)
    case REC_RECEIVER extends MethodFlag(0x4000)
    case REF_RECEIVER extends MethodFlag(0x8000)
  }

  enum MethodRefFlag(val mask: Int) extends Flag {
    case SRET         extends MethodRefFlag(0x01)
    case HAS_THIS_TI  extends MethodRefFlag(0x02)
    case HAS_OUTER_TI extends MethodRefFlag(0x04)
    case MUT          extends MethodRefFlag(0x08)
    case HAS_FTVARS   extends MethodRefFlag(0x10)
    case AOT          extends MethodRefFlag(0x20)
    case REC_RECEIVER extends MethodRefFlag(0x40)
    case REF_RECEIVER extends MethodRefFlag(0x80)
  }

  enum FieldFlag(val mask: Int) extends Flag {
    case PUBLIC    extends FieldFlag(0x0001)
    case PRIVATE   extends FieldFlag(0x0002)
    case PROTECTED extends FieldFlag(0x0003)

    case STATIC   extends FieldFlag(0x0004)
    case FINAL    extends FieldFlag(0x0008)
    case VOLATILE extends FieldFlag(0x0010)
    case AOT      extends FieldFlag(0x0020)
  }

  case class CbcFile(bytecodeVersion: Int,
                     mainTypeName: Option[String],
                     cbcDeps: Option[String],
                     aotDeps: Option[String],
                     foreignLibs: Option[String],
                     types: Seq[Type])

  sealed trait Named {
    def name: String
  }

  enum TypeEnumKind {
    case NotEnum
    case Union
    case Option0 // enum { Some(T); None }
    case Option1 // enum { None; Some(T) }
    case Primitive
  }

  case class Type(name: String,
                  superOrEnumType: Option[Signature],
                  flags: TypeFlags,
                  methods: Seq[Method],
                  fields: Seq[Field],
                  interfaces: Seq[Signature],
                  genericConstraints: Seq[Signature],
                  enumKind: TypeEnumKind,
                  unionFields: Seq[Signature] = Seq.empty) extends Named

  case class Method(name: String,
                    typeName: String,
                    signature: Signature,
                    code: Option[MethodCode],
                    flags: MethodFlags,
                    sourceFullName: Option[String],
                    sourceFile: Option[String],
                    linkageName: Option[String] = None,
                    genericParameters: Int = 0) extends Named

  case class Field(name: String,
                   fieldType: Signature,
                   flags: FieldFlags,
                   constValue: Option[(FieldTag, Long)]) extends Named

  case class MethodCode(segment: Segment,
                        exTable: ExceptionTable,
                        liveness: LivenessInfoCollector.AllStates,
                        untypedStackSlotsCount: Int,
                        usedNonVolIRegsMask: Int,
                        usedNonVolFRegsMask: Int,
                        maxCalleeStackArgsCount: Int,
                        mayHaveNativeCalls: Boolean,
                        stackAllocatedTypeSigs: Seq[Signature],
                        variableSizeTypes: Seq[Signature])

  case class MethodReference(name: String,
                             refType: Signature,
                             signature: Signature,
                             flags: MethodRefFlags,
                             aotData: Option[AotData] = None,
                             typeVars: Seq[Signature] = Seq.empty) extends BytecodeReference

  // TODO: References to fields should be encoded without specifying `refType` part.
  //       Memory location can be specified by triple `(base, offset, type)`, where
  //       `base` can be either:
  //         - typed stack slot;
  //         - untyped stack slot;
  //         - register;
  //         - static/global field;
  //       `offset` designates a position relative to `base`, which is encoded as field/index sequence.
  //       `type` designates a type of memory location.
  //       Note that for some cases `base` is already typed (typed stack slot, static field),
  //       so following `FieldReference` would duplicate a type that is already known.
  //       Moreover, for cases where the type of `base` is unknown - we must add type
  //       specifiers to `index` encodings:
  //       ```
  //       mem.head.typed ts { // <-- type of element can be deduced from the `ts`
  //         const.index 10, TypeOfElement
  //         load reg }
  //       ```
  //       Such inconsistency causes either excessive complexities in decoding side
  //       or inefficiencies in instruction encodings.
  //       To avoid it, it is better to:
  //         - always provide a type of a `base`;
  //         - remove type specifiers from field references and index operations;
  //       It will be sufficient to compute the final `type` of memory location,
  //       just by sequentially applying operations.
  sealed trait FieldReference extends BytecodeReference
  sealed trait FieldReferenceWithType extends FieldReference {
    def refType: Signature
    def fieldType: Signature
  }
  case class SingleFieldReference(refType: Signature, name: String, fieldType: Signature,
                                  aotData: Option[AotData] = None) extends FieldReferenceWithType
  case class ConstIndexFieldReference(refType: Signature, idx: Int, fieldType: Signature) extends FieldReferenceWithType
  case class MultiFieldReference(subRefs: Seq[FieldReferenceWithType]) extends FieldReferenceWithType {
    def refType = subRefs.head.refType
    def fieldType = subRefs.last.fieldType
  }
  case class NoneFieldReference(sig: Signature) extends FieldReference // TODO specify more

  sealed trait AotData
  case class DirectCallAotData(linkageName: String) extends AotData
  case class VirtualCallAotData(vnum: Int, extDefNum: Int) extends AotData
  case class InterfaceCallAotData(inum: Int) extends AotData
  case class StaticFieldAotData(linkageName: String) extends AotData
  case class InstanceFieldAotData(ordinal: Int) extends AotData

  case class IndexedAotData(index: Index, aotData: AotData)

  trait Builder {
    def newTypeBuilder(): Type.Builder
    def setBytecodeVersion(version: Int): Unit
    def setMainTypeName(name: String): Unit
    def setCbcDeps(libs: String): Unit
    def setAotDeps(libs: String): Unit
    def setForeignLibs(libs: String): Unit
    def build(): CbcFile  }

  object Type {
    trait Builder {
      def setName(name: String): Unit
      def getName: Option[String]
      def setSuperOrEnumType(signature: Signature): Unit
      def setInterfaces(interfaces: Seq[Signature]): Unit
      def setUnionFields(fields: Seq[Signature]): Unit
      def setEnumKind(enumKind: TypeEnumKind): Unit
      def setGenericConstraints(genericConstraints: Seq[Signature]): Unit
      def addFlag(flag: TypeFlag): Unit

      // The order of fields and methods are defined by the order,
      // in which corresponding builders were created.
      def newMethodBuilder(): Method.Builder
      def newFieldBuilder(): Field.Builder
    }
  }

  object Method {
    trait Builder {
      def setName(name: String): Unit
      def setTypeName(typeName: String): Unit
      def setSignature(signature: Signature): Unit
      def getCodeBuilder(): MethodCode.Builder
      def addFlag(flag: MethodFlag): Unit
      def setSourceFullName(linkageName: String): Unit
      def setLinkageName(fullName: String): Unit
      def setSourceFile(fileName: String): Unit
    }
  }

  object MethodCode {
    trait Builder {
      def setSegment(segment: Segment): Unit
      def setExceptionTable(exTable: ExceptionTable): Unit
      def setLiveness(liveness: LivenessInfoCollector.AllStates): Unit
      def setUntypedStackSlotsCount(untypedStackSlotsCount: Int): Unit
      def setUsedNonVolIRegsMask(usedNonVolIRegsMask: Int): Unit
      def setUsedNonVolFRegsMask(usedNonVolFRegsMask: Int): Unit
      def setMaxCalleeStackArgsCount(maxCalleeStackArgsCount: Int): Unit
      def setMayHaveNativeCalls(mayHaveNativeCalls: Boolean): Unit
      def setStackAllocatedTypeSigs(stackAllocatedTypeSigs: Seq[Signature]): Unit
      def setVariableSizeTypes(variableSizeTypes: Seq[Signature]): Unit
    }
  }

  object Field {
    trait Builder {
      def setName(name: String): Unit
      def setFieldType(signature: Signature): Unit
      def addFlag(flag: FieldFlag): Unit
      def setConstValue(tag: FieldTag, value: Long): Unit
    }
  }
}

private class CbcFileFormatBuilder extends CbcFileFormat.Builder {
  private val typeBuilders = ArrayBuffer.empty[TypeBuilder]

  private var bytecodeVersion: Int = 0
  private var mainTypeName: String = _
  private var cbcDeps: String = _
  private var aotDeps: String = _
  private var foreignLibs: String = _

  override def setBytecodeVersion(version: Int): Unit = this.bytecodeVersion = version
  override def setMainTypeName(name: String): Unit = this.mainTypeName = name

  override def setCbcDeps(deps: String): Unit = this.cbcDeps = deps
  override def setAotDeps(deps: String): Unit = this.aotDeps = deps
  override def setForeignLibs(libs: String): Unit = this.foreignLibs = libs

  override def newTypeBuilder(): CbcFileFormat.Type.Builder = {
    val builder = new TypeBuilder()
    typeBuilders += builder
    builder
  }

  override def build(): CbcFile = CbcFile(
    bytecodeVersion = bytecodeVersion,
    mainTypeName = Option(mainTypeName),
    cbcDeps = Option(cbcDeps),
    aotDeps = Option(aotDeps),
    foreignLibs = Option(foreignLibs),
    types = typeBuilders.toSeq.map(_.build())
  )

  private class TypeBuilder extends CbcFileFormat.Type.Builder {
    private val methodBuilders = ArrayBuffer.empty[MethodBuilder]
    private val fieldBuilders = ArrayBuffer.empty[FieldBuilder]

    private var name: String = _
    private var superOrEnumType: Signature = _
    private var flags: Int = 0
    private var interfaces: Seq[Signature] = Seq.empty
    private var genericConstraints: Seq[Signature] = Seq.empty
    private var unionFields: Seq[Signature] = Seq.empty
    private var enumKind: TypeEnumKind = NotEnum

    override def setName(name: String): Unit = this.name = name
    override def getName: Option[String] = Option(name)
    override def setSuperOrEnumType(tpe: Signature): Unit = this.superOrEnumType = tpe
    override def setInterfaces(interfaces: Seq[Signature]): Unit = this.interfaces = interfaces
    override def setGenericConstraints(genericConstraints: Seq[Signature]): Unit = this.genericConstraints = genericConstraints

    override def setUnionFields(fields: Seq[Signature]): Unit = this.unionFields = fields
    override def setEnumKind(enumKind: TypeEnumKind): Unit = this.enumKind = enumKind

    override def addFlag(flag: TypeFlag): Unit = {
      flags |= flag.mask
    }

    override def newMethodBuilder(): Method.Builder = {
      val builder = new MethodBuilder
      methodBuilders += builder
      builder
    }

    override def newFieldBuilder(): Field.Builder = {
      val builder = new FieldBuilder
      fieldBuilders += builder
      builder
    }

    def build(): CbcFileFormat.Type = Type(
      name = name.nn,
      superOrEnumType = Option(superOrEnumType),
      flags = TypeFlags(flags), // TODO: consistency check
      methods = methodBuilders.toSeq.map(_.build()),
      fields = fieldBuilders.toSeq.map(_.build()),
      interfaces = interfaces,
      genericConstraints = genericConstraints,
      enumKind = enumKind,
      unionFields = unionFields
    )
  }

  private class MethodBuilder extends CbcFileFormat.Method.Builder {
    private var name: String = _
    private var typeName: String = _
    private var signature: Signature = _
    private var codeBuilder: Option[MethodCodeBuilder] = None
    private var flags: Int = 0
    private var linkageName: Option[String] = None
    private var sourceFullName: Option[String] = None
    private var sourceFile: Option[String] = None

    override def setName(name: String): Unit = this.name = name

    override def setTypeName(typeName: String): Unit = this.typeName = typeName
    override def setSignature(signature: Signature): Unit = this.signature = signature.ensuring(_.isInstanceOf[Functional])

    override def getCodeBuilder(): MethodCodeBuilder = {
      if (codeBuilder.isEmpty) {
        codeBuilder = Some(new MethodCodeBuilder)
      }
      codeBuilder.get
    }

    override def addFlag(flag: MethodFlag): Unit = {
      flags |= flag.mask
    }

    override def setLinkageName(fullName: String): Unit = this.linkageName = Some(fullName)
    override def setSourceFullName(linkageName: String): Unit = this.sourceFullName = Some(linkageName)
    override def setSourceFile(fileName: String): Unit = this.sourceFile = Some(fileName)

    def build(): CbcFileFormat.Method = Method(
      name = name.nn,
      typeName = if typeName != null then typeName else "",
      signature = signature.nn,
      code = codeBuilder.map(_.build()),
      flags = MethodFlags(flags),
      sourceFullName = sourceFullName,
      sourceFile = sourceFile,
      linkageName = linkageName
    )
  }
  
  private class MethodCodeBuilder extends CbcFileFormat.MethodCode.Builder {
    private var segment: Segment = _
    private var exTable: ExceptionTable = ExceptionTable(Seq.empty)
    private var liveness: LivenessInfoCollector.AllStates = _
    private var untypedStackSlotsCount: Int = 0
    private var usedNonVolIRegsMask: Int = 0
    private var usedNonVolFRegsMask: Int = 0
    private var maxCalleeStackArgsCount: Int = 0
    private var mayHaveNativeCalls: Boolean = false
    private var stackAllocatedTypeSigs: Seq[Signature] = Seq.empty
    private var variableSizeTypes: Seq[Signature] = Seq.empty

    def setSegment(segment: Segment): Unit = { this.segment = segment }
    def setExceptionTable(exTable: ExceptionTable): Unit = { this.exTable = exTable }
    def setLiveness(liveness: LivenessInfoCollector.AllStates): Unit = { this.liveness = liveness }
    def setUntypedStackSlotsCount(untypedStackSlotsCount: Int): Unit = { this.untypedStackSlotsCount = untypedStackSlotsCount }
    def setUsedNonVolIRegsMask(usedNonVolIRegsMask: Int): Unit = { this.usedNonVolIRegsMask = usedNonVolIRegsMask }
    def setUsedNonVolFRegsMask(usedNonVolFRegsMask: Int): Unit = { this.usedNonVolFRegsMask = usedNonVolFRegsMask }
    def setMaxCalleeStackArgsCount(maxCalleeStackArgsCount: Int): Unit = { this.maxCalleeStackArgsCount = maxCalleeStackArgsCount }
    def setMayHaveNativeCalls(mayHaveNativeCalls: Boolean): Unit = { this.mayHaveNativeCalls = mayHaveNativeCalls }
    def setStackAllocatedTypeSigs(stackAllocatedTypeSigs: Seq[Signature]): Unit = { this.stackAllocatedTypeSigs = stackAllocatedTypeSigs }
    def setVariableSizeTypes(variableSizeTypes: Seq[Signature]): Unit = { this.variableSizeTypes = variableSizeTypes }

    def build(): CbcFileFormat.MethodCode = MethodCode(segment, exTable, liveness, untypedStackSlotsCount,
      usedNonVolIRegsMask, usedNonVolFRegsMask, maxCalleeStackArgsCount,
      mayHaveNativeCalls, stackAllocatedTypeSigs, variableSizeTypes)
  }

  private class FieldBuilder extends CbcFileFormat.Field.Builder {
    private var name: String = _
    private var fieldType: Signature = _
    private var flags: Int = 0
    private var constValue: Option[(FieldTag, Long)] = None

    override def setName(name: String): Unit = this.name = name
    override def setFieldType(signature: Signature): Unit = this.fieldType = signature

    override def addFlag(flag: FieldFlag): Unit = {
      flags |= flag.mask
    }

    override def setConstValue(tag: FieldTag, value: Long): Unit = this.constValue = Some(tag, value)

    def build(): CbcFileFormat.Field = Field(
      name = name.nn,
      fieldType = fieldType.nn,
      flags = FieldFlags(flags),
      constValue
    )
  }
}
