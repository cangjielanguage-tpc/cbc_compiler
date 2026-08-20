/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.{Segment, Symbol}
import com.huawei.excelsior.jet.assembler.cbc.CbcFileEncoder.{FILE_VERSION, Index, Offset}
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.TypeEnumKind.{NotEnum, Option0, Option1, Primitive, Union}
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{BuiltinSignature, *}
import com.huawei.excelsior.jet.assembler.cbc.FieldTag.{SlebConst, U64Const, UlebConst}
import com.huawei.excelsior.jet.assembler.cbc.Utils.writeSequence
import com.huawei.excelsior.jet.assembler.cbc.isa12.LivenessInfoCollector
import com.huawei.excelsior.jet.assembler.fixups.RelocationKind
import com.huawei.excelsior.jet.assembler.cbc.isa12.forked.Fixups
import xscala.io.{ByteBuffer, DataOutput, LEB128Encoder, TextOutput}
import xscala.util.MathUtils

import scala.annotation.nowarn
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object CbcFileEncoder {
  val FILE_VERSION: Byte = 1

  type Index = Int
  type Offset = Int

  def gen(file: CbcFile, output: DataOutput): Unit =
    CbcFileEncoder(file).generate(output)

  def apply(file: CbcFile): CbcFileEncoder = {
    val generator = new CbcFileEncoder(file)
    file.types.foreach(generator.addType)
    generator
  }

  private def defaultSymbolAdapter(s: Symbol): BytecodeReferenceSymbol =
    s.asInstanceOf[BytecodeReferenceSymbol]
}

class Statistics {
  private val counters: Array[Int] = new Array[Int](StatTag.values.length)

  def listener(statTag: StatTag): Int => Unit = size => {
    counters(statTag.ordinal) += size
  }

  def count(statTag: StatTag, out: DataOutput)(gen: DataOutput => Unit): Unit = {
    var counter = 0
    val countingStream = new DataOutput {
      override def putByte(b: Int): Unit = {
        counter += 1
        out.putByte(b)
      }

      override def putBytes(data: Array[Byte], offset: Index, size: Index): Unit = {
        counter += size
        out.putBytes(data, offset, size)
      }

      override def putBytes(data: Array[Byte]): Unit = {
        counter += data.length
        out.putBytes(data)
      }
    }

    gen(countingStream)
    counters(statTag.ordinal) += counter
  }

  def print(out: TextOutput): Unit = {
    val roots = StatTag.values.filter(_.isRoot)

    def dump(statTag: StatTag, indent: String): Unit = {
      out.println(s"$indent- ${statTag.name}: ${counters(statTag.ordinal)}")
      for (c <- statTag.children) {
        dump(c, s"$indent  ")
      }
    }
    for (tag <- roots) {
      dump(tag, "")
    }
  }
}

enum StatTag(val name: String, val children: StatTag*) {
  case MC_Instructions extends StatTag("instructions")
  case MC_Exceptions extends StatTag("exceptions")
  case MC_GCData extends StatTag("gc maps")
  case MC_StackChecks extends StatTag("stack maps")

  case String extends StatTag("strings")
  case ByteArray extends StatTag("raw data")
  case Signature extends StatTag("signature types")
  case MethodCode extends StatTag("method code", MC_Instructions, MC_Exceptions, MC_GCData, MC_StackChecks)
  case FieldDef extends StatTag("field definitions")
  case FieldRef extends StatTag("field references")
  case MethodDef extends StatTag("method definitions")
  case MethodRef extends StatTag("method references")
  case TypeDef extends StatTag("type definitions")
  case AotData extends StatTag("aot data")

  case TypeIndex extends StatTag("type index")
  case RefIndex extends StatTag("ref index")
  case AotDataTables extends StatTag("aot data tables")

  lazy val isRoot = !StatTag.values.exists(_.children.contains(this))
}

class CbcFileEncoder(file: CbcFile) { self =>
  private val globalPoolBuffer: ByteBuffer = new ByteBuffer()

  private val statistics = Statistics()

  private val strings = DedupPool(CountingPool(new StringPool with PoolView with GlobalPool, statistics.listener(StatTag.String)))
  private val byteArrays = DedupPool(CountingPool(new ByteArrayPool with GlobalPool, statistics.listener(StatTag.ByteArray)))
  private val signaturePool = DedupPool(CountingPool(new SignaturePool with PoolView with GlobalPool, statistics.listener(StatTag.Signature)))
  private val methodCodes = CountingPool(new MethodCodePool(statistics) with PoolView with GlobalPool, statistics.listener(StatTag.MethodCode))
  private val methods = DedupPool(CountingPool(new MethodPool with PoolView with GlobalPool, statistics.listener(StatTag.MethodDef)))
  private val fields = CountingPool(new FieldPool with PoolView with GlobalPool, statistics.listener(StatTag.FieldDef))
  private val fieldRefPool = DedupPool(CountingPool(new FieldReferencePool with PoolView with GlobalPool, statistics.listener(StatTag.FieldRef)))
  private val methodRefPool = DedupPool(CountingPool(new MethodReferencePool with PoolView with GlobalPool, statistics.listener(StatTag.MethodRef)))
  private val typeIndex = new MemberTable[Type](CountingPool(new TypePool with PoolView with GlobalPool, statistics.listener(StatTag.TypeDef)))

  private val aotTablePool = DedupPool(CountingPool(new AotDataPool with PoolView with GlobalPool, statistics.listener(StatTag.AotData)))
  private val directCallAotTable = new AotTable(aotTablePool)
  private val virtualCallAotTable = new AotTable(aotTablePool)
  private val interfaceCallAotTable = new AotTable(aotTablePool)
  private val staticFieldAotTable = new AotTable(aotTablePool)
  private val instanceFieldAotTable = new AotTable(aotTablePool)

  // TODO: To support regions:
  //       1. Write current region index to MethodCode and definitions.
  //       2. Make MethodCode fixup resolution non-destructive, so it could be repeated again on index overflow.
  //       3. Make separate pools for definitions and code. It will allow to roll them back safely on index overflow,
  //          without destructing pools for strings/signatures ant etc.
  //       4. Advance `regionIndex` on index overflow and re-build following tables.
  // private var regionIndex = 0
  private val signatures = new SignatureTable(signaturePool)
  private val fieldRefs = new FieldRefTable(fieldRefPool) with PoolView with GlobalPool
  private val methodRefs = new MethodRefTable(methodRefPool) with PoolView with GlobalPool

  private trait GlobalPool extends RawPool {
    def buffer: ByteBuffer = self.globalPoolBuffer
  }

  private trait PoolView extends PoolProvider {
    def strings = self.strings
    def byteArrays = self.byteArrays
    def signatures = self.signatures
    def fields = self.fields
    def methods = self.methods
    def methodCodes = self.methodCodes
    def methodRefs = self.methodRefs
    def fieldRefs = self.fieldRefs
    def directCallAotTable = self.directCallAotTable
    def virtualCallAotTable = self.virtualCallAotTable
    def interfaceCallAotTable = self.interfaceCallAotTable
    def staticFieldAotTable = self.staticFieldAotTable
    def instanceFieldAotTable = self.instanceFieldAotTable
  }

  def printStats(out: TextOutput): Unit = statistics.print(out)

  def stats(): String = {
    val b = StringBuilder()
    printStats(TextOutput(b))
    b.toString()
  }

  def addType(tpe: Type): Unit = typeIndex.add(tpe)

  def generate(output: DataOutput): Unit = {
    val layout = Layout()
    layout.bytes { buffer =>
      buffer.putBytes('C', 'B', 'C', FILE_VERSION) // magic & file_version
      buffer.putByte(file.bytecodeVersion) // bytecode_version
      buffer.putByte(0) // file_properties
    }

    val typeIndexOffs = layout.offset() // type_idx_offs
    val poolOffset = layout.offset()

    val directCallAotTableOffset = layout.offset()
    val virtualCallAotTableOffset = layout.offset()
    val interfaceCallAotTableOffset = layout.offset()
    val staticFieldAotTableOffset = layout.offset()
    val instanceFieldAotTableOffset = layout.offset()

    layout.w16(1) // num_index_regions
    val indexSectionOffs = layout.offset() // index_section_off[0]

    layout.w32(file.mainTypeName.map(strings.add).getOrElse(-1)) // main type
    layout.w32(file.cbcDeps.map(strings.add).getOrElse(-1))      // cbc package dependencies
    layout.w32(file.aotDeps.map(strings.add).getOrElse(-1))      // aot lib dependencies
    layout.w32(file.foreignLibs.map(strings.add).getOrElse(-1))  // foreign libs
    layout.uleb(0) // coverage id

    poolOffset.elem = layout.bytes(globalPoolBuffer.toByteArray)

    layout.setStatListener(statistics.listener(StatTag.TypeIndex))
    typeIndexOffs.elem = layout.bytes(typeIndex.asByteArray())

    layout.setStatListener(statistics.listener(StatTag.AotDataTables))
    directCallAotTableOffset.elem = layout.bytes(directCallAotTable.asByteArray())
    virtualCallAotTableOffset.elem = layout.bytes(virtualCallAotTable.asByteArray())
    interfaceCallAotTableOffset.elem = layout.bytes(interfaceCallAotTable.asByteArray())
    staticFieldAotTableOffset.elem = layout.bytes(staticFieldAotTable.asByteArray())
    instanceFieldAotTableOffset.elem = layout.bytes(instanceFieldAotTable.asByteArray())

    layout.setStatListener(statistics.listener(StatTag.RefIndex))
    val methodRefIndex  = layout.bytes(methodRefs.asByteArray())
    val fieldRefIndex = layout.bytes(fieldRefs.asByteArray())
    val signatureIndex = layout.bytes(signatures.asByteArray())

    // regions data
    layout.alignment(4) // TODO: why only here?
    indexSectionOffs.elem = layout.w16(0) // type_idx_size TODO: remove
    layout.w32(0) // type_idx_off TODO: remove
    layout.uleb(methodRefs.size) // method_idx_size
    layout.offset(methodRefIndex) // method_idx_off
    layout.uleb(fieldRefs.size) // field_idx_size
    layout.offset(fieldRefIndex) // field_idx_off
    layout.uleb(signatures.size) // sig_idx_size
    layout.offset(signatureIndex) // sig_idx_off

    layout.setStatListener(null)

    layout.write(output)
  }
}

private final class Layout { self =>

  private val elementPositions = mutable.LinkedHashMap[Element, Int]()
  private val elementStatListener = mutable.LinkedHashMap[Element, Int => Unit]()
  private var listener: Int => Unit = _

  def setStatListener(listener: Int => Unit): Unit = this.listener = listener

  def alignment(value: Int): Alignment = add(Alignment(value))
  def bytes(arr: Array[Byte]): Bytes = add(Bytes(arr))
  def w16(value: Int): W16 = add(W16(value))
  def w32(value: Int): W32 = add(W32(value))
  def uleb(value: Int): ULEB = add(ULEB(value))
  def offset(): Offset = add(Offset(null))
  def offset(elem: Element): Offset = add(Offset(elem))

  def bytes(f: ByteBuffer => Unit): Bytes = {
    val buffer = ByteBuffer()
    f(buffer)
    add(Bytes(buffer.toByteArray))
  }

  def write(out: DataOutput): Unit = {
    var position = 0
    for (elem <- elementPositions.keys) {
      elementPositions(elem) = position
      elem match {
        case alignment: Alignment =>
          val oldPos = position
          position = MathUtils.alignUp(oldPos, alignment.value)
          alignment.size = position - oldPos
        case _ =>
          position += elem.size
      }
    }
    elementStatListener.foreach((elem, statListener) => {
      if (statListener != null) {
        statListener(elem.size)
      }
    })
    elementPositions.keys.foreach(_.write(out))
  }

  private def add[T <: Element](elem: T): T = {
    elementPositions.put(elem, 0)
    elementStatListener.put(elem, listener)
    elem
  }

  private def position(elem: Element): Int = elementPositions(elem)

  sealed trait Element {
    def size: Int
    def write(out: DataOutput): Unit
  }

  class Alignment private[Layout](val value: Int) extends Element {
    var size: Int = 0
    def write(out: DataOutput): Unit = out.putZeroes(size)
  }

  class Bytes private[Layout](val arr: Array[Byte]) extends Element {
    override def size: Int = arr.length
    override def write(out: DataOutput): Unit = out.putBytes(arr)
  }

  class W16 private[Layout](val value: Int) extends Element {
    override def size: Int = 2
    override def write(out: DataOutput): Unit = out.putW16(value)
  }

  class W32 private[Layout](val value: Int) extends Element {
    override def size: Int = 4
    override def write(out: DataOutput): Unit = out.putW32(value)
  }

  class ULEB private[Layout](val value: Int) extends Element {
    override def size: Int = LEB128Encoder.calcSizeULEB128(value)
    override def write(out: DataOutput): Unit = out.putULEB(value)
  }

  class Offset private[Layout](var elem: Element) extends Element {
    override def size: Int = 4
    override def write(out: DataOutput): Unit = out.putW32(self.position(elem.nn))
  }
}

private enum SignatureTag(val tag: Byte) {
  case Nil              extends SignatureTag(0x00)
  case Reference        extends SignatureTag(0x01)
  case AotReference     extends SignatureTag(0x02)
  case CangjieArray     extends SignatureTag(0x03)
  case VArray           extends SignatureTag(0x04)
  case EnumWrapper      extends SignatureTag(0x05)
  case CPointer         extends SignatureTag(0x06)
  case FuncTypeVar      extends SignatureTag(0x07)
  case ClassTypeVar     extends SignatureTag(0x08)
  case GenericRecord    extends SignatureTag(0x09)
  case GenericReference extends SignatureTag(0x0a)
  case Nullable         extends SignatureTag(0x0b)
  case Functional       extends SignatureTag(0x0c)
  case Record           extends SignatureTag(0x0d)
  case AotRecord        extends SignatureTag(0x0e)
  case NonNullable      extends SignatureTag(0x0f)
  case GenericAotRef    extends SignatureTag(0x10)
  case GenericAotRec    extends SignatureTag(0x11)
  case Tuple            extends SignatureTag(0x12)
  case Box              extends SignatureTag(0x13)
  case Fst              extends SignatureTag(0x14)
  case Option           extends SignatureTag(0x15)
  case UnionEnum        extends SignatureTag(0x16)
  case PrimitiveEnum    extends SignatureTag(0x17)
}

private enum TypeTag(val tag: Byte) {
  case Nothing extends TypeTag(0x00)
  case Interfaces extends TypeTag(0x01)
  case GenericParameters extends TypeTag(0x05)
  case UnionFields extends TypeTag(0x06)
  case Option0 extends TypeTag(0x07)
  case Option1 extends TypeTag(0x08)
  case Primitive extends TypeTag(0x09)
}

private enum MethodTag(val tag: Byte) {
  case Nothing extends MethodTag(0x00)
  case Code extends MethodTag(0x01)
  case SourceFullName extends MethodTag(0x02)
  case SourceFile extends MethodTag(0x03)
  case LinkageName extends MethodTag(0x04)
  case GenericParameters extends MethodTag(0x05)
}

private enum FieldTag(val tag: Byte) {
  case Nothing extends FieldTag(0x00)
  case SlebConst extends FieldTag(0x01)
  case UlebConst extends FieldTag(0x02)
  case U64Const extends FieldTag(0x03)
  case AnnotationFactoryIndex extends FieldTag(0x04)
  case MangleKind extends FieldTag(0x05)
  case PrebuiltOffset extends FieldTag(0x06)
}

/**
  * Represents a continuous binary memory segment that could store [[Data]] objects.
  */
private trait Pool[Data] {
  /**
    * @return a position as [[Offset]] in the memory segment, where object is stored.
    */
  def add(data: Data): Offset
}

private trait PoolProvider {
  def strings: Pool[String]
  def byteArrays: Pool[ArraySeq[Byte]]
  def signatures: Table[Signature]
  def fields: Pool[Field]
  def methods: Pool[Method]
  def methodCodes: Pool[MethodCode]
  def methodRefs: Table[MethodReference]
  def fieldRefs: Table[FieldReference]
  def directCallAotTable: AotTable
  def virtualCallAotTable: AotTable
  def interfaceCallAotTable: AotTable
  def staticFieldAotTable: AotTable
  def instanceFieldAotTable: AotTable
}

/**
  * Represents a table of [[Data]] objects, which are stored in [[pool]].
  * Each object is referenced via [[Index]] to [[Offset]] in underlying [[pool]].
  */
private class Table[Data](pool: Pool[Data]) {
  val map = mutable.Map.empty[Offset, Index]

  def add(data: Data): Index = {
    val offs = pool.add(data)
    map.getOrElseUpdate(offs, map.size)
  }

  def size = map.size

  def serialize(out: DataOutput): Unit = {
    val arr = new Array[Int](map.size)
    map.foreach((offs, idx) => arr(idx) = offs)
    arr.foreach(out.putW32)
  }

  def asByteArray(): Array[Byte] = {
    val buffer = ByteBuffer()
    serialize(buffer)
    buffer.toByteArray
  }
}

private class SignatureTable(pool: Pool[Signature]) extends Table[Signature](pool) {
  override def add(data: Signature) = data match {
    case BuiltinSignature(id: Int) => id
    case _ => super.add(data) + BuiltinSignature.count
  }
}

private class MethodRefTable(pool: Pool[MethodReference]) extends Table[MethodReference](pool) {
  self: RawPool with PoolProvider =>

  override def add(data: MethodReference): Index = {
    val idx = super.add(data)
    data.aotData.foreach {
      case x: DirectCallAotData => directCallAotTable.add(idx, IndexedAotData(idx, x))
      case x: VirtualCallAotData => virtualCallAotTable.add(idx, IndexedAotData(idx, x))
      case x: InterfaceCallAotData => interfaceCallAotTable.add(idx, IndexedAotData(idx, x))
      case _ =>
    }
    idx
  }
}

private class FieldRefTable(pool: Pool[FieldReference]) extends Table[FieldReference](pool) {
  self: RawPool with PoolProvider =>

  @nowarn("msg=match may not be exhaustive")
  override def add(data: FieldReference): Index = {
    val idx = super.add(data)
    data.refType match {
      case _: AotTypeSignature => data.aotData.get match {
        case x: StaticFieldAotData => staticFieldAotTable.add(idx, IndexedAotData(idx, x))
        case x: InstanceFieldAotData => instanceFieldAotTable.add(idx, IndexedAotData(idx, x))
      }
      case _: TypeSignature => // TODO support. Intentionally empty for now
    }
    idx
  }
}

private abstract class BucketBasedHashTable[Key, Data](pool: Pool[Data]) {
  private type bucketId = Int
  private val loadFactor = 0.75
  private val entries = mutable.ArrayBuffer.empty[(Key, Offset)]

  def size = entries.size

  def add(key: Key, data: Data): Unit = {
    val offs = pool.add(data)
    entries.append((key, offs))
  }

  def hash(key: Key): Int

  private def encode(): (Seq[bucketId], Seq[Offset]) = {
    val entries = this.entries.distinct
    val N = entries.size ensuring (_ >= 0)
    val d = ((1 / loadFactor) - 1) ensuring (_ > 0)
    val bucketCount = math.max(((1 + d) * N).toInt, N)

    val buckets = Array.fill(bucketCount)(mutable.Seq.empty[Offset])
    for ((key, offset) <- entries) {
      val idx = MathUtils.urem(hash(key), bucketCount)
      buckets(idx) = buckets(idx) :+ offset
    }

    val bucketTable = new Array[bucketId](bucketCount + 1)
    var pointer = 0
    for (i <- buckets.indices) {
      bucketTable(i) = pointer
      pointer += buckets(i).size
    }

    assert(pointer == N)
    bucketTable(bucketCount) = N

    (bucketTable.toSeq, buckets.toSeq.flatten)
  }

  def serialize(out: DataOutput): Unit = {
    val (bucketTable, buckets) = encode()
    out.putW32(bucketTable.size);
    out.putW32(buckets.size);
    bucketTable.foreach(out.putW32)
    buckets.foreach(out.putW32)
  }

  def asByteArray(): Array[Byte] = {
    val buffer = ByteBuffer()
    serialize(buffer)
    buffer.toByteArray
  }
}

private class MemberTable[Data <: Named](pool: Pool[Data]) extends BucketBasedHashTable[String, Data](pool) {

  def add(data: Data): Unit = add(data.name, data)

  override def hash(name: String): Int = {
    val bytes = name.getBytes("UTF-8")
    var h = 0
    for (i <- bytes) {
      h = (h << 5) - h + (i & 0xff)
    }
    h
  }
}

private class AotTable(pool: Pool[IndexedAotData]) extends BucketBasedHashTable[Index, IndexedAotData](pool) {
  override def hash(idx: Index): Int = idx
}

private trait RawPool {
  def buffer: ByteBuffer

  final def put(f: DataOutput => Unit): Int = {
    val data = ByteBuffer()
    f(data)

    val offset = buffer.length
    buffer.putBytes(data.toByteArray)
    offset
  }
}

private final class CountingPool[Data](private val pool: Pool[Data] & RawPool, listener: Int => Unit) extends Pool[Data] {
  override def add(data: Data): Offset = {
    val offset = pool.add(data)
    val size = pool.buffer.length - offset
    assert(size >= 0)
    listener(size)
    offset
  }
}

private final class DedupPool[Data](private val pool: Pool[Data]) extends Pool[Data] {
  private val dedupTable = mutable.Map.empty[Data, Offset]

  override def add(data: Data): Offset = {
    dedupTable.get(data) match {
      case Some(offset) => offset
      case _ =>
        val offset = pool.add(data)
        dedupTable.put(data, offset)
        offset
    }
  }
}

private class StringPool extends Pool[String] { self: RawPool =>
  override def add(data: String): Offset = put { out =>
    val bytes = data.getBytes("UTF-8")
    out.putULEB(bytes.length)
    out.putBytes(bytes)
  }
}

private class ByteArrayPool extends Pool[ArraySeq[Byte]] { self: RawPool =>
  override def add(bytes: ArraySeq[Byte]): Offset = put { out =>
    out.putBytes(bytes.toArray)
  }
}

private class SignaturePool extends Pool[Signature] { self: RawPool with PoolProvider =>
  override def add(data: Signature): Offset = put { output =>
    data match {
      case TypeSignature(name, Seq(), isReference) =>
        val stringOffs = strings.add(name)
        val tag = if (isReference) SignatureTag.Reference else SignatureTag.Record
        output.putW8(tag.tag)
        output.putULEB(stringOffs)
      case TypeSignature(name, args, isReference) =>
        val stringOffs = strings.add(name)
        val tag = if (isReference) SignatureTag.GenericReference else SignatureTag.GenericRecord
        output.putW8(tag.tag)
        output.putULEB(stringOffs)
        output.putW8(args.size) // TODO: uleb?
        args.map(signatures.add).foreach(output.putULEB)
      case AotTypeSignature(name, Seq(), isReference) => // FIXME: add and handle args
        val tag = if (isReference) SignatureTag.AotReference else SignatureTag.AotRecord
        output.putW8(tag.tag)
        output.putULEB(strings.add(name))
      case AotTypeSignature(name, args, isReference) => // FIXME: add and handle args
        val tag = if (isReference) SignatureTag.GenericAotRef else SignatureTag.GenericAotRec
        output.putW8(tag.tag)
        output.putULEB(strings.add(name)) // TODO: uleb?
        output.putW8(args.length)
        args.map(signatures.add).foreach(output.putULEB)
      case Functional(args, retType) =>
        output.putW8(SignatureTag.Functional.tag)
        output.putW8(args.size) // TODO: uleb?
        args.map(signatures.add).foreach(output.putULEB)
        output.putULEB(signatures.add(retType))
      case Tuple(args) =>
        output.putW8(SignatureTag.Tuple.tag)
        output.putULEB(args.size)
        args.map(signatures.add).foreach(output.putULEB)
      case CangjieArray(tpe) =>
        output.putW8(SignatureTag.CangjieArray.tag)
        output.putULEB(signatures.add(tpe))
      case Nullable(tpe) =>
        output.putW8(SignatureTag.Nullable.tag)
        output.putULEB(signatures.add(tpe))
      case NonNullable(tpe) =>
        output.putW8(SignatureTag.NonNullable.tag)
        output.putULEB(signatures.add(tpe))
      case CPointer(tpe) =>
        output.putW8(SignatureTag.CPointer.tag)
        output.putULEB(signatures.add(tpe))
      case VArray(tpe, length) =>
        output.putW8(SignatureTag.VArray.tag)
        output.putULEB(length)
        output.putULEB(signatures.add(tpe))
      case ClassTypeVariable(id) =>
        output.putW8(SignatureTag.ClassTypeVar.tag)
        output.putW8(id)
      case FuncTypeVariable(id) =>
        output.putW8(SignatureTag.FuncTypeVar.tag)
        output.putW8(id)
      case Box(sig) =>
        output.putW8(SignatureTag.Box.tag)
        output.putULEB(signatures.add(sig))
      case Fst(sig) =>
        output.putW8(SignatureTag.Fst.tag)
        output.putULEB(signatures.add(sig))
      case OptionSignature(name, args, _) => // TODO: add short form of encoding
        output.putW8(SignatureTag.Option.tag)
        output.putULEB(strings.add(name))
        output.putW8(args.length)
        args.map(signatures.add).foreach(output.putULEB)
      case UnionEnum(name, args) => // TODO: add short form of encoding
        output.putW8(SignatureTag.UnionEnum.tag)
        output.putULEB(strings.add(name))
        output.putW8(args.length)
        args.map(signatures.add).foreach(output.putULEB)
      case PrimitiveEnum(name, args) => // TODO: add short form of encoding
        output.putW8(SignatureTag.PrimitiveEnum.tag)
        output.putULEB(strings.add(name))
        output.putW8(args.length)
        args.map(signatures.add).foreach(output.putULEB)
      case _ => assert(false, s"ShouldNotReachHere: writing $data to CBC file")
    }
  }
}

private class FieldReferencePool extends Pool[FieldReference] { self: RawPool with PoolProvider =>
  override def add(data: FieldReference): Offset = put { output =>
    output.putW32(strings.add(data.name))
    output.putULEB(signatures.add(data.refType))
    output.putULEB(signatures.add(data.fieldType))
  }
}

private class MethodReferencePool extends Pool[MethodReference] { self: RawPool with PoolProvider =>
  override def add(data: MethodReference): Offset = put { output =>
    output.putW32(strings.add(data.name))

    val hasTVars = data.typeVars.nonEmpty
    var flags = data.flags.mask
    if (data.aotData.isDefined) flags |= MethodRefFlag.AOT.mask
    if (hasTVars) flags |= MethodRefFlag.HAS_FTVARS.mask
    output.putW8(flags)

    output.putULEB(signatures.add(data.refType))
    output.putULEB(signatures.add(data.signature))

    if (hasTVars) output.putULEB(signatures.add(Tuple(data.typeVars)))
  }
}

private class MethodCodePool(stats: Statistics) extends Pool[MethodCode] { self: RawPool with PoolProvider =>
  override def add(data: MethodCode): Offset = put { output =>
    output.putULEB(data.untypedStackSlotsCount)
    output.putULEB(data.stackAllocatedTypeSigs.length)
    data.stackAllocatedTypeSigs.map(signatures.add).foreach(output.putULEB)
    output.putULEB(data.variableSizeTypes.length)
    data.variableSizeTypes.map(signatures.add).foreach(output.putULEB)
    output.putW8(data.usedNonVolIRegsMask)
    output.putW8(data.usedNonVolFRegsMask)
    output.putULEB(data.maxCalleeStackArgsCount)
    output.putW8(if (data.mayHaveNativeCalls) 1 else 0)

    assert(!data.segment.frozen)
    prepareReferenceFixups(data.segment)
    data.segment.finish((_, _, _) => {})

    stats.count(StatTag.MC_Instructions, output) { output =>
      output.putULEB(data.segment.length)
      output.putULEB(0) // FIXME: remove literal offset
      output.putBytes(data.segment.toByteArray)
    }

    stats.count(StatTag.MC_Exceptions, output) { output =>
      if (data.exTable.regionRefs.nonEmpty) {
        outExTable(output, data.segment, data.exTable)
      } else {
        output.putULEB(0)
      }
    }

    stats.count(StatTag.MC_GCData, output) { output =>
      if (data.liveness.liveStates != null) {
        outLiveStates(output, data.liveness.liveStates)
      } else {
        output.putULEB(0)
      }
    }

    stats.count(StatTag.MC_GCData, output) { output =>
      if (data.liveness.stackCheckStates != null) {
        outStackCheckStates(output, data.liveness.stackCheckStates)
      } else {
        output.putULEB(0)
      }
    }
  }

  private def prepareReferenceFixups(segment: Segment): Unit = {
    segment.getFixups foreach {
      case fixup: Fixups.Reference => fixup.setId(fixup.target.ref match {
        case sig: Signature => signatures.add(sig)
        case ref: MethodReference => methodRefs.add(ref)
        case ref: FieldReference => fieldRefs.add(ref)
        case literal: StringLiteral => strings.add(literal.s)
        case data: RawData => byteArrays.add(data.data)
      })
      case _ =>
    }
  }

  private def outExTable(output: DataOutput, segment: Segment, exTable: ExceptionTable): Unit = {
    val exTableBuffer = new ByteBuffer()
    exTable.resolve(segment).flatMap(r => Seq(r.start, r.end, r.target)).foreach(exTableBuffer.putULEB)
    output.putULEB(exTableBuffer.length)
    output.putBytes(exTableBuffer.toByteArray)
  }

  // TODO encode more efficiently
  private def outLiveStates(output: DataOutput, savedStates: Seq[LivenessInfoCollector.LiveState]): Unit = {
    val livenessInfoBuffer = new ByteBuffer()
    for (state <- savedStates) {
      livenessInfoBuffer.putULEB(state.label.position)
      livenessInfoBuffer.putW16(state.regMask)
      livenessInfoBuffer.putULEB(state.untypedSlots.length)
      state.untypedSlots.foreach(livenessInfoBuffer.putULEB)
      livenessInfoBuffer.putULEB(state.derivedPairs.length)
      state.derivedPairs.foreach { (base, derived) =>
        livenessInfoBuffer.putULEB(base)
        livenessInfoBuffer.putULEB(derived)
      }
    }
    output.putULEB(livenessInfoBuffer.length)
    output.putBytes(livenessInfoBuffer.toByteArray)
  }

  // TODO encode more efficiently
  private def outStackCheckStates(output: DataOutput, savedStates: Seq[LivenessInfoCollector.StackCheckState]): Unit = {
    val infoBuilder = new ByteBuffer()
    for (state <- savedStates) {
      infoBuilder.putULEB(state.label.position)
      infoBuilder.putULEB(state.stackPtrHolders.length)
      state.stackPtrHolders.foreach { resource =>
        infoBuilder.putULEB(resource)
      }
    }
    output.putULEB(infoBuilder.length)
    output.putBytes(infoBuilder.toByteArray)
  }
}

private class MethodPool extends Pool[Method] { self: RawPool with PoolProvider =>
  override def add(data: Method): Offset = put { output =>
    output.putW32(strings.add(data.name))
    output.putW32(strings.add(data.typeName))
    output.putW8(0) // region id
    output.putULEB(signatures.add(data.signature))
    output.putW16(data.flags.mask)
    for (code <- data.code) {
      output.putW8(MethodTag.Code.tag)
      output.putULEB(methodCodes.add(code))
    }
    for (name <- data.sourceFullName) {
      output.putW8(MethodTag.SourceFullName.tag)
      output.putULEB(strings.add(name))
    }
    for (name <- data.sourceFile) {
      output.putW8(MethodTag.SourceFile.tag)
      output.putULEB(strings.add(name))
    }
    for (linkageName <- data.linkageName) {
      output.putW8(MethodTag.LinkageName.tag)
      output.putULEB(strings.add(linkageName))
    }
    if (data.genericParameters > 0) {
      output.putW8(MethodTag.GenericParameters.tag)
      output.putW8(data.genericParameters)
    }
    output.putW8(MethodTag.Nothing.tag)
  }
}

private class FieldPool extends Pool[Field] { self: RawPool with PoolProvider =>
  override def add(data: Field): Offset = put { output =>
    output.putW32(strings.add(data.name))
    output.putW8(0) // region id
    output.putULEB(signatures.add(data.fieldType)) // FIXME: ref type for field definition is not needed anywhere.
    output.putW8(data.flags.mask)

    for ((tag, value) <- data.constValue) {
      output.putW8(tag.tag)
      tag match {
        case SlebConst => output.putSLEB(value)
        case UlebConst => output.putULEB(value)
        case U64Const =>  output.putW64(value)
        case _ => assert(false, s"Unexpected field const value tag $tag")
      }
    }

    output.putW8(FieldTag.Nothing.tag)
  }
}

private class TypePool extends Pool[Type] { self: RawPool with PoolProvider =>
  override def add(data: Type): Offset = put { output =>
    output.putW32(strings.add(data.name))
    output.putW8(0) // region id
    output.putW16(data.flags.mask)
    output.putULEB(signatures.add(data.superOrEnumType.getOrElse(BuiltinSignature.Nil)))

    val (virtualMethods, stationaryMethods) = data.methods.partition(_.flags.contains(MethodFlag.VIRTUAL))

    val methodTable = new MemberTable[Method](methods)
    stationaryMethods.foreach(methodTable.add)
    virtualMethods.foreach(methodTable.add)
    output.putBytes(methodTable.asByteArray())

    writeSequence(output, virtualMethods.map(methods.add))

    val (staticFields, instanceFields) = data.fields.partition(_.flags.contains(FieldFlag.STATIC))

    // TODO: separate instance/static fields
    val fieldTable = new MemberTable[Field](fields)
    staticFields.foreach(fieldTable.add)
    output.putBytes(fieldTable.asByteArray())

    writeSequence(output, instanceFields.map(fields.add))

    if (data.interfaces.nonEmpty) {
      output.putW8(TypeTag.Interfaces.tag)
      writeSequence(output, data.interfaces.map(signatures.add))
    }
    if (data.genericConstraints.nonEmpty) {
      output.putW8(TypeTag.GenericParameters.tag)
      output.putULEB(data.genericConstraints.length)
    }

    data.enumKind match {
      case Option0 => output.putW8(TypeTag.Option0.tag)
      case Option1 => output.putW8(TypeTag.Option1.tag)
      case Primitive => output.putW8(TypeTag.Primitive.tag)
      case Union =>
        output.putW8(TypeTag.UnionFields.tag)
        writeSequence(output, data.unionFields.map(signatures.add))
      case NotEnum =>
    }

    output.putW8(TypeTag.Nothing.tag)
  }
}

private class AotDataPool extends Pool[IndexedAotData] { self: RawPool with PoolProvider =>
  override def add(data: IndexedAotData): Offset = put { output =>
    output.putW32(data.index)
    data.aotData match {
      case DirectCallAotData(linkageName) =>
        output.putW32(strings.add(linkageName))
      case VirtualCallAotData(vnum, extDefNum) =>
        output.putW16(vnum)
        output.putW16(extDefNum)
      case InterfaceCallAotData(inum) =>
        output.putW16(inum)
      case StaticFieldAotData(linkageName) =>
        output.putW32(strings.add(linkageName))
      case InstanceFieldAotData(ordinal) =>
        output.putW32(ordinal)
    }
  }
}

private object Utils {
  /**
    * Writes a sequence of integers as a sequence of ULEB encoded values.
    * To define a size of sequence, uses total size of sequence in bytes (not element count).
    */
  def writeSequence(out: DataOutput, data: Iterable[Int]): Unit = {
    val size = data.fold(0)((x, y) => LEB128Encoder.calcSizeULEB128(y) + x)
    out.putULEB(size)
    data.foreach(out.putULEB)
  }
}
