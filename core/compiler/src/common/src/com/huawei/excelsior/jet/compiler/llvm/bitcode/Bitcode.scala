/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.llvm.bitcode

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.Env.targetOS
import com.huawei.excelsior.jet.compiler.cangjie.CangjieSymLevelMaker
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Attributes.{Attribute, AttributesList, EnumAttribute}
import com.huawei.excelsior.jet.compiler.llvm.bitcode.DwTag.*
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Errors.{error, hopeThat, require}
import xscala.util.MathUtils.alignUp
import xscala.util.simpleClassName

import scala.PartialFunction.condOpt
import scala.annotation.nowarn
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object Bitcode {
  /*private[bitcode]*/ object BlockIds { // TODO-DECAF: make private[bitcode] after TestBitstream.java translation
    val TOP_LEVEL = Bitstream.BlockIds.TOP_LEVEL
    val MODULE_BLOCK = 8
    val PARAMATTR_BLOCK = 9
    val PARAMATTR_GROUP_BLOCK = 10
    val CONSTANTS_BLOCK = 11
    val FUNCTION_BLOCK = 12
    val VALUE_SYMTAB_BLOCK = 14
    val METADATA_BLOCK = 15
    val METADATA_ATTACHMENT = 16
    val TYPE_BLOCK = 17
    val STRTAB_BLOCK = 23
  }

  private object FuncCodes {
    val DECLAREBLOCKS = 1               // [n]

    val INST_BINOP = 2                  // [ty, opval, opval, opcode]
    val INST_CAST = 3                   // [opval, opty, destty, castopc]
    val INST_GEP_OLD = 4                // [n x operands]
    val INST_SELECT = 5                 // [ty, opval, opval, opval]
    val INST_EXTRACTELT = 6             // [opty, opval, opval]
    val INST_INSERTELT = 7              // [ty, opval, opval, opval]
    val INST_SHUFFLEVEC = 8             // [ty, opval, opval, opval]
    val INST_CMP = 9                    // [opty, opval, opval, pred]

    val INST_RET = 10                   // [opty, opval<both optional>]
    val INST_BR = 11                    // [bb#, bb#, cond] or [bb#]
    val INST_SWITCH = 12                // [opty, op0, op1, ...]
    val INST_INVOKE = 13                // [attr, fnty, op0, op1, ...]
    // 14 is unused.
    val INST_UNREACHABLE = 15

    val INST_PHI = 16                   // [ty, val0, bb0, ...]
    // 17 is unused.
    // 18 is unused.
    val INST_ALLOCA = 19                // [instty, opty, op, align]
    val INST_LOAD = 20                  // [opty, op, align, vol]
    // 21 is unused.
    // 22 is unused.
    val INST_VAARG = 23                 // [valistty, valist, instty]
    // This store code encodes the pointer type; rather than the value type
    // this is so information only available in the pointer type (e.g. address
    // spaces) is retained.
    val INST_STORE_OLD = 24             // [ptrty, ptr, val, align, vol]
    // 25 is unused.
    val INST_EXTRACTVAL = 26            // [n x operands]
    val INST_INSERTVAL = 27             // [n x operands]
    // fcmp/icmp returning Int1TY or vector of Int1Ty. Same as CMP; exists to
    // support legacy vicmp/vfcmp instructions.
    val INST_CMP2 = 28                  // [opty, opval, opval, pred]
    // new select on i1 or [N x i1]
    val INST_VSELECT = 29               // [ty, opval, opval, predty, pred]
    val INST_INBOUNDS_GEP_OLD = 30      // [n x operands]
    val INST_INDIRECTBR = 31            // [opty, op0, op1, ...]
    // 32 is unused.
    val DEBUG_LOC_AGAIN = 33

    val INST_CALL = 34                  // [attr, cc, fnty, fnid, args...]

    val DEBUG_LOC = 35                  // [Line, Col, ScopeVal, IAVal]
    val INST_FENCE = 36                 // [ordering, synchscope]
    val INST_CMPXCHG_OLD = 37           // [ptrty, ptr, cmp, new, align, vol, ordering; synchscope]
    val INST_ATOMICRMW = 38             // [ptrty, ptr, val, operation, align; vol, ordering; synchscope]
    val INST_RESUME = 39                // [opval]
    val INST_LANDINGPAD_OLD = 40        // [ty, val, val, num, id0, val0...]
    val INST_LOADATOMIC = 41            // [opty, op, align, vol, ordering; synchscope]
    val INST_STOREATOMIC_OLD = 42       // [ptrty, ptr, val, align, vol, ordering; synchscope]
    val INST_GEP = 43                   // [inbounds, n x operands]
    val INST_STORE = 44                 // [ptrty, ptr, valty, val, align, vol]
    val INST_STOREATOMIC = 45           // [ptrty, ptr, val, align, vol]
    val INST_CMPXCHG = 46               // [ptrty, ptr, valty, cmp, new, align, vol; ordering, synchscope]
    val INST_LANDINGPAD = 47            // [ty, val, num, id0, val0...]
    val INST_CLEANUPRET = 48            // [val] or [val, bb#]
    val INST_CATCHRET = 49              // [val, bb#]
    val INST_CATCHPAD = 50              // [bb# ,bb#, num, args...]
    val INST_CLEANUPPAD = 51            // [num, args...]
    val INST_CATCHSWITCH = 52           // [num, args...] or [num, args..., bb]
    // 53 is unused.
    // 54 is unused.
    val OPERAND_BUNDLE = 55             // [tag#, value...]
    val INST_UNOP = 56                  // [opcode, ty, opval]
  }

  sealed abstract class Type private[bitcode](val repr: String) {
    override def toString = repr

    def isPointer = this.isInstanceOf[PointerType]

    def isBoolean = isInteger && getIntegerBitsNum == 1

    def isInteger = this.isInstanceOf[IntegralType]

    def isFloatingPoint = this.isInstanceOf[FloatingPointType]

    def isStruct = this.isInstanceOf[StructType]

    def isArray = this.isInstanceOf[ArrayType]

    /** Is zero-sized type. */
    def isZST = (this eq Types.VOID) || (this eq Types.UNIT)

    def getIntegerBitsNum = this.asInstanceOf[IntegralType].bitsCount
  }

  object Types {
    val METADATA = new UniqueType("metadata")
    val LABEL = new UniqueType("label")

    val VOID = new UniqueType("void")
    val UNIT = new UniqueType("%Unit.Type")

    val REF = new UniqueType("%Ref.Type")

    // struct AS$_ {
    //   let base: RawArray<_>
    //   let start: Int64
    //   let size: Int64
    // }
    val ARRAY_SLICE = new StructType(
      CangjieSymLevelMaker.ARRAY_SLICE_NAME,
      Array[Type](REF, i(64), i(64))
    )

    val FLOAT = new FloatingPointType("float", 32)
    val DOUBLE = new FloatingPointType("double", 64)
    val HALF = new FloatingPointType("half", 16)

    def i(bits: Int) = new IntegralType(bits)

    def ptrTo(base: Type) = new PointerType(base)

    def array(elem: Type, length: Int) = new ArrayType(elem, length)
  }

  class UniqueType private[bitcode](repr: String) extends Type(repr)

  case class FunctionType(vararg: Boolean, retTy: Type, paramTys: Array[Type])
    extends Type(s"$retTy (*)(${paramTys.mkString(", ")}${if (vararg) ", ..." else ""})")

  case class StructType(name: String, elements: Array[Type]) extends Type(s"%$name")

  case class ArrayType(element: Type, length: Int) extends Type(s"[$length x ${element.repr}]")

  case class PointerType(pointee: Type) extends Type(s"${pointee.repr}*")

  case class TypeVariableType(name: String) extends Type(s"%$name")

  sealed abstract class NBitsScalarType(repr: String) extends Type(repr) {
    def bitsCount: Int
  }

  case class IntegralType(bitsCount: Int) extends NBitsScalarType(s"i$bitsCount")

  case class FloatingPointType(override val repr: String, bitsCount: Int) extends NBitsScalarType(repr)

  object SizeAndAlignment {
    private[Bitcode] val ADDR_SIZE = 8 // FIXME: use some options
  }

  sealed abstract class ModuleValue {
    def ty: Type
  }

  object ModuleValue {
    case class NoValue(ty: Type) extends ModuleValue

    case class NumberConstant(ty: Type, value: Long) extends ModuleValue

    case class Global(g: Bitcode.Global) extends ModuleValue {
      def ty = Types.ptrTo(g.ty)
    }

    case class Function(fn: Bitcode.Function) extends ModuleValue {
      def ty = Types.ptrTo(fn.ty)
    }

    case class GetElementPtr(ty: Type, baseTy: Type, base: Long, indices: Array[Long], inbounds: Boolean) extends ModuleValue
  }

  object Strtab {
    private class Scanner extends BitstreamConsumer {
      private[Strtab] val strtab = new Strtab
      private var ctx: Bitstream.Context = _

      override def setContext(ctx: Bitstream.Context): Unit = this.ctx = ctx
      override def blockInfoAllowed = false
      override def magic(magicValue: Int) = true
      override def enterBlock(id: Int) = ctx.blockId == BlockIds.TOP_LEVEL && id == BlockIds.STRTAB_BLOCK
      override def endBlock(id: Int): Unit = {}

      override def record(code: Int, opsCount: Int, hasBlob: Boolean): Unit = {
        if (code == 1) { // STRTAB_BLOB: [blob]
          require(opsCount == 0 && hasBlob, "STRTAB_BLOB record must have single blob operand")
          hopeThat(!strtab.initialized, "there should be the only STRTAB")
          strtab.init(ctx.getBlob)
        }
      }

      override def endOfStream(): Unit = {
        require(strtab.initialized, "STRTAB must present")
      }
    }

    def fromFile(fileName: String) = {
      val scanner = new Scanner
      Bitstream.parseWhole(fileName, scanner)
      scanner.strtab
    }
  }

  final class Strtab {
    private var data: Array[Byte] = _

    private[bitcode] def init(data: Array[Byte]): Unit = {
      assert(!initialized)
      this.data = data
    }

    def initialized = data != null

    def getName(nameStart: Int, nameLen: Int) = {
      assert(initialized)
      Bitstream.decodeString(data, nameStart, nameLen)
    }
  }

  sealed abstract class NamedEntry protected (strtab: Strtab, nameStart: Int, nameLen: Int) {
    def isAnonymous = nameLen == 0

    def name = if (isAnonymous) "<anonymous>" else strtab.getName(nameStart, nameLen)

    override def toString = simpleClassName(this) + " " + name
  }

  class Function(strtab: Strtab, nameStart: Long, nameLen: Long, val ty: FunctionType, val isProto: Boolean, val idx: Int)
    extends NamedEntry(strtab, asUnsignedInt(nameStart), asUnsignedInt(nameLen)) {

    var debugInfo: DISubprogram = _

    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if that eq this => true
      case that: Function => this.ty == that.ty && this.idx == that.idx
      case _ => false
    }

    override def hashCode: Int = (ty, idx).##
  }

  object Global {
    val EXTERNAL = -1
  }

  class Global(strtab: Strtab, nameStart: Long, nameLen: Long, val ty: Type, val idx: Int, val initVarIdx: Int, val attrs: AttributesList)
    extends NamedEntry(strtab, asUnsignedInt(nameStart), asUnsignedInt(nameLen)) {

    assert(initVarIdx >= 0 || initVarIdx == Global.EXTERNAL)
    var debugInfo: DIGlobalVariable = _

    def isExternal = initVarIdx == Global.EXTERNAL

    override def equals(that: Any): Boolean = that match {
      case that: AnyRef if that eq this => true
      case that: Global => this.ty == that.ty && this.idx == that.idx
      case _ => false
    }

    override def hashCode: Int = (ty, idx).##
  }

  object MDItem {
    val INVALID = new MDResolvedItem() {
      override def toString = "INVALID"
    }
  }

  sealed abstract class MDItem {
    def resolve(): MDResolvedItem
  }

  class MDFwdItem(metadataList: collection.Seq[MDResolvedItem], val id: Int) extends MDItem {
    assert(id >= 0)

    override def resolve() = {
      hopeThat(id < metadataList.size, "forward reference is not yet resolvable !%d", id)
      metadataList(id)
    }

    override def toString = s"!$id"
  }

  sealed abstract class MDResolvedItem extends MDItem {
    @deprecated // don't call me directly
    override def resolve(): MDResolvedItem = this
    protected def quoted(s: String) = if (s != null) s"\"$s\"" else "null"
  }

  class MDString(val value: String) extends MDResolvedItem {
    assert(value != null)
    override def toString = "!" + quoted(value)
  }

  object MDString {
    def unapply(x: MDString) = Some(x.value)
  }

  class MDValue(val ty: Type, val valueId: Long) extends MDResolvedItem {
    override def toString = s"$ty %$valueId"
  }

  class MDNode(val elts: Array[MDItem]) extends MDResolvedItem {
    override def toString = elts.mkString("!{", ", ", "}")
  }

  object MDNode {
    def unapplySeq(x: MDNode) = Some(x.elts.toIndexedSeq)
  }

  object MDNull extends MDResolvedItem {
    override def toString = "null"
  }

  class DIFile(val directory: String, val filename: String) extends MDResolvedItem {

    def fullPath: String = {
      if (filename != null && filename.nonEmpty) {
        if (directory != null && directory.nonEmpty) {
          directory + targetOS.fileSeparator + filename
        } else {
          filename
        }
      } else {
        null
      }
    }

    override def toString = s"!DIFile(directory: ${quoted(directory)}, filename: ${quoted(filename)})"
  }

  class DIBasicType(val name: String, val sizeInBits: Int, val encoding: Int) extends MDResolvedItem {
    override def toString = s"!DIBasicType(name: ${quoted(name)}, size: $sizeInBits, encoding: ${dw(encoding)})"

    // TODO: change to enum if needed
    private def dw(encoding: Int): String = encoding match {
      case 0x00 => "DW_ATE_VOID"
      case 0x01 => "DW_ATE_ADDRESS"
      case 0x02 => "DW_ATE_BOOLEAN"
      case 0x03 => "DW_ATE_COMPLEX_FLOAT"
      case 0x04 => "DW_ATE_FLOAT"
      case 0x05 => "DW_ATE_SIGNED"
      case 0x06 => "DW_ATE_SIGNED_CHAR"
      case 0x07 => "DW_ATE_UNSIGNED"
      case 0x08 => "DW_ATE_UNSIGNED_CHAR"
      case 0x09 => "DW_ATE_IMAGINARY_FLOAT"
      case 0x0a => "DW_ATE_PACKED_DECIMAL"
      case 0x0b => "DW_ATE_NUMERIC_STRING"
      case 0x0c => "DW_ATE_EDITED"
      case 0x0d => "DW_ATE_SIGNED_FIXED"
      case 0x0e => "DW_ATE_UNSIGNED_FIXED"
      case 0x0f => "DW_ATE_DECIMAL_FLOAT"
      case 0x10 => "DW_ATE_UTF"
      case 0x80 => "DW_ATE_LO_USER"
      case 0xff => "DW_ATE_HI_USER"
    }
  }

  class DIDerivedType(val name: String, val tag: DwTag, val scope: MDItem, val baseType: MDItem,
                      val size: Int, val offset: Int, val flags: DIFlags) extends MDResolvedItem {
    override def toString =
      s"!DIDerivedType(name: ${quoted(name)}, tag: $tag, baseType: $baseType, " +
        s"scope: $scope, size: $size, offset: $offset, flags: $flags)"
  }

  class DIEnumerator(val name: String, val value: Int) extends MDResolvedItem {
    override def toString = s"!DIEnumerator(name: ${quoted(name)}, value: $value)"
  }

  class DICompileUnit(val language: Int, // language encoding according to DWARF spec.
                      val file: MDItem, val producer: String) extends MDResolvedItem {
    override def toString = s"!DICompileUnit(lang: $language, file: $file, producer: $producer)"
  }

  class DICompositeType(val name: String, val tag: DwTag, val size: Int, val elements: MDNode,
                        val flags: DIFlags, val identifier: String, val baseType: MDItem) extends MDResolvedItem {
    override def toString =
      s"!DICompositeType(name: ${quoted(name)}, tag: $tag, size: $size, elements: $elements, " +
        s"flags: $flags, identifier: ${quoted(identifier)}, baseType: $baseType)"
  }

  class CodeLinkedDIEntity(val name: String, val linkageName: String, val file: MDItem, val line: Int, val `type`: MDItem) extends MDResolvedItem {
    override def toString = s"name: ${quoted(name)}, linkageName: ${quoted(linkageName)}, file: $file, line: $line, type: ${`type`}"
  }

  class DISubprogram(name: String, linkageName: String, file: MDItem, line: Int, `type`: MDItem) extends CodeLinkedDIEntity(name, linkageName, file, line, `type`) {
    override def toString = s"!DISubprogram(${super.toString})"
  }

  class DILexicalBlock(val scopeId: Long, val file: MDItem, var line: Int, var column: Int) extends MDResolvedItem {
    // Let the lex block know about one more line-number info that belongs to it
    def recognizeInst(line: Int, column: Int): Unit = {
      assert(line > 0)
      if (this.line == 0 || line < this.line) {
        this.line = line
        this.column = column
      }
    }

    override def toString = s"!DILexicalBlock(scopeId: $scopeId, file: $file, line: $line, column: $column)"
  }

  class DISubroutineType(val types: MDItem) extends MDResolvedItem {
    override def toString = s"!DISubroutineType(contains: $types)"
  }

  class DIGlobalVariable(name: String, linkageName: String, `type`: MDItem, file: MDItem, line: Int,
                         val isLocalToUnit: Boolean, val isDefinition: Boolean, val staticDataMemberDeclaration: MDItem)
    extends CodeLinkedDIEntity(name, linkageName, file, line, `type`) {

    override def toString = s"!DIGlobalVariable(${super.toString}, isLocal: $isLocalToUnit, isDefinition: $isDefinition)"
  }

  private val allowedLocalVarFlags = DIFlags(DIFlag.FlagPrivate, DIFlag.FlagProtected, DIFlag.FlagPublic, DIFlag.FlagArtificial, DIFlag.FlagObjectPointer)

  class DILocalVariable(val name: String, val tpe: MDItem, val flags: DIFlags,
                        val arg: Int, val file: MDItem, val line: Int, val scope: MDItem) extends MDResolvedItem {
    hopeThat(allowedLocalVarFlags.containsAll(flags), "unexpected Flags = 0x%x", flags.raw)
    override def toString = {
      val argPart = if (arg != 0) s", arg: $arg" else ""
      s"!DILocalVariable(name: ${quoted(name)}$argPart, type: $tpe, flags: $flags, file: $file, line: $line, scope: $scope)"
    }
  }

  class DIGlobalVariableExpression(val variable: MDItem, val expression: MDItem) extends MDResolvedItem {
    override def toString = s"!DIGlobalVariableExpression(variable: $variable, expression: $expression)"
  }

  object DIExpressionEmpty extends MDResolvedItem {
    override def toString = "!DIExpression()"
  }

  private def asBoolean(v: Long) = v != 0

  private[bitcode] def asUnsignedInt(v: Long) = {
    hopeThat(0 <= v && v <= Int.MaxValue, "integer overflow (%d)", v)
    v.toInt
  }

  private def decodeSigned(encoded: Long): Long = {
    val lowBit = (encoded & 1).toInt
    val abs = encoded >>> 1
    assert(abs >= 0)
    if (lowBit == 0) {
      // positive or zero
      abs
    } else {
      // negative
      if (abs != 0) {
        -abs
      } else {
        // Mimic LLVM behaviour: "There is no such thing as -0 with integers. "-0" really means MININT."
        Long.MinValue
      }
    }
  }

  private def adjustIntegralConstant(ty: Type, numericValue: Long): Long = {
    if (ty.isBoolean) {
      // i1 (1-bit signed integral) type has two possible values: 0 as false and -1 as true.
      // Working with -1 is very inconvenient, so we translate them into 0 and +1.
      //
      // Pros:
      // * ease of calling of AJ code (i.e. runtime) which expects and returns 0/1
      // * ease of calling of foreign C code which expects and returns 0/1
      // * ease of debugging due to familiar 0/1 constants
      // * better debuggers support which assume true == 1
      //
      // Cons:
      // * semantics of some instructions should be adjusted (e.g. true should be less or equal to false)
      if (numericValue == 0) {
        0
      } else {
        require(numericValue == -1, "i1 constants must have value 0 or -1 but got %d", numericValue)
        1
      }
    } else {
      numericValue
    }
  }

  private def verifySingleParamAttr(ty: Type, attrs: Iterable[Attribute]): Unit = {
    for (attr <- attrs) {
      // Note that this attribute is free because we already convert signed i1 to zero-extended 0/1.
      hopeThat((attr == EnumAttribute.Z_EXT) && ty.isBoolean, "only i1 zeroext attribute is supported")
    }
  }

  private def verifyParamAttrs(fnTy: FunctionType, attrs: AttributesList): Unit = {
    for ((idx, elementAttrs) <- attrs.allAttrs) {
      if (idx == AttributesList.FUNCTION_IDX) {
        hopeThat(elementAttrs.isEmpty, "unexpected function attributes %s", elementAttrs)
      } else if (idx == AttributesList.RET_VAL_IDX) {
        verifySingleParamAttr(fnTy.retTy, elementAttrs)
      } else {
        val paramIdx = idx - AttributesList.FIRST_PARAM_IDX
        hopeThat(0 <= paramIdx && paramIdx < fnTy.paramTys.length, "unexpected param index %d", paramIdx)
        verifySingleParamAttr(fnTy.paramTys(paramIdx), elementAttrs)
      }
    }
  }

  sealed class LLVMState private( // TODO-decaf: split into mutable and immutable entities
    private[Bitcode] var attributeLists: Seq[Attributes.AttributesList],
    private val types0: ArrayBuffer[Type],
    private val functions0: ArrayBuffer[Function],
    private val globals0: ArrayBuffer[Global],
    private val moduleValues0: ArrayBuffer[ModuleValue],
    private[Bitcode] var metadataList: MetadataScanner.MDList,
    private val namedMetadata: HashMap[String, Array[MDResolvedItem]],
    private val namedDebugInfo: HashMap[String, MDResolvedItem],
    private[Bitcode] var compileUnit: DICompileUnit,
    private[Bitcode] val readOnly: Boolean
  ) {

    def this() = {
      this(null, ArrayBuffer.empty, ArrayBuffer.empty, ArrayBuffer.empty, ArrayBuffer.empty,
        new MetadataScanner.MDList(null), HashMap.empty, HashMap.empty, null, false)
    }

    def this(src: LLVMState) = {
      this(src.attributeLists, src.types0, src.functions0,
        src.globals0, src.moduleValues0,
        src.metadataList, src.namedMetadata,
        src.namedDebugInfo,
        src.compileUnit,
        src.readOnly)
    }

    private[Bitcode] def asReadOnly: LLVMState = {
      if (readOnly) {
        this
      } else {
        new LLVMState(
          attributeLists,
          types0,
          functions0,
          globals0,
          moduleValues0,
          metadataList,
          namedMetadata,
          namedDebugInfo,
          compileUnit,
          true)
      }
    }

    final def attrs(attrIdxOrZero: Int) = {
      if (attrIdxOrZero == 0) {
        AttributesList.EMPTY
      } else {
        attributeLists(attrIdxOrZero - 1)
      }
    }

    final def addType(t: Type): Unit = { assert(!readOnly); types0 += t }
    final def types: collection.IndexedSeq[Type] = types0

    final def addFunction(f: Function): Unit = { assert(!readOnly); functions0 += f }
    final def functions: collection.IndexedSeq[Function] = functions0

    final def addGlobal(g: Global): Unit = { assert(!readOnly); globals0 += g }
    final def globals: collection.IndexedSeq[Global] = globals0

    final def addModuleValue(mv: ModuleValue): Unit = { assert(!readOnly); moduleValues0 += mv }
    final def moduleValues: collection.IndexedSeq[ModuleValue] = moduleValues0

    final def metadataItem(idx: Int): MDResolvedItem = metadataList.get(idx, allowFwd = false).asInstanceOf[MDResolvedItem]

    final def namedMetadata(name: String): Option[Array[MDResolvedItem]] = namedMetadata.get(name)
    final def putNamedMetadata(name: String, md: Array[MDResolvedItem]) = { assert(!readOnly); namedMetadata.put(name, md) }

    final def namedDebugInfo(name: String): Option[MDResolvedItem] = namedDebugInfo.get(name)
    final def putNamedDebugInfo(name: String, di: MDResolvedItem) = { assert(!readOnly); namedDebugInfo.put(name, di) }

    final def getCompileUnit = compileUnit

    final def setCompileUnit(compileUnit: DICompileUnit): Unit = {
      assert(!readOnly)
      this.compileUnit = compileUnit
    }

    private[Bitcode] def getMDOrNullResolved(idOrZero: Long): MDResolvedItem = {
      // return getMDOrNull(idOrZero).resolve(); // FIXME: which version should be here ?
      if (idOrZero == 0) {
        MDNull
      } else {
        val id = asUnsignedInt(idOrZero - 1)
        metadataItem(id)
      }
    }

    private[Bitcode] def getMDOrNull(idOrZero: Long): MDItem = {
      if (idOrZero == 0) {
        MDNull
      } else {
        val id = asUnsignedInt(idOrZero - 1)
        metadataList.get(id, allowFwd = true)
      }
    }

    private[Bitcode] def getMDResolved(id: Long): MDResolvedItem = {
      val idInt = asUnsignedInt(id)
      val md = metadataItem(idInt)
      hopeThat(md != null, "unknown metadata with index %d", idInt)
      md
    }

    private[Bitcode] def getMDString(id: Long): String = {
      val md = getMDOrNull(id)
      if (md == MDNull) return null
      require(md.isInstanceOf[MDString], "MDString must be resolved")
      md.asInstanceOf[MDString].value
    }

    def getConstValue(md: MDValue): Option[Long] = getConstValue(md.valueId.toInt)

    def getConstValue(idx: Int): Option[Long] = condOpt(moduleValues(idx)) {
      case ModuleValue.NumberConstant(_, constValue) => constValue
    }
  }

  sealed abstract class BlockScanner(protected val ll: LLVMState) { thisScanner =>
    protected var ctx: Bitstream.Context = _

    def this(parent: BlockScanner) = {
      this(parent.ll)
      ctx = parent.ctx ensuring (_ != null)
    }

    def scannerForBlock(id: Int): BlockScanner

    def applyResult(id: Int): Unit = {}

    def record(code: Int, opsCount: Int, hasBlob: Boolean): Unit

    def wrap(): BitstreamConsumer = new BitstreamConsumer() {
      private val primary = thisScanner
      private var current = primary
      private val stack = ArrayBuffer.empty[BlockScanner]

      override def setContext(ctx: Bitstream.Context): Unit = {
        assert(current eq primary)
        current.ctx = ctx
      }

      override def magic(magicValue: Int) = {
        // TODO: check magic
        true
      }

      override def blockInfoAllowed: Boolean = current.ctx.blockId == BlockIds.MODULE_BLOCK

      override def enterBlock(id: Int): Boolean = {
        val newScanner = current.scannerForBlock(id)
        if (newScanner == null) {
          false
        } else {
          stack += current
          current = newScanner
          true
        }
      }

      override def endBlock(id: Int): Unit = {
        current.applyResult(id)
        current = stack.remove(stack.size - 1)
      }

      override def record(code: Int, opsCount: Int, hasBlob: Boolean): Unit = {
        current.record(code, opsCount, hasBlob)
      }

      override def endOfStream(): Unit = {
        assert(stack.isEmpty)
        current.applyResult(BlockIds.TOP_LEVEL)
      }
    }
  }

  class TypesScanner(parent: BlockScanner) extends BlockScanner(parent) {
    hopeThat(ll.types.isEmpty, "more than one TYPE_BLOCK exists in MODULE_BLOCK")

    private var curStructName: String = _

    private def addType(t: Type): Unit = ll.addType(t)

    override def scannerForBlock(id: Int) = shouldNotReachHere()

    override def record(code: Int, opsCount: Int, hasBlob: Boolean): Unit = {
      require(!hasBlob, "TYPE_* record must have no blob operand")
      decodeType(code, opsCount)
    }

    private def typeOperand(operandIdx: Int): Type = ll.types(asUnsignedInt(ctx.operand(operandIdx)))

    private def decodeType(code: Int, opsCount: Int): Unit = code match {
      case 1 => // NUMENTRY: [numentries]

      case 16 => // METADATA
        require(opsCount == 0, "record must have no operands")
        addType(Types.METADATA)

      case 5 => // LABEL
        require(opsCount == 0, "record must have no operands")
        addType(Types.LABEL)

      case 2 => // VOID
        require(opsCount == 0, "record must have no operands")
        addType(Types.VOID)

      case 3 => // FLOAT
        require(opsCount == 0, "record must have no operands")
        addType(Types.FLOAT)

      case 4 => // DOUBLE
        require(opsCount == 0, "record must have no operands")
        addType(Types.DOUBLE)

      case 7 => // INTEGER: [width]
        require(opsCount == 1, "record must have single operand")
        addType(Types.i(asUnsignedInt(ctx.operand(0))))

      case 8 => // POINTER: [pointee type] or [pointee type, address space]
        require(1 <= opsCount && opsCount <= 2, "record must have 1 or 2 operands")
        hopeThat(opsCount < 2 || ctx.operand(1) == 0, "address space should be zero")
        addType(Types.ptrTo(typeOperand(0)))

      case 10 => // HALF
        require(opsCount == 0, "record must have no operands")
        addType(Types.HALF)

      case 11 => // ARRAY: [numelts, eltty]
        require(opsCount == 2, "record must have 2 operands")
        addType(Types.array(typeOperand(1), asUnsignedInt(ctx.operand(0))))

      case 18 =>
        // STRUCT_ANON: [ispacked, eltty x N]
        assert(curStructName == null)
        val elements = decodeStructElements(opsCount)
        val name = elements.map(_.repr).mkString("{ ", ", ", " }")
        addType(new StructType(name, elements))

      case 19 =>
        // STRUCT_NAME: [strchr x N]
        this.curStructName = ctx.operandsAsName

      case 20 =>
        // STRUCT_NAMED: [ispacked, eltty x N]
        require(curStructName != null, "STRUCT_NAMED must have STRUCT_NAME")
        val elements = decodeStructElements(opsCount)
        val structType = curStructName match {
          case "Ref.Type" | "Nullable.Type" | "Array.Type" | "JavaArray.Type" | "Meta.Type" =>
            hopeThat(elements.length == 0, s"%$curStructName should be an empty structure")
            Types.REF
          case "ArraySlice.Type" =>
            hopeThat(elements.length == 0, "%ArraySlice.Type should be an empty structure")
            Types.ARRAY_SLICE
          case "Unit.Type" =>
            hopeThat(elements.length == 0, "%Unit.Type should be an empty structure")
            Types.UNIT
          case _ =>
            if (curStructName.startsWith("typevar.")) {
              hopeThat(elements.length == 0, s"%$curStructName should be an empty structure")
              TypeVariableType(curStructName)
            } else {
              StructType(curStructName, elements)
            }
        }
        addType(structType)
        curStructName = null

      case 21 => // FUNCTION: [vararg, retty, paramty x N]
        require(opsCount >= 2, "record must have at least 2 operands")
        val vararg = asBoolean(ctx.operand(0))
        val retTy = typeOperand(1)
        val paramTys = Array.tabulate(opsCount - 2) { i => typeOperand(i + 2) }
        addType(FunctionType(vararg, retTy, paramTys))

      case 6  | // OPAQUE
           9  | // FUNCTION_OLD: [vararg, attrid, retty, paramty x N]
           12 | // VECTOR: [numelts, eltty]
           13 | // X86_FP80, X86 LONG DOUBLE
           14 | // FP128, LONG DOUBLE (112 bit mantissa)
           15 | // PPC_FP128, PPC LONG DOUBLE (2 doubles)
           17 | // X86_MMX, X86 MMX
           22 | // TOKEN
           _ =>
        error("not supported type record (code %d)", code)
    }

    private def decodeStructElements(opsCount: Int) = {
      require(opsCount >= 1, "record must have at least 1 operand")
      hopeThat(ctx.operand(0) == 0, "ispacked should be 0")
      Array.tabulate(opsCount - 1) { i => typeOperand(1 + i) }
    }
  }

  class ConstantsScanner(parent: BlockScanner) extends BlockScanner(parent) {
    protected val isGlobal = ctx.blockId == BlockIds.MODULE_BLOCK
    private var curConstantType: Type = _

    override def scannerForBlock(id: Int) = shouldNotReachHere()

    override def record(code: Int, opsCount: Int, hasBlob: Boolean): Unit = {
      require(!hasBlob, "CST_* record must have no blob operand")
      val c = decodeConstant(code, opsCount)
      if (c != null) processConstant(c)
    }

    protected def processConstant(c: ModuleValue): Unit = {
      assert(isGlobal)
      ll.addModuleValue(c)
    }

    private def typeOperand(operandIdx: Int): Type = ll.types(asUnsignedInt(ctx.operand(operandIdx)))

    private def decodeConstant(code: Int, opsCount: Int): ModuleValue = code match {
      case 1 => // SETTYPE: [typeid]
        require(opsCount == 1, "record must have single operand")
        curConstantType = typeOperand(0)
        null

      case 2 => // NULL: []
        require(opsCount == 0, "record must have no operands")
        if (curConstantType.isInteger) {
          ModuleValue.NumberConstant(curConstantType, adjustIntegralConstant(curConstantType, 0))
        } else if (curConstantType.isFloatingPoint || curConstantType.isPointer || curConstantType.isStruct ||
          curConstantType == Types.REF || curConstantType.isArray) {
          ModuleValue.NumberConstant(curConstantType, 0)
        } else if (curConstantType.isZST) {
          ModuleValue.NoValue(curConstantType)
        } else {
          error("unexpected type of null constant %s", curConstantType)
        }

      case 4 => // INTEGER: [intval]
        require(opsCount == 1, "record must have single operand")
        val v = adjustIntegralConstant(curConstantType, decodeSigned(ctx.operand(0)))
        hopeThat(curConstantType.isInteger, "integral constant should have integral type")
        ModuleValue.NumberConstant(curConstantType, v)

      case 6 => // FLOAT: [fpval]
        require(opsCount == 1, "record must have single operand")
        val bits = ctx.operand(0)
        hopeThat(curConstantType.isFloatingPoint, "fp constant should have fp type")
        ModuleValue.NumberConstant(curConstantType, bits)

      case 7 => // AGGREGATE: [n x value number]
        ModuleValue.NoValue(curConstantType)

      case 11 => // CE_CAST: [opcode, opty, opval]
        require(opsCount == 3, "record must have 3 operands")
        hopeThat(ctx.operand(0) == 11, "only bitcast constant cast is supported") // CAST_BITCAST
        val ty = typeOperand(1)
        val argIdx = asUnsignedInt(ctx.operand(2))
        val arg = ll.moduleValues(argIdx)
        hopeThat(ty == arg.ty, "ty consistency")
        (curConstantType, ty) match {
          case (_: PointerType, _: PointerType) =>
            // pointer to pointer is ok, no cast is required
            arg

          case (from: NBitsScalarType, to: NBitsScalarType) =>
            hopeThat(from.bitsCount == to.bitsCount, "mismatched constant bitcast bits count %d <- %d", to.bitsCount, from.bitsCount)
            val const = ll.getConstValue(argIdx)
            ModuleValue.NumberConstant(to, const.get)

          case (from, to) =>
            error("unexpected bitstream: unsupported constant bitcast %s <- %s", to, from)
            arg
        }

      case 20 => // CE_INBOUNDS_GEP: [n x operands]
        hopeThat(opsCount >= 3 && opsCount % 2 == 1, "should have at least 3 operands and any number of pairs")

        val ty = typeOperand(0)

        val baseTy = typeOperand(1)
        val base = ctx.operand(2)
        hopeThat(Types.ptrTo(ty) == baseTy, "ty and base types consistency")

        val firstIndexOffset = 3
        val indices = Array.tabulate((opsCount - firstIndexOffset) / 2) { i =>
          val k = firstIndexOffset + i * 2
          val indexTy = typeOperand(k)
          ctx.operand(k + 1)
        }

        val res = ModuleValue.GetElementPtr(curConstantType, ty, base, indices, inbounds = true)
        hopeThat(curConstantType == res.ty, "ty and constant type consistency")
        res

      case 3  | // UNDEF: []
           5  | // WIDE_INTEGER: [n x intval]
           8  | // STRING: [values]
           9  | // CSTRING: [values]
           10 | // CE_BINOP: [opcode, opval, opval]
           12 | // CE_GEP: [n x operands]
           13 | // CE_SELECT: [opval, opval, opval]
           14 | // CE_EXTRACTELT: [opty, opval, opval]
           15 | // CE_INSERTELT: [opval, opval, opval]
           16 | // CE_SHUFFLEVEC: [opval, opval, opval]
           17 | // CE_CMP: [opty, opval, opval, pred]
           18 | // INLINEASM_OLD: [sideeffect|alignstack, asmstr,conststr]
           19 | // CE_SHUFVEC_EX: [opty, opval, opval, opval]
           21 | // BLOCKADDRESS: [fnty, fnval, bb#]
           22 | // DATA: [n x elements]
           23 | // INLINEASM: [sideeffect|alignstack|asmdialect,asmstr,conststr]
           24 | // CE_GEP_WITH_INRANGE_INDEX: [opty, flags, n x operands]
           25 | // CE_UNOP: [opcode, opval]
           _ =>
        error("unexpected constant record (code %d)", code)
    }
  }

  object MetadataScanner {
    private[Bitcode] class MDList(private val prefix: MDList) {
      assert(prefix == null || prefix.prefix == null)

      private val startIdx = if (prefix == null) 0 else prefix.items.size
      private val items = ArrayBuffer.empty[MDResolvedItem]

      def globalPart: MDList = if (prefix == null) this else prefix
      def isEmpty = startIdx == 0 && items.isEmpty

      def add(md: MDResolvedItem): Unit = items += md

      def get(idx: Int, allowFwd: Boolean): MDItem = {
        if (idx < startIdx) {
          return prefix.get(idx, allowFwd = false)
        }

        val localIdx = idx - startIdx
        if (localIdx < items.size) {
          return items(localIdx)
        }

        if (allowFwd) new MDFwdItem(items, localIdx) else null
      }
    }
  }

  final class MetadataScanner(parent: BlockScanner) extends BlockScanner(parent) {
    private var metadataNodeName: String = _

    ll.metadataList = {
      val isGlobal = ctx.blockId == BlockIds.MODULE_BLOCK
      val prefix = if (!isGlobal) {
        ll.metadataList.globalPart
      } else {
        assert(ll.metadataList.isEmpty)
        null
      }
      new MetadataScanner.MDList(prefix)
    }

    private def typeOperand(operandIdx: Int): Type = ll.types(asUnsignedInt(ctx.operand(operandIdx)))

    override def scannerForBlock(id: Int) = shouldNotReachHere()

    override def record(code: Int, opsCount: Int, hasBlob: Boolean): Unit = {
      decodeMetadata(code, opsCount, hasBlob)
    }

    private def addMetadata(md: MDResolvedItem): Unit = {
      ll.metadataList.add(md)
    }

    private def addCompositeTypeMetadata(md: DICompositeType): Unit = {
      ll.metadataList.add(md)
      if (!ll.readOnly) {
        ll.putNamedDebugInfo(md.identifier, md) // do not need debug info for readonly copy of LLVMState
      }
    }

    private def unrotateSign(value: Int) = {
      if ((value & 1) == 1) {
        ~(value >> 1)
      } else {
        value >> 1
      }
    }

    private def decodeMetadata(code: Int, opsCount: Int, hasBlob: Boolean): Unit = code match {
      case 38 |  // METADATA_INDEX_OFFSET           [offset]
           39 => // METADATA_INDEX                  [bitpos]
        // just ignore them

      case 35 => // METADATA_STRINGS         [count, offset] blob([lengths][chars])
        require(opsCount == 2 && hasBlob, "record must have 2 operands with a blob")
        decodeMetadataStrings(asUnsignedInt(ctx.operand(0)), asUnsignedInt(ctx.operand(1)), ctx.getBlob)

      case 4 => // METADATA_NAME           STRING:        [values]
        require(!hasBlob, "record does not expect blob operand")
        decodeMetadataName()

      case 10 => // METADATA_NAMED_NODE    NAMED_NODE:    [n x mdnodes]
        require(!hasBlob, "record does not expect blob operand")
        decodeMetadataNamedNode()

      case 2 => // METADATA_VALUE          VALUE:         [type num, value num]
        require(opsCount == 2 && !hasBlob, "record must have only 2 operands without a blob")
        // We don't resolve value right now, because function's metadata could have forward reference to value from function's body.
        addMetadata(new MDValue(typeOperand(0), ctx.operand(1)))

      case 3 => // METADATA_NODE           NODE:          [n x md num]
        require(!hasBlob, "record does not expect blob operand")
        val elts = Array.tabulate[MDItem](opsCount) { i =>
          ll.getMDOrNull(ctx.operand(i))
        }
        addMetadata(new MDNode(elts))

      case 14 =>
        // METADATA_ENUMERATOR  [distinct, value, name]
        hopeThat(opsCount >= 3 && !hasBlob, "record must have 3 or more operands without a blob")

        val properties = ctx.operand(0)
        val name = ll.getMDString(ctx.operand(2))
        val isDistinct = (properties & (1 << 0)) != 0
        val isUnsigned = (properties & (1 << 1)) != 0
        val isBigInt = (properties & (1 << 2)) != 0
        hopeThat(!isDistinct && !isUnsigned, "other modes unsupported now") // feel free to support other formats

        val value = if (isBigInt) {
          hopeThat(opsCount == 4, "only trivial bigint cases are supported for METADATA_ENUMERATOR record")
          unrotateSign(asUnsignedInt(ctx.operand(3)))
        } else {
          unrotateSign(asUnsignedInt(ctx.operand(1)))
        }

        addMetadata(new DIEnumerator(name, value))

      case 15 =>
        // METADATA_BASIC_TYPE    [distinct, tag, name, size, align, enc]
        hopeThat(opsCount == 7 && !hasBlob, "record must have only 7 operands without a blob")

        val name = ll.getMDString(ctx.operand(2))
        val sizeInBits = asUnsignedInt(ctx.operand(3))
        val alignInBits = asUnsignedInt(ctx.operand(4))
        val encoding = asUnsignedInt(ctx.operand(5))

        if (encoding != 0) {
          addMetadata(new DIBasicType(name, sizeInBits, encoding))
        } else {
          addMetadata(MDItem.INVALID)
        }

      case 16 => // METADATA_FILE [distinct, filename, directory, checksumkind, checksum]
        hopeThat(opsCount == 5 && !hasBlob, "record must have only 5 operands without a blob")
        // operands[0] -- distinct, ignored
        val filename = ll.getMDString(ctx.operand(1))
        val directory = ll.getMDString(ctx.operand(2))
        addMetadata(new DIFile(directory, filename))

      case 17 => // METADATA_DERIVED_TYPE       [distinct, ...]
        hopeThat(opsCount >= 12 && !hasBlob, "record must have at least 12 operands without a blob")

        val tag = DwTag.byValue(asUnsignedInt(ctx.operand(1)))
        val name = ll.getMDString(ctx.operand(2))
        val file = ll.getMDOrNull(ctx.operand(3))
        val line = asUnsignedInt(ctx.operand(4))
        val scope = ll.getMDOrNull(ctx.operand(5))
        val baseType = ll.getMDOrNull(ctx.operand(6))
        val sizeInBits = asUnsignedInt(ctx.operand(7))
        val alignInBits = asUnsignedInt(ctx.operand(8))
        val offsetInBits = asUnsignedInt(ctx.operand(9))
        val flags = DIFlags(asUnsignedInt(ctx.operand(10)))

        addMetadata(new DIDerivedType(name, tag, scope, baseType, sizeInBits, offsetInBits, flags))

      case 18 => // METADATA_COMPOSITE_TYPE     [distinct, ...]
        hopeThat(opsCount >= 16 && !hasBlob, "record must have at least 16 operands without a blob")

        val tag = DwTag.byValue(asUnsignedInt(ctx.operand(1)))
        val name = ll.getMDString(ctx.operand(2))
        val file = ll.getMDOrNull(ctx.operand(3))
        val line = asUnsignedInt(ctx.operand(4))
        val scope = ll.getMDOrNull(ctx.operand(5))
        val baseType = ll.getMDOrNull(ctx.operand(6))
        val sizeInBits = asUnsignedInt(ctx.operand(7))
        val alignInBits = asUnsignedInt(ctx.operand(8))
        val offsetInBits = asUnsignedInt(ctx.operand(9))
        val flags = DIFlags(asUnsignedInt(ctx.operand(10)))
        val elements = ll.getMDOrNull(ctx.operand(11)).asInstanceOf[MDNode]
        val identifier = ll.getMDString(ctx.operand(15))

        // TODO-DWARF verify DW_TAG_array_type attributes

        hopeThat(tag == DW_TAG_array_type || tag == DW_TAG_structure_type || tag == DW_TAG_enumeration_type, "currently supports only DW_TAG_array_type, DW_TAG_structure_type or DW_TAG_enumeration_type")
        hopeThat(identifier != null || tag == DW_TAG_array_type, "identifier attribute is absent for DICompositeType %s", name)
        addCompositeTypeMetadata(new DICompositeType(name, tag, sizeInBits, elements, flags, identifier, baseType))

      case 19 => // METADATA_SUBROUTINE_TYPE    [distinct, flags, types, cc]
        require(opsCount >= 3 && !hasBlob, "record must have at least 3 operands without a blob")
        val types = ll.getMDOrNull(ctx.operand(2))
        addMetadata(new DISubroutineType(types))

      case 20 => // METADATA_COMPILE_UNIT       [distinct, ...]
        hopeThat(ll.getCompileUnit == null, "only 1 DICompileUnit per module is allowed")
        hopeThat(opsCount >= 14 && !hasBlob, "METADATA_COMPILE_UNIT record must have at least 14 operands without a blob")
        // operands[0] -- distinct, ignored
        val language = asUnsignedInt(ctx.operand(1))
        val file = ll.getMDOrNull(ctx.operand(2))
        val producer = ll.getMDString(ctx.operand(3))
        val compileUnit = new DICompileUnit(language, file, producer)
        ll.setCompileUnit(compileUnit)
        addMetadata(compileUnit)

      case 21 => // METADATA_SUBPROGRAM         [distinct, ...]
        require(opsCount >= 6 && !hasBlob, "record must have at least 6 operands without a blob")

        val name = ll.getMDString(ctx.operand(2))
        val linkageName = ll.getMDString(ctx.operand(3))
        val file = ll.getMDOrNull(ctx.operand(4))
        val line = asUnsignedInt(ctx.operand(5))
        val `type` = ll.getMDOrNull(ctx.operand(6))
        val sp = new DISubprogram(name, linkageName, file, line, `type`)
        addMetadata(sp)

      case 22 => // METADATA_LEXICAL_BLOCK      [distinct, scope, file, line, column]
        require(opsCount == 5 && !hasBlob, "record must have only 5 operands without a blob")

        val scopeId = ctx.operand(1)
        val scope = ll.getMDOrNull(scopeId)
        val file = ll.getMDOrNull(ctx.operand(2))
        val line = asUnsignedInt(ctx.operand(3))
        val column = asUnsignedInt(ctx.operand(4))
        val lb = new DILexicalBlock(scopeId, file, line, column)
        addMetadata(lb)

      case 27 => // METADATA_GLOBAL_VAR      [distinct, ...]
        require(opsCount >= 9 && !hasBlob, "record must have at least 9 operands without a blob")

        val version = asUnsignedInt(ctx.operand(0)) / 2
        hopeThat(version == 2, "unexpected GLOBAL_VAR version %d", version)

        val scope = ll.getMDOrNull(ctx.operand(1))
        val name = ll.getMDString(ctx.operand(2))
        val linkageName = ll.getMDString(ctx.operand(3))
        val file = ll.getMDOrNull(ctx.operand(4))
        val line = asUnsignedInt(ctx.operand(5))
        val tpe = ll.getMDOrNull(ctx.operand(6))
        val isLocalToUnit = asBoolean(ctx.operand(7))
        val isDefinition = asBoolean(ctx.operand(8))
        val staticDataMemberDeclaration = ll.getMDOrNull(ctx.operand(9))
        val templateParams = ll.getMDOrNull(ctx.operand(10))
        val alignInBits = asUnsignedInt(ctx.operand(11))

        addMetadata(new DIGlobalVariable(name, linkageName, tpe, file, line, isLocalToUnit, isDefinition, staticDataMemberDeclaration))

      case 28 => // METADATA_LOCAL_VAR       [distinct, ...]
        hopeThat((opsCount == 9 || opsCount == 10) && !hasBlob, "record must have 9 or 10 operands without a blob")
        hopeThat(ctx.operand(0) == 2, "!IsDistinct && HasAlignment && !HasTag but got %d", ctx.operand(0))
        hopeThat(ctx.operand(8) == 0, "AlignInBits = %d", ctx.operand(8))

        if (opsCount == 10) {
          hopeThat(ctx.operand(9) == 0, "Annotations = %d", ctx.operand(9))
        }

        val scope = ll.getMDOrNull(ctx.operand(1))
        val name = ll.getMDString(ctx.operand(2))
        val file = ll.getMDOrNull(ctx.operand(3))
        val line = asUnsignedInt(ctx.operand(4))
        val tpe = ll.getMDOrNull(ctx.operand(5))
        val arg = asUnsignedInt(ctx.operand(6))
        val flags = DIFlags(asUnsignedInt(ctx.operand(7)))
        addMetadata(new DILocalVariable(name, tpe, flags, arg, file, line, scope))

      case 29 => // METADATA_EXPRESSION      [distinct, n x element]
        addMetadata(DIExpressionEmpty)

      case 36 => // METADATA_GLOBAL_DECL_ATTACHMENT [valueid, n x [id, mdnode]]
        require(opsCount % 2 == 1 && !hasBlob, "record must have odd number of operands without a blob")
        val varId = ctx.operand(0)
        for (i <- 1 until opsCount by 2) {
          val kind = ctx.operand(i)
          val md = ll.getMDResolved(ctx.operand(i + 1))
          attachGlobalVariableMetadata(varId, md)
        }

      case 37 => // METADATA_GLOBAL_VAR_EXPR        [distinct, var, expression]
        require(opsCount == 3 && !hasBlob, "record must have only 3 operands without a blob")
        // operands[0] -- distinct, ignored
        val `var` = ll.getMDOrNull(ctx.operand(1))
        val expr = ll.getMDOrNull(ctx.operand(2))
        addMetadata(new DIGlobalVariableExpression(`var`, expr))

      // TODO-DWARF support it
      case 13 => // METADATA_SUBRANGE      [distinct, count, lo]
        addMetadata(MDItem.INVALID)

      case 7  |  // METADATA_LOCATION       [distinct, line, col, scope, inlined-at?]
           24 |  // METADATA_NAMESPACE [distinct, scope, file, name, line, exportSymbols]
           31 => // METADATA_IMPORTED_ENTITY [distinct, tag, scope, entity, line, name]
        addMetadata(MDItem.INVALID)

      case 1  | // METADATA_STRING_OLD     MDSTRING:      [values]
           5  | // METADATA_DISTINCT_NODE  DISTINCT_NODE: [n x md num]
           6  | // METADATA_KIND           [n x [id, name]]
           8  | // METADATA_OLD_NODE       OLD_NODE:      [n x (type num, value num)]
           9  | // METADATA_OLD_FN_NODE    OLD_FN_NODE:   [n x (type num, value num)]
           11 | // METADATA_ATTACHMENT    [m x [value, [n x [id, mdnode]]]
           12 | // METADATA_GENERIC_DEBUG [distinct, tag, vers, header, n x md num]
           23 | // METADATA_LEXICAL_BLOCK_FILE //[distinct, scope, file, discriminator]
           25 | // METADATA_TEMPLATE_TYPE   [distinct, scope, name, type, ...]
           26 | // METADATA_TEMPLATE_VALUE  [distinct, scope, name, type, value, ...]
           30 | // METADATA_OBJC_PROPERTY   [distinct, name, file, line, ...]
           32 | // METADATA_MODULE          [distinct, scope, name, ...]
           33 | // METADATA_MACRO           [distinct, macinfo, line, name, value]
           34 | // METADATA_MACRO_FILE      [distinct, macinfo, line, file, ...]
           40 | // METADATA_LABEL                  [distinct, scope, name, file, line]
           _ =>
        error("unexpected metadata record (code %d)", code)
    }

    private def decodeMetadataStrings(count: Int, offset: Int, blob: Array[Byte]): Unit = {
      require(blob.length >= offset, "invalid offset of strings in blob")
      val lengths = Bitstream.raw(blob, 0, offset)
      var curOffset = offset
      for (_ <- 0 until count) {
        val len = lengths.readVBR(6)
        require(curOffset + len <= blob.length, "invalid length of string in blob")

        val str = Bitstream.decodeString(blob, curOffset, len)
        addMetadata(new MDString(str))

        curOffset += len
      }
    }

    private def decodeMetadataName(): Unit = {
      require(metadataNodeName == null, "NAME must be followed by NAMED_NODE")
      metadataNodeName = ctx.operandsAsName
    }

    private def decodeMetadataNamedNode(): Unit = {
      require(metadataNodeName != null, "NAME must be followed by NAMED_NODE")
      val elts = Array.tabulate[MDResolvedItem](ctx.operandsCount) { i =>
        ll.getMDResolved(ctx.operand(i))
      }
      val previous = ll.putNamedMetadata(metadataNodeName, elts)
      require(previous.isEmpty, "duplicate named metadata")
      metadataNodeName = null
    }

    private def attachGlobalVariableMetadata(valueId: Long, md: MDResolvedItem): Unit = {
      ll.moduleValues(asUnsignedInt(valueId)) match {
        case ModuleValue.Global(global) =>
          hopeThat(global.debugInfo == null, "duplicate debug info")
          global.debugInfo = md match {
            case md: DIGlobalVariableExpression =>
              md.variable.resolve() match {
                case variable: DIGlobalVariable => variable
                case _ => error("unexpected global variable metadata format")
              }
            case _ => error("unexpected global variable metadata attachment")
          }
        case _ => error("invalid global variable value")
      }
    }
  }

  private sealed abstract class FileScanner(val strtab: Strtab) extends BlockScanner(new LLVMState) {
    val skipStrtab = strtab.initialized
    private val attributes = new Attributes.Scanner
    private var curFunctionIdx = -1
    var sourceFilename: String = _
    var globalState: LLVMState = _

    protected def enterFunctionBlock(fnIdx: Int): BlockScanner

    override def scannerForBlock(id: Int): BlockScanner = {
      import BlockIds.*

      (ctx.blockId, id) match {
        case (TOP_LEVEL, STRTAB_BLOCK) => if (skipStrtab) null else this
        case (TOP_LEVEL, MODULE_BLOCK) => this
        case (TOP_LEVEL, _) => null

        case (MODULE_BLOCK, PARAMATTR_BLOCK | PARAMATTR_GROUP_BLOCK) => this
        case (MODULE_BLOCK, TYPE_BLOCK)      => new TypesScanner(this)
        case (MODULE_BLOCK, CONSTANTS_BLOCK) => new ConstantsScanner(this)
        case (MODULE_BLOCK, METADATA_BLOCK)  => new MetadataScanner(this)

        case (MODULE_BLOCK, FUNCTION_BLOCK) =>
          if (globalState == null) { // first function block in bitstream
            globalState = ll.asReadOnly
          }
          curFunctionIdx += 1
          while (ll.functions(curFunctionIdx).isProto) {
            curFunctionIdx += 1 // skip
          }
          enterFunctionBlock(curFunctionIdx) ensuring (_ ne this)

        case (MODULE_BLOCK, _) => null
      }
    }

    override def applyResult(id: Int): Unit = id match {
      case BlockIds.MODULE_BLOCK | BlockIds.STRTAB_BLOCK | BlockIds.PARAMATTR_GROUP_BLOCK =>

      case BlockIds.PARAMATTR_BLOCK =>
        ll.attributeLists = attributes.getResult.toSeq

      case BlockIds.TOP_LEVEL =>
        require(strtab.initialized, "STRTAB must present")
        hopeThat(sourceFilename != null, "source filename should be specified")

      case _ => shouldNotReachHere()
    }

    override final def record(code: Int, opsCount: Int, hasBlob: Boolean): Unit = {
      try {
        ctx.blockId match {
          case BlockIds.MODULE_BLOCK =>
            if (code == 1) { // VERSION: [version#]
              require(opsCount == 1 && !hasBlob, "VERSION record must have single operand")
              hopeThat(ctx.operand(0) == 2, "version must be 2")

            } else if (code == 7) { // GLOBALVAR
              require(!hasBlob, "GLOBALVAR record must have no blob operand")
              decodeGlobalDef(opsCount)

            } else if (code == 8) { // FUNCTION
              require(!hasBlob, "FUNCTION record must have no blob operand")
              decodeFunctionDef(opsCount)

            } else if (code == 16) { // SOURCE_FILENAME
              require(!hasBlob, "SOURCE_FILENAME record must have no blob operand")
              hopeThat(sourceFilename == null, "only one declaration is allowed")
              sourceFilename = ctx.operandsAsName

            } // else ignore

          case BlockIds.PARAMATTR_GROUP_BLOCK =>
            require(!hasBlob, "PARAMATTR_* record must have no blob operand")
            attributes.decodeAttributeGroup(ctx)

          case BlockIds.PARAMATTR_BLOCK =>
            require(!hasBlob, "PARAMATTR_* record must have no blob operand")
            attributes.decodeAttributes(ctx)

          case BlockIds.STRTAB_BLOCK =>
            if (code == 1) { // STRTAB_BLOB: [blob]
              require(opsCount == 0 && hasBlob, "STRTAB_BLOB record must have single blob operand")
              hopeThat(!strtab.initialized, "there should be the only STRTAB")
              strtab.init(ctx.getBlob)
            }

          case _ => shouldNotReachHere()
        }
      } catch { case e: Errors.Error =>
        val operands = ctx.operandsAsArray.mkString("[", ", ", "]")
        throw new Errors.Error(s"record decoding failed: code $code $operands" + (if (!hasBlob) "" else " with blob"), e)
      }
    }

    protected def typeOperand(operandIdx: Int): Type = ll.types(asUnsignedInt(ctx.operand(operandIdx)))

    private def decodeGlobalDef(opsCount: Int): Unit = {
      require(opsCount >= 4, "GLOBALVAR record must have at least 4 operands")
      val nameStart = ctx.operand(0)
      val nameLen = ctx.operand(1)
      val ty = typeOperand(2)
      hopeThat(ctx.operand(3) == 2, "global with explicit type is expected, got %d", ctx.operand(3))
      val initVarIdxEncoded = ctx.operand(4)
      val initValueIdx = if (initVarIdxEncoded != 0) asUnsignedInt(initVarIdxEncoded - 1) else Global.EXTERNAL

      val attrsId = if (opsCount > 14) asUnsignedInt(ctx.operand(14)) else 0
      val attrs = ll.attrs(attrsId)

      val g = new Global(strtab, nameStart, nameLen, ty, ll.globals.size, initValueIdx, attrs)
      ll.addGlobal(g)
      ll.addModuleValue(ModuleValue.Global(g))
    }

    private def decodeFunctionDef(opsCount: Int): Unit = {
      // FUNCTION: [strtab offset, strtab size, type, .., isproto, ...]
      require(opsCount >= 7, "FUNCTION record must have at least 7 operands")
      val nameStart = ctx.operand(0)
      val nameLen = ctx.operand(1)

      val `type` = typeOperand(2)
      require(`type`.isInstanceOf[FunctionType], "non-function type for function")
      val fnTy = `type`.asInstanceOf[FunctionType]

      val isProto = asBoolean(ctx.operand(4))

      val attrsId = asUnsignedInt(ctx.operand(6))
      val attrs = ll.attrs(attrsId)
      verifyParamAttrs(fnTy, attrs)

      val f = new Function(strtab, nameStart, nameLen, fnTy, isProto, ll.functions.size)
      ll.addFunction(f)
      ll.addModuleValue(ModuleValue.Function(f))
    }
  }

  object ParsedModule {
    def fromFile[V](source: String, cb: InstructionConsumer[V], preloadStrtab: Boolean) = {
      val savedContexts = mutable.HashMap.empty[Int, Bitstream.Context]

      val strtab = if (preloadStrtab) Strtab.fromFile(source) else new Strtab
      val scanner = new FileScanner(strtab) {
        override protected def enterFunctionBlock(fnIdx: Int) = {
          savedContexts(fnIdx) = ctx.makeSingleBlockContext()
          if (cb == null) {
            // Note: we still need to process function block to parse metadata attachment, which contains debug info!
            new FunctionBlockScanner(this, fnIdx, attachMetadata = true) {
              override protected def processConstant(c: ModuleValue): Unit = {}
              override protected def decodeInstr(code: Int, opsCount: Int): Unit = {}
            }
          } else {
            new FunctionBodyScanner[V](this, fnIdx, cacheModuleValues = true, attachMetadata = true, cb)
          }
        }
      }

      Bitstream.parseWhole(source, scanner.wrap())

      new ParsedModule(scanner.globalState, savedContexts, scanner.sourceFilename)
    }
  }

  class ParsedModule private(ll: LLVMState, savedContexts: collection.Map[Int, Bitstream.Context], val sourceFilename: String) extends LLVMState(ll) {
    assert(ll.readOnly)

    def contextForFunction(fnIdx: Int) = {
      // TODO: Make Context reusable and eliminate redundant copying (fix JET-14206).
      Bitstream.Context.copy(savedContexts(fnIdx))
    }
  }

  def parseFunctionBody[V](source: String, module: ParsedModule, fnIdx: Int, cb: InstructionConsumer[V], cacheModuleValues: Boolean): Unit = {
    val scanner = new BlockScanner(module) {
      override def scannerForBlock(id: Int): BlockScanner = {
        if (ctx.blockId == BlockIds.MODULE_BLOCK && id == BlockIds.FUNCTION_BLOCK) {
          return new FunctionBodyScanner[V](this, fnIdx, cacheModuleValues, false, cb)
        }
        shouldNotReachHere()
      }

      override def record(code: Int, opsCount: Int, hasBlob: Boolean): Unit = shouldNotReachHere()
    }

    Bitstream.parseSingleBlock(source, module.contextForFunction(fnIdx), scanner.wrap())
  }

  case class TypedV[V](ty: Type, v: V)

  object InstructionConsumer {
    val NO_HANDLER = -1
  }

  trait InstructionConsumer[V] {
    def emptyValuesArray(length: Int): Array[V]

    def startFunction(fn: Function): Unit
    def endFunction(): Unit

    def lexicalBlock(id: Long, lb: DILexicalBlock, lineNumber: Int, columnNumber: Int): Unit
    def instructionLocation(instrNumber: Int, file: DIFile, lineNumber: Int, columnNumber: Int, scopeId: Long): Unit

    def startInstruction(instrNumber: Int): Unit
    def endInstruction(): Unit

    def startXBlock(instrNumber: Int): Unit

    def noValue(): V

    def cstIntegral(ty: Type, numericValue: Long): V
    def cstFloatingPoint(ty: Type, bits: Long): V
    def cstNullPointer(ty: Type): V

    def metadata(md: MDItem): V
    def getMDValue(value: V): MDValue

    def param(ty: Type, idx: Int): V

    def global(g: Global): V

    def function(fn: Function): V

    def ret(): Unit
    def ret(ty: Type, value: V): Unit

    def unreachable(): Unit

    def alloca(allocTy: Type, count: V): V

    def extractValue(baseTy: Type, baseVal: V, indices: Array[Int]): TypedV[V]
    def getElementPtr(baseTy: Type, basePtr: V, indices: Array[V], inbounds: Boolean): TypedV[V]

    def store(ty: Type, mem: V, value: V): Unit
    def load(ty: Type, mem: V): V

    def cast(op: Int, toTy: Type, fromTy: Type, value: V): V

    def unOp(ty: Type, op: Int, value: V): V
    def binOp(ty: Type, op: Int, l: V, r: V): V

    def br(bb: Int): Unit

    def br(cond: V, trueBB: Int, falseBB: Int): Unit

    def phi(values: Array[V], predBBs: Array[Int]): V

    def cmp(ty: Type, op: Int, l: V, r: V): V

    def call(fnTy: FunctionType, target: V, args: Array[V], argTys: Array[Type], handlerBB: Int): V
  }

  abstract class FunctionBlockScanner(
      parent: BlockScanner,
      functionIdx: Int,
      private val attachMetadata: Boolean
    ) extends BlockScanner(parent) { self =>

    protected val function = ll.functions(functionIdx)

    protected def processConstant(c: ModuleValue): Unit

    protected def decodeInstr(code: Int, opsCount: Int): Unit

    override def scannerForBlock(id: Int): BlockScanner = {
      assert(ctx.blockId == BlockIds.FUNCTION_BLOCK)

      id match {
        case BlockIds.METADATA_ATTACHMENT =>
          if (attachMetadata) this else null

        case BlockIds.CONSTANTS_BLOCK =>
          new ConstantsScanner(this) {
            override protected def processConstant(c: ModuleValue): Unit = {
              assert(!isGlobal)
              self.processConstant(c)
            }
          }

        case BlockIds.METADATA_BLOCK =>
          new MetadataScanner(this)

        case _ => null
      }
    }

    override def record(code: Int, opsCount: Int, hasBlob: Boolean): Unit = ctx.blockId match {
      case BlockIds.METADATA_ATTACHMENT =>
        decodeMetadataAttachment(code, opsCount, hasBlob)

      case BlockIds.FUNCTION_BLOCK =>
        require(!hasBlob, "FUNCTION_BLOCK's record must have no blob operand")
        decodeInstr(code, opsCount)
    }

    private def decodeMetadataAttachment(code: Int, opsCount: Int, hasBlob: Boolean): Unit = {
      // METADATA_ATTACHMENT    [m x [value, [n x [id, mdnode]]]
      hopeThat(code == 11, "only ATTACHMENT is supported in METADATA ATTACHMENT")
      hopeThat(opsCount == 2 && !hasBlob, "only single function attachment is supported")
      hopeThat(ctx.operand(0) == 0, "only zero kind is supported")

      val md = ll.getMDResolved(ctx.operand(1))
      hopeThat(md.isInstanceOf[DISubprogram], "only DISubprogram attachment is supported")
      attachFunctionMetadata(md.asInstanceOf[DISubprogram])
    }

    private def attachFunctionMetadata(sp: DISubprogram): Unit = {
      hopeThat(function.debugInfo == null, "duplicate subprogram")
      function.debugInfo = sp
    }
  }

  class FunctionBodyScanner[V](
      parent: BlockScanner,
      functionIdx: Int,
      private val cacheModuleValues: Boolean,
      private val attachMetadata: Boolean,
      private val cb: InstructionConsumer[V]
    ) extends FunctionBlockScanner(parent, functionIdx, attachMetadata) {

    assert(cb != null)
    assert(!function.isProto)
    cb.startFunction(function)

    private val values = ArrayBuffer.fill[TypedV[V]](ll.moduleValues.size)(null)

    for ((paramTy, i) <- function.ty.paramTys.zipWithIndex) {
      addValue(paramTy, cb.param(paramTy, i))
    }

    override def applyResult(id: Int): Unit = id match {
      case BlockIds.METADATA_ATTACHMENT =>

      case BlockIds.FUNCTION_BLOCK =>
        cb.endFunction()
        ll.metadataList = ll.metadataList.globalPart
    }

    override protected def processConstant(c: ModuleValue): Unit = {
      addValue(processModuleValue(c))
    }

    private def typeOperand(operandIdx: Int): Type = ll.types(asUnsignedInt(ctx.operand(operandIdx)))

    private def addValue(v: TypedV[V]): Unit = {
      assert(v.ty != Types.VOID)
      values += v
    }

    private def addValue(ty: Type, v: V): Unit = {
      addValue(TypedV(ty, v))
    }

    private var instrCounter = 0
    private var lastLineNumber = -1
    private var lastColumnNumber = -1
    private var lastScopeId = 0L
    private var lastFile: DIFile = _

    override protected def decodeInstr(code: Int, opsCount: Int): Unit = {
      import FuncCodes.*

      code match {
        case DECLAREBLOCKS =>
          require(opsCount == 1, "record must have single operand")
          instrCounter = 0

        case DEBUG_LOC =>
          hopeThat(opsCount == 5, "record must have 5 operands")
          hopeThat(instrCounter > 0, "DEBUG_LOC should not be the first")
          lastLineNumber = asUnsignedInt(ctx.operand(0))
          lastColumnNumber = asUnsignedInt(ctx.operand(1))
          val scopeId = ctx.operand(2)
          val scope = ll.getMDOrNullResolved(scopeId)
          hopeThat(ctx.operand(3) == 0 && ctx.operand(4) == 0, "IAID = 0 and !isImplicitCode")

          val fileItem: MDItem = scope match {
            case scope: DISubprogram =>
              lastScopeId = 0
              scope.file

            case lb: DILexicalBlock =>
              lastScopeId = scopeId
              if (lastLineNumber > 0) {
                cb.lexicalBlock(scopeId, lb, lastLineNumber, lastColumnNumber)
                // TODO-DWARF are we interested in LexBlocks without normal line numbers?
              }
              lb.file

            case _ => shouldNotReachHere("unexpected scope for debug location: " + scope)
          }
          lastFile = fileItem.resolve().asInstanceOf[DIFile]

          cb.instructionLocation(instrCounter - 1, lastFile, lastLineNumber, lastColumnNumber, lastScopeId)

        case DEBUG_LOC_AGAIN =>
          hopeThat(opsCount == 0, "record must have no operands")
          hopeThat(lastLineNumber != -1 && lastColumnNumber != -1, "DEBUG_LOC_AGAIN should follow DEBUG_LOC")
          cb.instructionLocation(instrCounter - 1, lastFile, lastLineNumber, lastColumnNumber, lastScopeId)

        case INST_LANDINGPAD =>
          require(opsCount >= 1, "should have at least one operand")
          val ty = typeOperand(0)
          addValue(ty, cb.noValue())
          cb.startXBlock(instrCounter) // `startInstruction` inside
          cb.endInstruction()
          instrCounter += 1

        case _ =>
          cb.startInstruction(instrCounter)
          decodeInstr2(code, opsCount)
          cb.endInstruction()
          instrCounter += 1
      }
    }

    private def decodeInstr2(code: Int, opsCount: Int): Unit = {
      import FuncCodes.*

      code match {
        case INST_RET =>
          if (opsCount == 0) {
            require(function.ty.retTy == Types.VOID, "RET without operands in non-void function")
            cb.ret()
          } else {
            require(opsCount == 1, "record must have 0 or 1 operand")
            cb.ret(function.ty.retTy, getRelValue(ctx.operand(0)).v)
          }

        case INST_BR =>
          if (opsCount == 1) {
            val target = asUnsignedInt(ctx.operand(0))
            cb.br(target)
          } else {
            require(opsCount == 3, "record must have 1 or 3 operand")
            val cond = getRelValue(ctx.operand(2)).v
            val trueBB = asUnsignedInt(ctx.operand(0))
            val falseBB = asUnsignedInt(ctx.operand(1))
            cb.br(cond, trueBB, falseBB)
          }

        case INST_UNREACHABLE =>
          require(opsCount == 0, "record must have no operands")
          cb.unreachable()

        case INST_PHI =>
          require(opsCount % 2 == 1, "record must have at least 1 operand and any number of pairs")
          val ty = typeOperand(0)

          val offset = 1
          val count = (opsCount - offset) / 2
          val vs = cb.emptyValuesArray(count)
          val bbs = new Array[Int](count)
          for (i <- 0 until count) {
            val v = getRelValue(decodeSigned(ctx.operand(offset + 2 * i)))
            require(v.ty == ty, "phi type consistency")
            vs(i) = v.v
            bbs(i) = asUnsignedInt(ctx.operand(offset + 2 * i + 1))
          }
          hopeThat(count == bbs.toSet.size, "no duplicate predecessors")
          addValue(ty, cb.phi(vs, bbs))

        case INST_ALLOCA => // ALLOCA: [instty, opty, op, align]
          require(opsCount == 4, "record must have 4 operands")

          val instTy = typeOperand(0)

          val inAllocaMask = 1 << 5
          val explicitTypeMask = 1 << 6
          val swiftErrorMask = 1 << 7
          val flagMask = inAllocaMask | explicitTypeMask | swiftErrorMask

          val alignOp = ctx.operand(3)

          val inAlloca = asBoolean(alignOp & inAllocaMask)
          val swiftError = asBoolean(alignOp & swiftErrorMask)
          val explicitType = asBoolean(alignOp & explicitTypeMask)

          hopeThat(!swiftError, "Swift error")
          hopeThat(!inAlloca, "inalloca")

          val alignExp = alignOp & ~flagMask
          require(alignExp < 32, "spoiled alignment operand") // cause only 4 bits are available

          hopeThat(typeOperand(1) == Types.i(32), "count should be an 32-bit integral")
          val count = getValue(ctx.operand(2)).v

          hopeThat(explicitType, "explicit type")
          val resTy = Types.ptrTo(instTy)
          val allocTy = instTy

          // Note: ignore alignment since llvm 15.
          addValue(resTy, cb.alloca(allocTy, count))

        case INST_EXTRACTVAL =>
          hopeThat(opsCount >= 2, "should have at least 3 operands")
          // always inbounds

          val base = getRelValue(ctx.operand(0))
          val ty = base.ty
          val indices = Array.tabulate(opsCount - 1) { i => asUnsignedInt(ctx.operand(i + 1)) }

          val res = cb.extractValue(ty, base.v, indices)
          addValue(res.ty, res.v)

        case INST_GEP =>
          hopeThat(opsCount >= 3, "should have at least 3 operands")

          val inbounds = asBoolean(ctx.operand(0))

          val ty = typeOperand(1)

          val base = getRelValue(ctx.operand(2))
          hopeThat(Types.ptrTo(ty) == base.ty, "ty and base types consistency")

          val firstIndexOffset = 3
          val indices = cb.emptyValuesArray(opsCount - firstIndexOffset)
          for (i <- firstIndexOffset until opsCount) {
            val index = getRelValue(ctx.operand(i))
            indices(i - firstIndexOffset) = index.v
          }

          val res = cb.getElementPtr(ty, base.v, indices, inbounds)
          addValue(res.ty, res.v)

        case INST_STORE =>
          hopeThat(opsCount == 4, "should have 4 operands (values are backward references)")
          val ptr = getRelValue(ctx.operand(0))
          val value = getRelValue(ctx.operand(1))
          require(ptr.ty.isPointer, "ptr type is not a pointer")
          val ptrTy = ptr.ty.asInstanceOf[PointerType]
          require(value.ty == ptrTy.pointee || (value.ty.isPointer && ptrTy.pointee.isPointer), "invalid ptr and val types")
          // Note: ignore alignment since llvm 15.
          hopeThat(ctx.operand(3) == 0, "vol")
          cb.store(value.ty, ptr.v, value.v)

        case INST_LOAD =>
          hopeThat(opsCount == 4, "should have 4 operands (values are backward references)")
          val ptr = getRelValue(ctx.operand(0))
          val resTy = typeOperand(1)
          require(ptr.ty.isPointer, "ptr type is not a pointer")
          val ptrTy = ptr.ty.asInstanceOf[PointerType]
          require(resTy == ptrTy.pointee || (resTy.isPointer && ptrTy.pointee.isPointer), "invalid ptr and res types")
          // Note: ignore alignment since llvm 15.
          hopeThat(ctx.operand(3) == 0, "vol")
          addValue(resTy, cb.load(resTy, ptr.v))

        case INST_CAST =>
          hopeThat(opsCount == 3, "should have 3 operands (values are backward references)")
          val ptr = getRelValue(ctx.operand(0))
          val resTy = typeOperand(1)
          val op = asUnsignedInt(ctx.operand(2))
          addValue(resTy, cb.cast(op, resTy, ptr.ty, ptr.v))

        case INST_UNOP =>
          hopeThat(2 <= opsCount && opsCount <= 3, "should have 2 or 3 operands")
          val opval = getRelValue(ctx.operand(0))
          val op = asUnsignedInt(ctx.operand(1))
          // ignore operands[2] if any, it contains flags // FIXME: should we fix it?
          addValue(opval.ty, cb.unOp(opval.ty, op, opval.v))

        case INST_BINOP =>
          hopeThat(3 <= opsCount && opsCount <= 4, "should have 3 or 4 operands (values are backward references)")
          val fst = getRelValue(ctx.operand(0))
          val snd = getRelValue(ctx.operand(1))
          require(fst.ty == snd.ty, "invalid arguments type")

          val op = asUnsignedInt(ctx.operand(2))
          // ignore operands[3] if any, it contains flags // FIXME: should we fix it?

          addValue(fst.ty, cb.binOp(fst.ty, op, fst.v, snd.v))

        case INST_CMP |
             INST_CMP2 =>
          hopeThat(opsCount == 3, "should have 3 operands (values are backward references)")
          val fst = getRelValue(ctx.operand(0))
          val snd = getRelValue(ctx.operand(1))
          require(fst.ty == snd.ty, "invalid arguments type")

          val op = asUnsignedInt(ctx.operand(2))
          addValue(Types.i(1), cb.cmp(fst.ty, op, fst.v, snd.v))

        case INST_VSELECT =>
          hopeThat(opsCount == 3, "should have 3 operands (values are backward references)")
          val trueVal = getRelValue(ctx.operand(0))
          val falseVal = getRelValue(ctx.operand(1))
          val condVal = getRelValue(ctx.operand(2))
          val resTy = trueVal.ty
          hopeThat(resTy == falseVal.ty, "argument type consistency")
          hopeThat(condVal.ty == Types.i(1), "only boolean condition")
          notImplemented("select requires CondMov")
        // addValue(resTy, cb.select(condVal.v, trueVal.v, falseVal.v));

        case INST_CALL =>
          val baseOperands = 4
          hopeThat(opsCount >= baseOperands, "should have at least 4 operands")
          val attrsId = asUnsignedInt(ctx.operand(0))
          val ccInfo = ctx.operand(1)
          hopeThat(ccInfo == (1 << 15), "only explicit type flag")

          decodeCall(2, attrsId, InstructionConsumer.NO_HANDLER)

        case INST_INVOKE =>
          val baseOperands = 6
          hopeThat(opsCount >= baseOperands, "should have at least 6 operands")
          val attrsId = asUnsignedInt(ctx.operand(0))
          val ccInfo = ctx.operand(1)
          hopeThat(ccInfo == (1 << 13), "only explicit type flag")

          val normalBB = asUnsignedInt(ctx.operand(2))
          val unwindBB = asUnsignedInt(ctx.operand(3))
          decodeCall(4, attrsId, unwindBB)
          cb.br(normalBB)

        case _ => error("unsupported instruction record (code %d)", code)
      }
    }

    private def decodeCall(startIdx: Int, attrsId: Int, handlerBB: Int): Unit = {
      val fnTyGeneric = typeOperand(startIdx)
      require(fnTyGeneric.isInstanceOf[FunctionType], "call expects function type")
      val fnTy = fnTyGeneric.asInstanceOf[FunctionType]

      verifyParamAttrs(fnTy, ll.attrs(attrsId))

      val callee = getRelValue(ctx.operand(startIdx + 1))

      val argsCount = ctx.operandsCount - (startIdx + 2)
      hopeThat((!fnTy.vararg && argsCount == fnTy.paramTys.length) ||
        (fnTy.vararg && argsCount >= fnTy.paramTys.length),
        "should have corresponding number of arguments")

      val args = cb.emptyValuesArray(argsCount)
      val argTys = new Array[Type](argsCount)
      for (i <- args.indices) {
        val paramTy = if (i < fnTy.paramTys.length) fnTy.paramTys(i) else null
        val arg = getRelValueOrMetadata(paramTy, ctx.operand(startIdx + 2 + i))

        val mdValueArg = cb.getMDValue(arg.v)
        val tyv = if (mdValueArg != null) getValue(mdValueArg.valueId) else arg
        args(i) = tyv.v
        argTys(i) = tyv.ty
      }

      val retTy = fnTy.retTy
      val retValue = cb.call(fnTy, callee.v, args, argTys, handlerBB)
      if (retTy != Types.VOID) {
        addValue(retTy, retValue)
      }
    }

    private def getRelValue(relId: Long): TypedV[V] = getRelValueOrMetadata(null, relId)

    private def getRelValueOrMetadata(ty: Type, relId: Long): TypedV[V] = {
      val id = values.size - relId

      if (Types.METADATA == ty) {
        val HIGH_BITS = 0xFFFF_FFFF_0000_0000L
        val md = if ((id & HIGH_BITS) == HIGH_BITS) {
          // Sometimes (I really don't know when, maybe local metadata?) metadata ids have all high bits set.
          // Drop them for now. Could somebody discover what does it mean?
          val maskedId = id & ~HIGH_BITS
          ll.getMDResolved(maskedId)
        } else {
          ll.getMDResolved(id)
        }
        TypedV(ty, cb.metadata(md))

      } else {
        hopeThat(relId >= 1, "relative value should not reference self")
        require(id >= 0, "relative value reference underflow")

        val tv = getValue(id)
        hopeThat(ty == null || ty == tv.ty || (ty.isPointer && tv.ty.isPointer), "type inconsistency, expected %s but got %s", ty, tv.ty)
        tv
      }
    }

    private def getValue(valueIdLong: Long): TypedV[V] = {
      hopeThat(0 <= valueIdLong && valueIdLong < values.size, "value reference should be a backward reference")
      val valueId = valueIdLong.toInt
      val v = values(valueId)
      if (v != null) {
        v
      } else { // Otherwise - uninitialized global value
        val gv = processModuleValue(ll.moduleValues(valueId))
        if (cacheModuleValues) {
          values(valueId) = gv
        }
        gv
      }
    }

    private def processModuleValue(v: ModuleValue): TypedV[V] = v match {
      case ModuleValue.NoValue(ty) =>
        TypedV(ty, cb.noValue())

      case ModuleValue.NumberConstant(ty, value) =>
        val res = if (ty.isInteger) {
          cb.cstIntegral(ty, value)
        } else if (ty.isFloatingPoint) {
          cb.cstFloatingPoint(ty, value)
        } else {
          assert(value == 0)
          if (ty.isPointer) {
            cb.cstNullPointer(ty)
          } else {
            cb.noValue()
          }
        }
        TypedV(ty, res)

      case ModuleValue.Global(g) =>
        TypedV(v.ty, cb.global(g))

      case ModuleValue.Function(fn) =>
        TypedV(v.ty, cb.function(fn))

      case gep: ModuleValue.GetElementPtr =>
        val base = getValue(gep.base)
        hopeThat(Types.ptrTo(gep.baseTy) == base.ty, "ty and base types consistency")

        val indices = cb.emptyValuesArray(gep.indices.length)
        for (i <- gep.indices.indices) {
          val index = getValue(gep.indices(i))
          indices(i) = index.v
        }

        val res = cb.getElementPtr(gep.baseTy, base.v, indices, inbounds = true)
        hopeThat(gep.ty == res.ty, "ty and constant type consistency")
        res
    }
  }
}