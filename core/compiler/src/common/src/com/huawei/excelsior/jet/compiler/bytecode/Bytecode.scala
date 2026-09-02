/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.*
import com.huawei.excelsior.jet.compiler.bytecode.CompareOp.*
import com.huawei.excelsior.jet.compiler.bytecode.OpKind.*
import xscala.util.StringOps.*

/** Bytecode instructions: format, properties, parsing from bytecode binary stream
  *
  * @author paul
  */
enum Bytecode private (_fmt: String, var _kind: OpKind) {
  case NOP extends Bytecode(">")

  case ACONST_NULL extends Bytecode(">R", CONST, 0)
  case ICONST_M1 extends Bytecode(">I", CONST, -1)
  case ICONST_0 extends Bytecode(">I", CONST, 0)
  case ICONST_1 extends Bytecode(">I", CONST, 1)
  case ICONST_2 extends Bytecode(">I", CONST, 2)
  case ICONST_3 extends Bytecode(">I", CONST, 3)
  case ICONST_4 extends Bytecode(">I", CONST, 4)
  case ICONST_5 extends Bytecode(">I", CONST, 5)
  case LCONST_0 extends Bytecode(">L", CONST, 0)
  case LCONST_1 extends Bytecode(">L", CONST, 1)
  case FCONST_0 extends Bytecode(">F", CONST, 0)
  case FCONST_1 extends Bytecode(">F", CONST, 1)
  case FCONST_2 extends Bytecode(">F", CONST, 2)
  case DCONST_0 extends Bytecode(">D", CONST, 0)
  case DCONST_1 extends Bytecode(">D", CONST, 1)

  case BIPUSH extends Bytecode("i:>B", CONST)
  case SIPUSH extends Bytecode("wi:>S", CONST)
  case LDC extends Bytecode("c:>X")
  case LDC_W extends Bytecode("wc:>X")
  case LDC2_W extends Bytecode("wc:>X")

  case ILOAD extends Bytecode("l:>I", LOAD)
  case LLOAD extends Bytecode("l:>L", LOAD)
  case FLOAD extends Bytecode("l:>F", LOAD)
  case DLOAD extends Bytecode("l:>D", LOAD)
  case ALOAD extends Bytecode("l:>R", LOAD)

  case ILOAD_0 extends Bytecode(">I", LOAD, 0)
  case ILOAD_1 extends Bytecode(">I", LOAD, 1)
  case ILOAD_2 extends Bytecode(">I", LOAD, 2)
  case ILOAD_3 extends Bytecode(">I", LOAD, 3)
  case LLOAD_0 extends Bytecode(">L", LOAD, 0)
  case LLOAD_1 extends Bytecode(">L", LOAD, 1)
  case LLOAD_2 extends Bytecode(">L", LOAD, 2)
  case LLOAD_3 extends Bytecode(">L", LOAD, 3)
  case FLOAD_0 extends Bytecode(">F", LOAD, 0)
  case FLOAD_1 extends Bytecode(">F", LOAD, 1)
  case FLOAD_2 extends Bytecode(">F", LOAD, 2)
  case FLOAD_3 extends Bytecode(">F", LOAD, 3)
  case DLOAD_0 extends Bytecode(">D", LOAD, 0)
  case DLOAD_1 extends Bytecode(">D", LOAD, 1)
  case DLOAD_2 extends Bytecode(">D", LOAD, 2)
  case DLOAD_3 extends Bytecode(">D", LOAD, 3)
  case ALOAD_0 extends Bytecode(">R", LOAD, 0)
  case ALOAD_1 extends Bytecode(">R", LOAD, 1)
  case ALOAD_2 extends Bytecode(">R", LOAD, 2)
  case ALOAD_3 extends Bytecode(">R", LOAD, 3)

  case IALOAD extends Bytecode("AI>I", ARRAYGET)
  case LALOAD extends Bytecode("AI>L", ARRAYGET)
  case FALOAD extends Bytecode("AI>F", ARRAYGET)
  case DALOAD extends Bytecode("AI>D", ARRAYGET)
  case AALOAD extends Bytecode("AI>R", ARRAYGET)
  case BALOAD extends Bytecode("AI>B", ARRAYGET)
  case CALOAD extends Bytecode("AI>C", ARRAYGET)
  case SALOAD extends Bytecode("AI>S", ARRAYGET)

  case ISTORE extends Bytecode("l:I>", STORE)
  case LSTORE extends Bytecode("l:L>", STORE)
  case FSTORE extends Bytecode("l:F>", STORE)
  case DSTORE extends Bytecode("l:D>", STORE)
  case ASTORE extends Bytecode("l:R>", STORE)

  case ISTORE_0 extends Bytecode("I>", STORE, 0)
  case ISTORE_1 extends Bytecode("I>", STORE, 1)
  case ISTORE_2 extends Bytecode("I>", STORE, 2)
  case ISTORE_3 extends Bytecode("I>", STORE, 3)
  case LSTORE_0 extends Bytecode("L>", STORE, 0)
  case LSTORE_1 extends Bytecode("L>", STORE, 1)
  case LSTORE_2 extends Bytecode("L>", STORE, 2)
  case LSTORE_3 extends Bytecode("L>", STORE, 3)
  case FSTORE_0 extends Bytecode("F>", STORE, 0)
  case FSTORE_1 extends Bytecode("F>", STORE, 1)
  case FSTORE_2 extends Bytecode("F>", STORE, 2)
  case FSTORE_3 extends Bytecode("F>", STORE, 3)
  case DSTORE_0 extends Bytecode("D>", STORE, 0)
  case DSTORE_1 extends Bytecode("D>", STORE, 1)
  case DSTORE_2 extends Bytecode("D>", STORE, 2)
  case DSTORE_3 extends Bytecode("D>", STORE, 3)
  case ASTORE_0 extends Bytecode("R>", STORE, 0)
  case ASTORE_1 extends Bytecode("R>", STORE, 1)
  case ASTORE_2 extends Bytecode("R>", STORE, 2)
  case ASTORE_3 extends Bytecode("R>", STORE, 3)

  case IASTORE extends Bytecode("AII>", ARRAYPUT)
  case LASTORE extends Bytecode("AIL>", ARRAYPUT)
  case FASTORE extends Bytecode("AIF>", ARRAYPUT)
  case DASTORE extends Bytecode("AID>", ARRAYPUT)
  case AASTORE extends Bytecode("AIR>", ARRAYPUT)
  case BASTORE extends Bytecode("AIB>", ARRAYPUT)
  case CASTORE extends Bytecode("AIC>", ARRAYPUT)
  case SASTORE extends Bytecode("AIS>", ARRAYPUT)

  case POP extends Bytecode("?>?", STACK)
  case POP2 extends Bytecode("?>?", STACK)
  case DUP extends Bytecode("?>?", STACK)
  case DUP_X1 extends Bytecode("?>?", STACK)
  case DUP_X2 extends Bytecode("?>?", STACK)
  case DUP2 extends Bytecode("?>?", STACK)
  case DUP2_X1 extends Bytecode("?>?", STACK)
  case DUP2_X2 extends Bytecode("?>?", STACK)
  case SWAP extends Bytecode("?>?", STACK)

  case IADD extends Bytecode("II>I", ADD)
  case LADD extends Bytecode("LL>L", ADD)
  case FADD extends Bytecode("FF>F", ADD)
  case DADD extends Bytecode("DD>D", ADD)
  case ISUB extends Bytecode("II>I", SUB)
  case LSUB extends Bytecode("LL>L", SUB)
  case FSUB extends Bytecode("FF>F", SUB)
  case DSUB extends Bytecode("DD>D", SUB)
  case IMUL extends Bytecode("II>I", MUL)
  case LMUL extends Bytecode("LL>L", MUL)
  case FMUL extends Bytecode("FF>F", MUL)
  case DMUL extends Bytecode("DD>D", MUL)
  case IDIV extends Bytecode("II>I", DIV)
  case LDIV extends Bytecode("LL>L", DIV)
  case FDIV extends Bytecode("FF>F", DIV)
  case DDIV extends Bytecode("DD>D", DIV)
  case IREM extends Bytecode("II>I", REM)
  case LREM extends Bytecode("LL>L", REM)
  case FREM extends Bytecode("FF>F", REM)
  case DREM extends Bytecode("DD>D", REM)
  case INEG extends Bytecode("I>I", NEG)
  case LNEG extends Bytecode("L>L", NEG)
  case FNEG extends Bytecode("F>F", NEG)
  case DNEG extends Bytecode("D>D", NEG)

  case ISHL extends Bytecode("II>I", LSL)
  case LSHL extends Bytecode("LI>L", LSL)
  case ISHR extends Bytecode("II>I", ASR)
  case LSHR extends Bytecode("LI>L", ASR)
  case IUSHR extends Bytecode("II>I", LSR)
  case LUSHR extends Bytecode("LI>L", LSR)
  case IAND extends Bytecode("II>I", AND)
  case LAND extends Bytecode("LL>L", AND)
  case IOR extends Bytecode("II>I", OR)
  case LOR extends Bytecode("LL>L", OR)
  case IXOR extends Bytecode("II>I", XOR)
  case LXOR extends Bytecode("LL>L", XOR)

  case IINC extends Bytecode("li:>")

  case I2L extends Bytecode("I>L", ConvertOp.I2L)
  case I2F extends Bytecode("I>F", ConvertOp.I2F)
  case I2D extends Bytecode("I>D", ConvertOp.I2D)
  case L2I extends Bytecode("L>I", ConvertOp.L2I)
  case L2F extends Bytecode("L>F", ConvertOp.L2F)
  case L2D extends Bytecode("L>D", ConvertOp.L2D)
  case F2I extends Bytecode("F>I", ConvertOp.F2I)
  case F2L extends Bytecode("F>L", ConvertOp.F2L)
  case F2D extends Bytecode("F>D", ConvertOp.F2D)
  case D2I extends Bytecode("D>I", ConvertOp.D2I)
  case D2L extends Bytecode("D>L", ConvertOp.D2L)
  case D2F extends Bytecode("D>F", ConvertOp.D2F)
  case I2B extends Bytecode("I>B", ConvertOp.I2B)
  case I2C extends Bytecode("I>C", ConvertOp.I2C)
  case I2S extends Bytecode("I>S", ConvertOp.I2S)

  case LCMP extends Bytecode("LL>I", CMP)
  case FCMPL extends Bytecode("FF>I", CMPL)
  case FCMPG extends Bytecode("FF>I", CMPG)
  case DCMPL extends Bytecode("DD>I", CMPL)
  case DCMPG extends Bytecode("DD>I", CMPG)

  case IFEQ extends Bytecode("j:I>", EQ)
  case IFNE extends Bytecode("j:I>", NE)
  case IFLT extends Bytecode("j:I>", LT)
  case IFGE extends Bytecode("j:I>", GE)
  case IFGT extends Bytecode("j:I>", GT)
  case IFLE extends Bytecode("j:I>", LE)
  case IF_ICMPEQ extends Bytecode("j:II>", EQ)
  case IF_ICMPNE extends Bytecode("j:II>", NE)
  case IF_ICMPLT extends Bytecode("j:II>", LT)
  case IF_ICMPGE extends Bytecode("j:II>", GE)
  case IF_ICMPGT extends Bytecode("j:II>", GT)
  case IF_ICMPLE extends Bytecode("j:II>", LE)
  case IF_ACMPEQ extends Bytecode("j:RR>", EQ)
  case IF_ACMPNE extends Bytecode("j:RR>", NE)

  case GOTO extends Bytecode("j:>", CONTROL)
  case JSR extends Bytecode("j:>?", CONTROL)
  case RET extends Bytecode("l:>", CONTROL)
  case TABLESWITCH extends Bytecode("wj44?:I>", CONTROL)
  case LOOKUPSWITCH extends Bytecode("wj4?:I>", CONTROL)

  case IRETURN extends Bytecode("I>", XRETURN)
  case LRETURN extends Bytecode("L>", XRETURN)
  case FRETURN extends Bytecode("F>", XRETURN)
  case DRETURN extends Bytecode("D>", XRETURN)
  case ARETURN extends Bytecode("R>", XRETURN)
  case RETURN extends Bytecode(">", XRETURN)

  case GETSTATIC extends Bytecode("f:>X")
  case PUTSTATIC extends Bytecode("f:X>")
  case GETFIELD extends Bytecode("f:R>X")
  case PUTFIELD extends Bytecode("f:RX>")

  case INVOKEVIRTUAL extends Bytecode("m:?>?")
  case INVOKESPECIAL extends Bytecode("m:?>?")
  case INVOKESTATIC extends Bytecode("m:?>?")
  case INVOKEINTERFACE extends Bytecode("mii:?>?")
  case INVOKEDYNAMIC extends Bytecode("mii:?>?")

  case NEW extends Bytecode("t:>R")
  case NEWARRAY extends Bytecode("i:I>A")
  case ANEWARRAY extends Bytecode("t:I>A")
  case ARRAYLENGTH extends Bytecode("A>I")
  case ATHROW extends Bytecode("R>R", CONTROL)
  case CHECKCAST extends Bytecode("t:R>R")
  case INSTANCEOF extends Bytecode("t:R>I")
  case MONITORENTER extends Bytecode("R>")
  case MONITOREXIT extends Bytecode("R>")

  case WIDE extends Bytecode()

  case MULTIANEWARRAY extends Bytecode("tu:?>A")
  case IFNULL extends Bytecode("j:R>", EQ)
  case IFNONNULL extends Bytecode("j:R>", NE)
  case GOTO_W extends Bytecode("wj:>", CONTROL)
  case JSR_W extends Bytecode("wj:>?", CONTROL)
  case BREAKPOINT extends Bytecode(">", RESERVED)

  case _RESERVED_203 extends Bytecode(RESERVED)
  case _RESERVED_204 extends Bytecode(RESERVED)
  case _RESERVED_205 extends Bytecode(RESERVED)
  case _RESERVED_206 extends Bytecode(RESERVED)
  case _RESERVED_207 extends Bytecode(RESERVED)
  case _RESERVED_208 extends Bytecode(RESERVED)
  case _RESERVED_209 extends Bytecode(RESERVED)
  case _RESERVED_210 extends Bytecode(RESERVED)
  case _RESERVED_211 extends Bytecode(RESERVED)
  case _RESERVED_212 extends Bytecode(RESERVED)
  case _RESERVED_213 extends Bytecode(RESERVED)
  case _RESERVED_214 extends Bytecode(RESERVED)
  case _RESERVED_215 extends Bytecode(RESERVED)
  case _RESERVED_216 extends Bytecode(RESERVED)
  case _RESERVED_217 extends Bytecode(RESERVED)
  case _RESERVED_218 extends Bytecode(RESERVED)
  case _RESERVED_219 extends Bytecode(RESERVED)
  case _RESERVED_220 extends Bytecode(RESERVED)
  case _RESERVED_221 extends Bytecode(RESERVED)
  case _RESERVED_222 extends Bytecode(RESERVED)
  case _RESERVED_223 extends Bytecode(RESERVED)
  case _RESERVED_224 extends Bytecode(RESERVED)
  case _RESERVED_225 extends Bytecode(RESERVED)
  case _RESERVED_226 extends Bytecode(RESERVED)
  case _RESERVED_227 extends Bytecode(RESERVED)
  case _RESERVED_228 extends Bytecode(RESERVED)
  case _RESERVED_229 extends Bytecode(RESERVED)
  case _RESERVED_230 extends Bytecode(RESERVED)
  case _RESERVED_231 extends Bytecode(RESERVED)
  case _RESERVED_232 extends Bytecode(RESERVED)
  case _RESERVED_233 extends Bytecode(RESERVED)
  case _RESERVED_234 extends Bytecode(RESERVED)
  case _RESERVED_235 extends Bytecode(RESERVED)
  case _RESERVED_236 extends Bytecode(RESERVED)
  case _RESERVED_237 extends Bytecode(RESERVED)
  case _RESERVED_238 extends Bytecode(RESERVED)
  case _RESERVED_239 extends Bytecode(RESERVED)
  case _RESERVED_240 extends Bytecode(RESERVED)
  case _RESERVED_241 extends Bytecode(RESERVED)
  case _RESERVED_242 extends Bytecode(RESERVED)
  case _RESERVED_243 extends Bytecode(RESERVED)
  case _RESERVED_244 extends Bytecode(RESERVED)
  case _RESERVED_245 extends Bytecode(RESERVED)
  case _RESERVED_246 extends Bytecode(RESERVED)
  case _RESERVED_247 extends Bytecode(RESERVED)
  case _RESERVED_248 extends Bytecode(RESERVED)
  case _RESERVED_249 extends Bytecode(RESERVED)
  case _RESERVED_250 extends Bytecode(RESERVED)
  case _RESERVED_251 extends Bytecode(RESERVED)
  case _RESERVED_252 extends Bytecode(RESERVED)
  case _RESERVED_253 extends Bytecode(RESERVED)

  case IMPDEP1 extends Bytecode(">", RESERVED)
  case IMPDEP2 extends Bytecode(">", RESERVED)


  private[bytecode] var length = -1    // length in the bytecode stream (non-wide form, -1 for variable-length instructions)
  private[bytecode] var nparams = -1   // number of static parameters in the bytecode stream (excluding extra params of tableswitch/lookupswitch)
  private[bytecode] var noperands = -1 // number of input values on the operand stack (-1 if unknown)


  /** Instruction encoding. */
  def code = ordinal

  /** Operation kind, used to reduce code duplication. */
  def kind = _kind

  /** Instruction format in string-encoded form. */
  val fmt = checkFormat(_fmt)


  private var _arithOp: ArithOp = _
  private var _compareOp: CompareOp = _
  private var _convertOp: ConvertOp = _
  private[bytecode] var implicitParam = Integer.MIN_VALUE

  def arithOp = _arithOp ensuring (kind == ARITH && _ != null)
  def compareOp = _compareOp ensuring ((kind == UNARY_IF || kind == BINARY_IF) && _ != null)
  def convertOp = _convertOp ensuring (kind == CONVERT && _ != null)

  def hasImplicitParam = implicitParam != Integer.MIN_VALUE
  def mayBeWide = ((kind == OpKind.STORE || kind == OpKind.LOAD) && !hasImplicitParam) || this == RET || this == IINC


  def this(fmt: String = null) = {
    this(fmt, OTHER)
  }

  def this(kind: OpKind) = {
    this(null, kind)
  }

  def this(fmt: String, kind: OpKind, implicitParam: Int) = {
    this(fmt, kind)
    assert(kind == CONST || kind == LOAD || kind == STORE)
    assert(nparams == 0)
    this.implicitParam = implicitParam
    nparams += 1
  }

  def this(fmt: String, op: ArithOp) = {
    this(fmt, ARITH)
    _arithOp = op
  }

  def this(fmt: String, op: CompareOp) = {
    this(fmt, _kind = null)
    _kind = if (noperands == 1) UNARY_IF else BINARY_IF
    _compareOp = op
  }

  def this(fmt: String, op: ConvertOp) = {
    this(fmt, CONVERT)
    _convertOp = op
  }

  override def toString = productPrefix.asciiToLowerCase

  def operandType: BytecodeTypeKind = operandType(0) ensuring (noperands == 1)

  def operandType(i: Int): BytecodeTypeKind = {
    assert(i >= 0 && i < noperands)
    val len = fmt.length
    assert(fmt.charAt(len - 2) == '>')
    decodeType(fmt.charAt(len - 3 - i))
  }

  def resultType = decodeType(fmt.last)

  ////////////////////////////////////////////////
  // PARSING INSTRUCTION FORMATS
  ////////////////////////////////////////////////

  // format: binFormat:opStack
  // binFormat:
  //        l  - unsigned byte index of local variable (two-byte unsigned index for wide instructions)
  //        j  - signed 2-byte branch offset relative to branch instruction
  //        wj - signed 4-byte branch offset relative to branch instruction
  //        c  - unsigned byte index in the constant pool (int|float|string literal|class literal)
  //        wc - unsigned 2-byte index in the constant pool (int|float|long|double|string literal|class literal)
  //        f  - unsigned 2-byte index in the constant pool (field);
  //        m  - unsigned 2-byte index in the constant pool (method);
  //        t  - unsigned 2-byte index in the constant pool (class|array|interface);
  //        i  - imm. byte (sign-extended)
  //        u  - imm. byte (zero-extended)
  //        wi - imm. 2-byte value (sign-extended)
  //        4  - imm. 4-byte value
  // opStack: before>after
  //         I - int; R - reference; A - array; B - sign-extended byte; C - zero-extended char
  //         D - double, F - float, L - long; S - sign-extended short
  //         X - one-slot or two-slot stack element (determined by instruction's static parameters)
  //         ? - unknown (depends on instruction's static parameters)

  private def checkFormat(_fmt: String): String = {
    var fmt = _fmt
    if (fmt == null) {
      return null
    }
    assert(fmt.nonEmpty)
    if (fmt.last == '>') {
      fmt = fmt + 'V'
    }
    val i = fmt.indexOf(':')
    length = checkBinaryFormat(if (i < 0) "" else fmt.substring(0, i))
    noperands = checkStackFormat(fmt.substring(i + 1))
    fmt
  }

  private def checkBinaryFormat(fmt: String): Int = {
    nparams = 0
    var paramsLength = 1
    var wideParam = false
    for (i <- 0 until fmt.length) {
      val ch = fmt.charAt(i)
      assert(!wideParam || (ch != 'w' && ch != 'l'))
      if (ch == '?') {
        assert(i == fmt.length - 1)
        return -1 // unknown length
      }
      if (ch != 'w') {
        if (paramsLength != -1) {
          val paramLength = getParamLength(ch, wideParam)
          if (paramLength != -1) {
            paramsLength += paramLength
          } else {
            paramsLength = -1
          }
        }
        nparams += 1
      }
      wideParam = (ch == 'w')
    }
    paramsLength
  }

  def getParamLength(fmt: Char, wide: Boolean) = fmt match {
    case 'i' | 'c' | 'l' | 'u' => if (wide) 2 else 1
    case 'j' => if (wide) 4 else 2
    case 'f' | 'm' | 't' if !wide => 2
    case '4' if !wide => 4
    case 'T' | 'I' | 'P' | 'S' | 'C' | 'V' if !wide => -1
  }

  private def checkStackFormat(_fmt: String) = {
    var fmt = _fmt
    val delimiter = fmt.indexOf('>')
    assert(delimiter >= 0)
    var nStackOperands = delimiter
    if (nStackOperands == 1 && fmt.charAt(0) == '?') {
      nStackOperands = -1
    } else {
      for (i <- 0 until nStackOperands) {
        val ch = fmt.charAt(i)
        assert((ch != '?') && (ch != 'V'))
        decodeType(ch)
      }
    }
    fmt = fmt.substring(delimiter + 1)
    assert(fmt.length == 1)
    decodeType(fmt.charAt(0))
    nStackOperands
  }

  private def decodeType(ch: Char) = ch match {
    case 'I' => BytecodeTypeKind.INT
    case 'L' => BytecodeTypeKind.LONG
    case 'F' => BytecodeTypeKind.FLOAT
    case 'D' => BytecodeTypeKind.DOUBLE
    case 'B' => BytecodeTypeKind.BYTE
    case 'C' => BytecodeTypeKind.CHAR
    case 'S' => BytecodeTypeKind.SHORT
    case 'R' => BytecodeTypeKind.CLASS
    case 'A' => BytecodeTypeKind.ARRAY
    case 'X' => null // return type dependent on instruction's static parameters
    case '?' => null // unknown type
    case 'V' => BytecodeTypeKind.VOID
  }
}

object Bytecode {

  /** Maximum number of static parameters for any instruction except xxxSWITCH.
    * Used for faster decoding instruction's parameters from bytecode stream.
    */
  val MAX_NPARAMS = 4

  val NEWARRAY_BASIC_TYPE_KIND_START = 4
  val NEWARRAY_BASIC_TYPE_KIND_END = 11

  private val NEWARRAY_BASIC_TYPE_KIND = Array(
    null, null, null, null,
    BytecodeTypeKind.BOOLEAN,
    BytecodeTypeKind.CHAR,
    BytecodeTypeKind.FLOAT,
    BytecodeTypeKind.DOUBLE,
    BytecodeTypeKind.BYTE,
    BytecodeTypeKind.SHORT,
    BytecodeTypeKind.INT,
    BytecodeTypeKind.LONG
  )

  assert(NEWARRAY_BASIC_TYPE_KIND(NEWARRAY_BASIC_TYPE_KIND_START - 1) == null)
  assert(NEWARRAY_BASIC_TYPE_KIND(NEWARRAY_BASIC_TYPE_KIND_START) != null)
  assert(NEWARRAY_BASIC_TYPE_KIND.length == NEWARRAY_BASIC_TYPE_KIND_END + 1)

  def newarrayBasicType(basicType: Int) = NEWARRAY_BASIC_TYPE_KIND(basicType)
}
