/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.common.CodeHelpers.{shouldNotCallThis, shouldNotReachHere}
import com.huawei.excelsior.jet.compiler.bytecode.Bytecode.*
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeIterator.*
import FieldAccessKind.{GETFIELD, GETSTATIC, PUTFIELD, PUTSTATIC}
import MethodAccessKind.*
import com.huawei.excelsior.jet.compiler.verifier.{VerifiableMethod, VerificationUnit}
import xscala.util.MathUtils
import xscala.util.MathUtils.alignUp

/** Iterator over JVM instructions
  *
  * @param stream Array containing bytecode stream.
  * @param streamStart Start offset of bytecode stream in 'stream' array.
  * @param streamLength Number of bytes in the bytecode stream.
  *
  * @author paul
  */
final class BytecodeIterator (stream: Array[Byte], streamStart: Int, streamLength: Int,
                              verify: Boolean = false, verificationContext: VerifiableMethod = null)
  extends VerificationUnit(verify, verificationContext) { bcIter =>

  locally {
    assert(stream != null)
    assert(streamStart >= 0 && streamStart + streamLength <= stream.length)
  }

  def this(bytecode: Array[Byte]) = {
    this(bytecode, 0, bytecode.length)
  }

  /** Constructs BytecodeIterator for specified CodeAttribute.
    *
    * @param code                CodeAttribute containing bytecode stream.
    * @param verify              if we should do verification
    * @param verificationContext verification context
    */
  def this(code: MethodCodeAttribute, verify: Boolean, verificationContext: VerifiableMethod) = {
    this(code.bytecodeArray, code.bytecodeStart, code.bytecodeLength, verify, verificationContext)
  }

  def this(code: MethodCodeAttribute) = {
    this(code, false, null)
  }

  /** Current offset in the bytecode stream. */
  private var position = 0

  /** Last index of iteration. */
  private var endPos = 0

  /** Current instruction. */
  private var op: Bytecode = _

  /** Offset of current instruction in the bytecode stream. */
  private var opPos = 0

  /** Number of current instruction params. */
  private var nparams = 0

  /** Buffer for params of current instruction. */
  private val params = new Array[Int](Bytecode.MAX_NPARAMS)

  locally {
    reset()
  }

  ////////////////////////////////////////////////
  // GETTING INTEGERS FROM BYTECODE
  ////////////////////////////////////////////////

  private def getByte(pos: Int): Byte = {
    verifyThat(pos < endPos, "Unexpected end of bytecode")
    stream(streamStart + pos)
  }

  private def getUByte(pos: Int): Int = getByte(pos) & 0xFF

  private def getShort(pos: Int): Short = ((getUByte(pos) << 8) + getUByte(pos + 1)).toShort

  private def getUShort(pos: Int): Int = getShort(pos) & 0xFFFF

  private def getInt(pos: Int): Int = (getUShort(pos) << 16) + getUShort(pos + 2)

  private def getIntegers(buffer: Array[Int], pos: Int, count: Int, step: Int): Unit = {
    var curPos = pos
    var i = 0
    while (i < count) {
      buffer(i) = getInt(curPos)
      i += 1
      curPos += step * 4
    }
  }

  private def inputFrom(pos: Int): () => Int = {
    var curPos = pos
    { () =>
       val result = bcIter.getByte(curPos) & 0xFF
       curPos += 1
       result
     }
   }

  private def getParam(fmt: Char, wide: Boolean, pos: Int): Int = fmt match {
    case 'i' =>
      if (wide) getShort(pos) else getByte(pos)
    case 'c' | 'l' | 'u' =>
      if (wide) getUShort(pos) else getUByte(pos)
    case 'j' =>
      if (wide) getInt(pos) else getShort(pos)
    case 'f' | 'm' | 't' =>
      assert(!wide)
      getUShort(pos)
    case '4' =>
      assert(!wide)
      getInt(pos)
    case _ =>
      shouldNotReachHere(fmt)
  }

  ////////////////////////////////////////////////
  // PARSING BYTECODES FROM BINARY STREAM
  ////////////////////////////////////////////////

  private def decode(code: Int) = {
    val bc = BC_TABLE(code)
    verifyThat(bc != null, "Unknown instruction op code: %d", code)
    bc
  }

  private def fetch(pos: Int) = {
    var rawBC = getUByte(pos)
    val wide = rawBC == Bytecode.WIDE.code
    if (wide) {
      rawBC = getUByte(pos + 1)
    }
    val curOp = decode(rawBC)
    verifyThat(!wide || curOp.mayBeWide, "Illegal wide modifier for instruction %s", curOp)
    curOp
  }

  private def length(bc: Bytecode, pos: Int): Int = {
    val rawBC = getUByte(pos)
    if (rawBC == Bytecode.WIDE.code) {
      assert(bc.code == getUByte(pos + 1))
      assert(bc.length > 0)
      return 1 + bc.length + (if (bc == Bytecode.IINC) 2 else 1)
    }
    assert(bc.code == rawBC)
    bc match {
      case TABLESWITCH | LOOKUPSWITCH =>
        assert(bc.length == -1)
        val nExtraParams = ncases(bc, pos) * (if (bc == Bytecode.TABLESWITCH) 1 else 2)
        switchParamPos(pos, bc.nparams + nExtraParams) - pos

      case _ =>
        if (bc.length > 0) {
          bc.length
        } else {
          var curPos = pos + 1
          for (i <- 0 until bc.nparams) {
            val ch = bc.fmt.charAt(i)
            curPos += bc.getParamLength(ch, false)
          }
          curPos - pos
        }
    }
  }

  private def ncases(bc: Bytecode, pos: Int) = {
    assert(getUByte(pos) == bc.code)
    bc match {
      case TABLESWITCH => // params: default, low, high, ...
        val low = getInt(switchParamPos(pos, 1))
        val high = getInt(switchParamPos(pos, 2))
        high - low + 1

      case LOOKUPSWITCH => // params: default, npairs, ...
        val npairs = getInt(switchParamPos(pos, 1))
        npairs

      case _ =>
        shouldNotReachHere()
    }
  }
  
  private def getParamsImpl(bc: Bytecode, instrPos: Int, buffer: Array[Int], wide: Boolean): Unit = {
    assert(bc.nparams >= 0)
    var npars = 0
    var curPos = instrPos + 1

    val nExplicitParams = bc.nparams - (if (bc.hasImplicitParam) 1 else 0)
    if (nExplicitParams > 0) {
      var wideParam = false
      var i = 0
      var ch = bc.fmt.charAt(i)
      while (ch != ':') {
        assert(!(wide && wideParam))
        if (ch != 'w') {
          var par: Int = getParam(ch, wide || wideParam, curPos)
          if (ch == 'j') {
            assert(!wide)
            par += instrPos // convert relative branch offsets to absolute
          }
          buffer(npars) = par
          npars += 1
          curPos += bc.getParamLength(ch, wide || wideParam)
        }
        wideParam = ch == 'w'
        i += 1
        ch = bc.fmt.charAt(i)
      }
    }

    if (bc.hasImplicitParam) {
      buffer(npars) = bc.implicitParam
      npars += 1
    }
    assert(npars == bc.nparams)
  }

  // TODO: combine with `length(bc, pos)`, specialize & simplify
  private def getParams(bc: Bytecode, pos: Int, buffer: Array[Int]): Unit = {
    val rawBC = getUByte(pos)
    if (rawBC == Bytecode.WIDE.code) {
      assert(bc.code == getUByte(pos + 1))
      getParamsImpl(bc, pos + 1, buffer, wide = true)
    } else {
      assert(bc.code == rawBC)
      bc match {
        case TABLESWITCH | LOOKUPSWITCH =>
          getIntegers(buffer, switchParamPos(pos, 0), bc.nparams, 1)
          buffer(0) += pos // defOffs
        case _ =>
          if (bc.nparams > 0) {
            getParamsImpl(bc, pos, buffer, wide = false)
          }
      }
    }
  }


  ////////////////////////////////////////////////
  // ITERATION PUBLIC API
  ////////////////////////////////////////////////

  /** @return current offset in the bytecode stream. */
  def offset = position

  /** Returns param of current instruction with specified index.
    *
    * @param i param index.
    * @return i'th param.
    */
  def param(i: Int): Int = {
    assert(i >= 0 && i < nparams)
    params(i)
  }

  /** Returns single param of current instruction.
    * Asserts if params count not equals to one.
    *
    * @return single param of current instruction.
    */
  def param: Int = {
    assert(nparams == 1)
    param(0)
  }

  /** @return true if the iteration has more instructions.
    */
  def hasNext = position < endPos

  /** Advances current position and returns next instruction.
    * Asserts if iteration has no more instructions.
    *
    * @return next instruction.
    */
  def next() = {
    assert(hasNext)
    opPos = position
    op = fetch(opPos)
    nparams = op.nparams
    assert(params.length >= nparams)
    getParams(op, opPos, params)
    position = opPos + length(op, opPos)
    op
  }

  def getRawBytesOfCurrentBC = {
    assert(op != null)
    stream.slice(streamStart + opPos, streamStart + position)
  }

  /** Returns bytecode offsets of target instructions for each case of tableswitch/lookupswitch. */
  def getSwitchTargets: Array[Int] = {
    assert(opPos >= 0)
    assert((op == Bytecode.TABLESWITCH) ||  // params: defOffs, low, high, offs0, offs1, ...
           (op == Bytecode.LOOKUPSWITCH))   // params: defOffs, npairs, match0, offs0, match1, offs1, ...

    val ntargets = ncases(op, opPos)
    val bcTargets = new Array[Int](ntargets)
    getIntegers(bcTargets, switchParamPos(opPos, 3), ntargets, if (op == Bytecode.TABLESWITCH) 1 else 2)
    bcTargets.mapInPlace(_ + opPos)
    bcTargets
  }

  /** Returns match values of lookupswitch instruction. */
  def getSwitchMatches: Array[Int] = {
    assert(opPos >= 0)
    assert(op == Bytecode.LOOKUPSWITCH) // params: defOffs, npairs, match0, offs0, match1, offs1, ...

    val npairs = ncases(op, opPos)
    val labels = new Array[Int](npairs)
    getIntegers(labels, switchParamPos(opPos, 2), npairs, 2)
    labels
  }

  def isSwitchPaddingZero: Boolean = {
    assert(opPos >= 0)
    assert((op == Bytecode.LOOKUPSWITCH) || (op == Bytecode.TABLESWITCH))

    val paddingEnd = switchParamPos(opPos, 0)
    for (pos <- opPos + 1 until paddingEnd) {
      if (getByte(pos) != 0) {
        return false
      }
    }
    true
  }

  /** Reset BytecodeIterator to specified range [startPC, endPC).
    *
    * @param startPC start offset of iteration.
    * @param endPC   end offset of iteration.
    */
  def reset(startPC: Int, endPC: Int): Unit = {
    assert(startPC >= 0)
    assert(startPC <= endPC)
    assert(endPC <= streamLength)
    this.position = startPC
    this.endPos = endPC
    this.op = null
    this.opPos = -1
    this.nparams = 0
  }

  /** Reset BytecodeIterator to range [0, stream.length).
    */
  def reset(): Unit = {
    reset(0, streamLength)
  }

  ////////////////////////////////////////////////
  // SMART INTERNAL ITERATOR
  ////////////////////////////////////////////////

  /** Iterate and process all bytecode instructions with specified bytecode processor.
    * Apply special process methods for each iterated instruction.
    *
    * @param processor bytecode processor.
    */
  def iterate(processor: BytecodeProcessor): Unit = {
    iterate(processor, 0, streamLength)
  }

  /** Iterate and process all bytecode instructions from specified range [startPC, endPC)
    * with specified bytecode processor.
    * Apply special process methods for each iterated instruction.
    *
    * @param processor bytecode processor.
    * @param startPC   start offset of iteration.
    * @param endPC     end offset of iteration.
    */
  def iterate(processor: BytecodeProcessor, startPC: Int, endPC: Int): Unit = {
    reset(startPC, endPC)
    while (hasNext) {
      val startOffs = offset
      val curOp = next()
      processor.startInstruction(startOffs, offset)
      processOne(processor, curOp)
      processor.finishInstruction()
    }
  }

  private def processOne(processor: BytecodeProcessor, op: Bytecode): Unit = {
    import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.*
    import com.huawei.excelsior.jet.compiler.bytecode.OpKind.*

    op.kind match {
      case CONST =>
        processor.pushConst(op.resultType, param)

      case LOAD =>
        processor.pushLocal(op.resultType, param)

      case STORE =>
        processor.storeLocal(op.operandType, param)

      // arithmetic and logical instructions
      case ARITH =>
        val `type`: BytecodeTypeKind = op.arithOp match {
          case CMP | CMPL | CMPG => op.operandType(0)
          case _ => op.resultType
        }
        processor.arithOp(`type`, op.arithOp)

      case CONVERT =>
        processor.convert(op.convertOp)

      case STACK =>
        processor.stackOp(op)

      case ARRAYGET =>
        processor.arrayGet(op.resultType)

      case ARRAYPUT =>
        processor.arrayPut(op.operandType(0))

      case XRETURN =>
        val isLastBytecode = !hasNext
        processor.doReturn(if (op.noperands == 0) BytecodeTypeKind.VOID else op.operandType, isLastBytecode)

      case UNARY_IF =>
        processor.unaryIf(op.operandType(0), op.compareOp, param)

      case BINARY_IF =>
        processor.binaryIf(op.operandType(0), op.compareOp, param)

      case CONTROL =>
        processControl(processor, op)

      case RESERVED =>

      case OTHER =>
        processOther(processor, op)
    }
  }

  private def processControl(processor: BytecodeProcessor, op: Bytecode): Unit = {
    op match {
      // control transfer instructions
      case GOTO | GOTO_W =>
        processor.jump(param)

      case JSR | JSR_W =>
        processor.jsr(param)

      case RET =>
        processor.ret(param)

      case ATHROW =>
        processor.doThrow()

      // table jumping
      case TABLESWITCH =>
        processor.tableSwitch(param(0), param(1), param(2), getSwitchTargets)

      case LOOKUPSWITCH =>
        processor.lookupSwitch(param(0), getSwitchMatches, getSwitchTargets)

      case _ =>
        shouldNotReachHere(op)
    }
  }

  private def processOther(processor: BytecodeProcessor, op: Bytecode): Unit = {
    op match {
      // pushing constants onto the stack
      case LDC | LDC_W | LDC2_W =>
        processor.pushCPEntry(param)

      case IINC =>
        processor.increment(param(0), param(1))

      // monitors
      case MONITORENTER =>
        processor.monitorEnter()

      case MONITOREXIT =>
        processor.monitorExit()

      // manipulating fields
      case Bytecode.GETFIELD =>
        processor.fieldOp(param, GETFIELD)

      case Bytecode.PUTFIELD =>
        processor.fieldOp(param, PUTFIELD)

      case Bytecode.GETSTATIC =>
        processor.fieldOp(param, GETSTATIC)

      case Bytecode.PUTSTATIC =>
        processor.fieldOp(param, PUTSTATIC)

      // Method invocation
      case INVOKESPECIAL =>
        processor.invoke(param, SPECIAL)

      case INVOKESTATIC =>
        processor.invoke(param, STATIC)

      case INVOKEVIRTUAL =>
        processor.invoke(param, VIRTUAL)

      case INVOKEINTERFACE =>
        processor.invoke(param(0), INTERFACE)

      case INVOKEDYNAMIC =>
        processor.invoke(param(0), DYNAMIC)

      //  Miscellaneous object operations
      case NEW =>
        processor.doNew(param)

      case INSTANCEOF =>
        processor.instanceOf(param)

      case CHECKCAST =>
        processor.checkCast(param)

      // managing arrays
      case NEWARRAY =>
        processor.newPrimitiveArray(Bytecode.newarrayBasicType(param))

      case ANEWARRAY =>
        processor.newObjectArray(param)

      case MULTIANEWARRAY =>
        processor.newMultiObjectArray(param(0), param(1))

      case ARRAYLENGTH =>
        processor.arrayLength()

      // no-op
      case NOP =>
        processor.nop()

      case _ =>
        shouldNotReachHere(op)
    }
  }
}

object BytecodeIterator {

  val INVALID_IDX = -1

  private val BC_TABLE: Array[Bytecode] = {
    val table = new Array[Bytecode](256)
    for (bc <- Bytecode.values) {
      assert(table(bc.code) == null)
      table(bc.code) = bc
    }
    table
  }

  /** Returns pos of i-th param of tableswitch/lookupswitch instruction
    */
  private def switchParamPos(opcodePos: Int, i: Int) = alignUp(opcodePos + 1, 4) + i * 4
}
