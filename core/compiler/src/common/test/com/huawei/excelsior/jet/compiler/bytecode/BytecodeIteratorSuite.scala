/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

import com.huawei.excelsior.common.CodeHelpers.shouldNotCallThis
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp.*
import com.huawei.excelsior.jet.compiler.bytecode.Bytecode.*
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind.*
import com.huawei.excelsior.jet.compiler.bytecode.CompareOp.*
import com.huawei.excelsior.jet.compiler.symlevel.Method
import org.easymock.EasyMock.{eq as eq_, *}
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

/** Tests for [[BytecodeIterator]].
  *
  * @author cypok
  */
class BytecodeIteratorSuite extends AnyFunSuite with BeforeAndAfter {

  private var processor: BytecodeProcessor = _
  private var bytecode: Array[Byte] = _
  private val bcStreamOffs = 3

  private val code: Method.CodeAttribute = new Method.CodeAttribute() {
    override def bytecodeArray = bytecode
    override def bytecodeStart = bcStreamOffs
    override def bytecodeLength = bytecode.length - bcStreamOffs

    override def maxStack = -1
    override def maxLocals = -1

    override def hasExceptionTable = false
    override def exceptionTableLength = 0
    override def getExceptionTableTraverser = shouldNotCallThis()

    override def findLineNumber(bytecodeOffset: Int) = -1

    override def localVariableTable: Array[Byte] = null
    override def stackMapTable: Array[Byte] = null
  }

  private def bytecode(bytes: Any*): Unit = {
    bytecode = new Array[Byte](bytes.length + bcStreamOffs)
    for ((byte, i) <- bytes.zipWithIndex) {
      byte match {
        case bc: Bytecode =>
          bytecode(i + bcStreamOffs) = bc.code.toByte
        case b: Number =>
          bytecode(i + bcStreamOffs) = b.byteValue
      }
    }
    nextOffset = bytes.length
  }

  private var nextOffset = 0

  before { // set up
    processor = createMock(classOf[BytecodeProcessor])
  }

  after { // tear down
    // this is done every time
    processor.startInstruction(0, nextOffset)
    processor.finishInstruction()
    replay(processor)

    new BytecodeIterator(code, false, null).iterate(processor)
    verify(processor)
  }

  test("NOP") {
    bytecode(NOP)
    processor.nop()
  }

  test("BREAKPOINT") {
    bytecode(BREAKPOINT)
  }

  test("IMPDEP1") {
    bytecode(IMPDEP1)
  }

  test("IMPDEP2") {
    bytecode(IMPDEP2)
  }

  test("IRETURN") {
    bytecode(IRETURN)
    processor.doReturn(INT, true)
  }

  test("LRETURN") {
    bytecode(LRETURN)
    processor.doReturn(LONG, true)
  }

  test("FRETURN") {
    bytecode(FRETURN)
    processor.doReturn(FLOAT, true)
  }

  test("DRETURN") {
    bytecode(DRETURN)
    processor.doReturn(DOUBLE, true)
  }

  test("ARETURN") {
    bytecode(ARETURN)
    processor.doReturn(CLASS, true)
  }

  test("RETURN") {
    bytecode(RETURN)
    processor.doReturn(VOID, true)
  }

  test("BIPUSH") {
    bytecode(BIPUSH, 0x12)
    processor.pushConst(BYTE, 0x12)
  }

  test("BIPUSHNegative") {
    bytecode(BIPUSH, 0xff)
    processor.pushConst(BYTE, -1)
  }

  test("SIPUSH") {
    bytecode(SIPUSH, 0x12, 0x34)
    processor.pushConst(SHORT, 0x1234)
  }

  test("SIPUSHNegative") {
    bytecode(SIPUSH, 0xff, 0xff)
    processor.pushConst(SHORT, -1)
  }

  test("ACONSTNULL") {
    bytecode(ACONST_NULL)
    processor.pushConst(CLASS, 0)
  }

  test("ICONSTM1") {
    bytecode(ICONST_M1)
    processor.pushConst(INT, -1)
  }

  test("ICONST0") {
    bytecode(ICONST_0)
    processor.pushConst(INT, 0)
  }

  test("ICONST1") {
    bytecode(ICONST_1)
    processor.pushConst(INT, 1)
  }

  test("ICONST2") {
    bytecode(ICONST_2)
    processor.pushConst(INT, 2)
  }

  test("ICONST3") {
    bytecode(ICONST_3)
    processor.pushConst(INT, 3)
  }

  test("ICONST4") {
    bytecode(ICONST_4)
    processor.pushConst(INT, 4)
  }

  test("ICONST5") {
    bytecode(ICONST_5)
    processor.pushConst(INT, 5)
  }

  test("LCONST0") {
    bytecode(LCONST_0)
    processor.pushConst(LONG, 0)
  }

  test("LCONST1") {
    bytecode(LCONST_1)
    processor.pushConst(LONG, 1)
  }

  test("FCONST0") {
    bytecode(FCONST_0)
    processor.pushConst(FLOAT, 0)
  }

  test("FCONST1") {
    bytecode(FCONST_1)
    processor.pushConst(FLOAT, 1)
  }

  test("FCONST2") {
    bytecode(FCONST_2)
    processor.pushConst(FLOAT, 2)
  }

  test("DCONST0") {
    bytecode(DCONST_0)
    processor.pushConst(DOUBLE, 0)
  }

  test("DCONST1") {
    bytecode(DCONST_1)
    processor.pushConst(DOUBLE, 1)
  }

  test("IADD") {
    bytecode(IADD)
    processor.arithOp(INT, ADD)
  }

  test("ISUB") {
    bytecode(ISUB)
    processor.arithOp(INT, SUB)
  }

  test("IMUL") {
    bytecode(IMUL)
    processor.arithOp(INT, MUL)
  }

  test("IDIV") {
    bytecode(IDIV)
    processor.arithOp(INT, DIV)
  }

  test("IREM") {
    bytecode(IREM)
    processor.arithOp(INT, REM)
  }

  test("IAND") {
    bytecode(IAND)
    processor.arithOp(INT, AND)
  }

  test("IOR") {
    bytecode(IOR)
    processor.arithOp(INT, OR)
  }

  test("IXOR") {
    bytecode(IXOR)
    processor.arithOp(INT, XOR)
  }

  test("ISHL") {
    bytecode(ISHL)
    processor.arithOp(INT, LSL)
  }

  test("ISHR") {
    bytecode(ISHR)
    processor.arithOp(INT, ASR)
  }

  test("IUSHR") {
    bytecode(IUSHR)
    processor.arithOp(INT, LSR)
  }

  test("INEG") {
    bytecode(INEG)
    processor.arithOp(INT, NEG)
  }

  test("LADD") {
    bytecode(LADD)
    processor.arithOp(LONG, ADD)
  }

  test("LSUB") {
    bytecode(LSUB)
    processor.arithOp(LONG, SUB)
  }

  test("LMUL") {
    bytecode(LMUL)
    processor.arithOp(LONG, MUL)
  }

  test("LDIV") {
    bytecode(LDIV)
    processor.arithOp(LONG, DIV)
  }

  test("LREM") {
    bytecode(LREM)
    processor.arithOp(LONG, REM)
  }

  test("LAND") {
    bytecode(LAND)
    processor.arithOp(LONG, AND)
  }

  test("LOR") {
    bytecode(LOR)
    processor.arithOp(LONG, OR)
  }

  test("LXOR") {
    bytecode(LXOR)
    processor.arithOp(LONG, XOR)
  }

  test("LSHL") {
    bytecode(LSHL)
    processor.arithOp(LONG, LSL)
  }

  test("LSHR") {
    bytecode(LSHR)
    processor.arithOp(LONG, ASR)
  }

  test("LUSHR") {
    bytecode(LUSHR)
    processor.arithOp(LONG, LSR)
  }

  test("LCMP") {
    bytecode(LCMP)
    processor.arithOp(LONG, CMP)
  }

  test("LNEG") {
    bytecode(LNEG)
    processor.arithOp(LONG, NEG)
  }

  test("FADD") {
    bytecode(FADD)
    processor.arithOp(FLOAT, ADD)
  }

  test("FSUB") {
    bytecode(FSUB)
    processor.arithOp(FLOAT, SUB)
  }

  test("FMUL") {
    bytecode(FMUL)
    processor.arithOp(FLOAT, MUL)
  }

  test("FDIV") {
    bytecode(FDIV)
    processor.arithOp(FLOAT, DIV)
  }

  test("FREM") {
    bytecode(FREM)
    processor.arithOp(FLOAT, REM)
  }

  test("FCMPL") {
    bytecode(FCMPL)
    processor.arithOp(FLOAT, CMPL)
  }

  test("FCMPG") {
    bytecode(FCMPG)
    processor.arithOp(FLOAT, CMPG)
  }

  test("FNEG") {
    bytecode(FNEG)
    processor.arithOp(FLOAT, NEG)
  }

  test("DADD") {
    bytecode(DADD)
    processor.arithOp(DOUBLE, ADD)
  }

  test("DSUB") {
    bytecode(DSUB)
    processor.arithOp(DOUBLE, SUB)
  }

  test("DMUL") {
    bytecode(DMUL)
    processor.arithOp(DOUBLE, MUL)
  }

  test("DDIV") {
    bytecode(DDIV)
    processor.arithOp(DOUBLE, DIV)
  }

  test("DREM") {
    bytecode(DREM)
    processor.arithOp(DOUBLE, REM)
  }

  test("DCMPL") {
    bytecode(DCMPL)
    processor.arithOp(DOUBLE, CMPL)
  }

  test("DCMPG") {
    bytecode(DCMPG)
    processor.arithOp(DOUBLE, CMPG)
  }

  test("DNEG") {
    bytecode(DNEG)
    processor.arithOp(DOUBLE, NEG)
  }

  test("I2L") {
    bytecode(I2L)
    processor.convert(ConvertOp.I2L)
  }

  test("I2F") {
    bytecode(I2F)
    processor.convert(ConvertOp.I2F)
  }

  test("I2D") {
    bytecode(I2D)
    processor.convert(ConvertOp.I2D)
  }

  test("L2I") {
    bytecode(L2I)
    processor.convert(ConvertOp.L2I)
  }

  test("L2F") {
    bytecode(L2F)
    processor.convert(ConvertOp.L2F)
  }

  test("L2D") {
    bytecode(L2D)
    processor.convert(ConvertOp.L2D)
  }

  test("F2I") {
    bytecode(F2I)
    processor.convert(ConvertOp.F2I)
  }

  test("F2L") {
    bytecode(F2L)
    processor.convert(ConvertOp.F2L)
  }

  test("F2D") {
    bytecode(F2D)
    processor.convert(ConvertOp.F2D)
  }

  test("D2I") {
    bytecode(D2I)
    processor.convert(ConvertOp.D2I)
  }

  test("D2L") {
    bytecode(D2L)
    processor.convert(ConvertOp.D2L)
  }

  test("D2F") {
    bytecode(D2F)
    processor.convert(ConvertOp.D2F)
  }

  test("I2B") {
    bytecode(I2B)
    processor.convert(ConvertOp.I2B)
  }

  test("I2C") {
    bytecode(I2C)
    processor.convert(ConvertOp.I2C)
  }

  test("I2S") {
    bytecode(I2S)
    processor.convert(ConvertOp.I2S)
  }

  test("POP") {
    bytecode(POP)
    processor.stackOp(Bytecode.POP)
  }

  test("POP2") {
    bytecode(POP2)
    processor.stackOp(Bytecode.POP2)
  }

  test("SWAP") {
    bytecode(SWAP)
    processor.stackOp(Bytecode.SWAP)
  }

  test("DUP") {
    bytecode(DUP)
    processor.stackOp(Bytecode.DUP)
  }

  test("DUP2") {
    bytecode(DUP2)
    processor.stackOp(Bytecode.DUP2)
  }

  test("DUP_X1") {
    bytecode(DUP_X1)
    processor.stackOp(Bytecode.DUP_X1)
  }

  test("DUP2_X1") {
    bytecode(DUP2_X1)
    processor.stackOp(Bytecode.DUP2_X1)
  }

  test("DUP_X2") {
    bytecode(DUP_X2)
    processor.stackOp(Bytecode.DUP_X2)
  }

  test("DUP2_X2") {
    bytecode(DUP2_X2)
    processor.stackOp(Bytecode.DUP2_X2)
  }

  test("GOTO") {
    bytecode(GOTO, 0x12, 0x34)
    processor.jump(0x1234)
  }

  ignore("GOTOBack") {
    bytecode(GOTO, 0xFF, 0xFF)
    processor.jump(3)
  }

  test("GOTOW") {
    bytecode(GOTO_W, 0x12, 0x34, 0x56, 0x78)
    processor.jump(0x12345678)
  }

  test("GOTOWBack") {
    bytecode(GOTO_W, 0xff, 0xff, 0xff, 0xff)
    processor.jump(-1)
  }

  test("JSR") {
    bytecode(JSR, 0x12, 0x34)
    processor.jsr(0x1234)
  }

  test("JSRW") {
    bytecode(JSR_W, 0x12, 0x34, 0x56, 0x78)
    processor.jsr(0x12345678)
  }

  test("RET") {
    bytecode(RET, 0xAB)
    processor.ret(0xAB)
  }

  test("IFEQ") {
    bytecode(IFEQ, 0x12, 0x34)
    processor.unaryIf(INT, EQ, 0x1234)
  }

  test("IFNE") {
    bytecode(IFNE, 0x12, 0x34)
    processor.unaryIf(INT, NE, 0x1234)
  }

  test("IFLT") {
    bytecode(IFLT, 0x12, 0x34)
    processor.unaryIf(INT, LT, 0x1234)
  }

  test("IFLE") {
    bytecode(IFLE, 0x12, 0x34)
    processor.unaryIf(INT, LE, 0x1234)
  }

  test("IFGT") {
    bytecode(IFGT, 0x12, 0x34)
    processor.unaryIf(INT, GT, 0x1234)
  }

  test("IFGE") {
    bytecode(IFGE, 0x12, 0x34)
    processor.unaryIf(INT, GE, 0x1234)
  }

  test("IFNULL") {
    bytecode(IFNULL, 0x12, 0x34)
    processor.unaryIf(CLASS, EQ, 0x1234)
  }

  test("IFNONNULL") {
    bytecode(IFNONNULL, 0x12, 0x34)
    processor.unaryIf(CLASS, NE, 0x1234)
  }

  test("IF_ICMPEQ") {
    bytecode(IF_ICMPEQ, 0x12, 0x34)
    processor.binaryIf(INT, EQ, 0x1234)
  }

  test("IF_ICMPNE") {
    bytecode(IF_ICMPNE, 0x12, 0x34)
    processor.binaryIf(INT, NE, 0x1234)
  }

  test("IF_ICMPLT") {
    bytecode(IF_ICMPLT, 0x12, 0x34)
    processor.binaryIf(INT, LT, 0x1234)
  }

  test("IF_ICMPLE") {
    bytecode(IF_ICMPLE, 0x12, 0x34)
    processor.binaryIf(INT, LE, 0x1234)
  }

  test("IF_ICMPGT") {
    bytecode(IF_ICMPGT, 0x12, 0x34)
    processor.binaryIf(INT, GT, 0x1234)
  }

  test("IF_ICMPGE") {
    bytecode(IF_ICMPGE, 0x12, 0x34)
    processor.binaryIf(INT, GE, 0x1234)
  }

  test("IF_ACMPEQ") {
    bytecode(IF_ACMPEQ, 0x12, 0x34)
    processor.binaryIf(CLASS, EQ, 0x1234)
  }

  test("IF_ACMPNE") {
    bytecode(IF_ACMPNE, 0x12, 0x34)
    processor.binaryIf(CLASS, NE, 0x1234)
  }

  test("TABLESWITCH") {
    bytecode(TABLESWITCH, 0x00, 0x00, 0x00,
      0x10, 0x20, 0x30, 0x40,
      0x00, 0x00, 0x00, 0x04,
      0x00, 0x00, 0x00, 0x05,
      0x11, 0x11, 0x11, 0x12,
      0x22, 0x22, 0x22, 0x23)
    processor.tableSwitch(eq_(0x10203040), eq_(4), eq_(5), aryEq(Array[Int](0x11111112, 0x22222223)))
  }

  test("LOOKUPSWITCH") {
    bytecode(LOOKUPSWITCH, 0x00, 0x00, 0x00,
      0x10, 0x20, 0x30, 0x40,
      0x00, 0x00, 0x00, 0x02,
      0x11, 0x11, 0x11, 0x12,
      0x22, 0x22, 0x22, 0x23,
      0x33, 0x33, 0x33, 0x34,
      0x44, 0x44, 0x44, 0x45)
    processor.lookupSwitch(eq_(0x10203040),
      aryEq(Array[Int](0x11111112, 0x33333334)),
      aryEq(Array[Int](0x22222223, 0x44444445)))
  }

  test("ILOAD") {
    bytecode(ILOAD, 0x04)
    processor.pushLocal(INT, 4)
  }

  test("FLOAD") {
    bytecode(FLOAD, 0x04)
    processor.pushLocal(FLOAT, 4)
  }

  test("ALOAD") {
    bytecode(ALOAD, 0x04)
    processor.pushLocal(CLASS, 4)
  }

  test("LLOAD") {
    bytecode(LLOAD, 0x04)
    processor.pushLocal(LONG, 4)
  }

  test("DLOAD") {
    bytecode(DLOAD, 0x04)
    processor.pushLocal(DOUBLE, 4)
  }

  test("ILOAD_0") {
    bytecode(ILOAD_0)
    processor.pushLocal(INT, 0)
  }

  test("ILOAD_1") {
    bytecode(ILOAD_1)
    processor.pushLocal(INT, 1)
  }

  test("ILOAD_2") {
    bytecode(ILOAD_2)
    processor.pushLocal(INT, 2)
  }

  test("ILOAD_3") {
    bytecode(ILOAD_3)
    processor.pushLocal(INT, 3)
  }

  test("FLOAD_0") {
    bytecode(FLOAD_0)
    processor.pushLocal(FLOAT, 0)
  }

  test("FLOAD_1") {
    bytecode(FLOAD_1)
    processor.pushLocal(FLOAT, 1)
  }

  test("FLOAD_2") {
    bytecode(FLOAD_2)
    processor.pushLocal(FLOAT, 2)
  }

  test("FLOAD_3") {
    bytecode(FLOAD_3)
    processor.pushLocal(FLOAT, 3)
  }

  test("ALOAD_0") {
    bytecode(ALOAD_0)
    processor.pushLocal(CLASS, 0)
  }

  test("ALOAD_1") {
    bytecode(ALOAD_1)
    processor.pushLocal(CLASS, 1)
  }

  test("ALOAD_2") {
    bytecode(ALOAD_2)
    processor.pushLocal(CLASS, 2)
  }

  test("ALOAD_3") {
    bytecode(ALOAD_3)
    processor.pushLocal(CLASS, 3)
  }

  test("LLOAD_0") {
    bytecode(LLOAD_0)
    processor.pushLocal(LONG, 0)
  }

  test("LLOAD_1") {
    bytecode(LLOAD_1)
    processor.pushLocal(LONG, 1)
  }

  test("LLOAD_2") {
    bytecode(LLOAD_2)
    processor.pushLocal(LONG, 2)
  }

  test("LLOAD_3") {
    bytecode(LLOAD_3)
    processor.pushLocal(LONG, 3)
  }

  test("DLOAD_0") {
    bytecode(DLOAD_0)
    processor.pushLocal(DOUBLE, 0)
  }

  test("DLOAD_1") {
    bytecode(DLOAD_1)
    processor.pushLocal(DOUBLE, 1)
  }

  test("DLOAD_2") {
    bytecode(DLOAD_2)
    processor.pushLocal(DOUBLE, 2)
  }

  test("DLOAD_3") {
    bytecode(DLOAD_3)
    processor.pushLocal(DOUBLE, 3)
  }

  test("ISTORE") {
    bytecode(ISTORE, 0x04)
    processor.storeLocal(INT, 4)
  }

  test("FSTORE") {
    bytecode(FSTORE, 0x04)
    processor.storeLocal(FLOAT, 4)
  }

  test("ASTORE") {
    bytecode(ASTORE, 0x04)
    processor.storeLocal(CLASS, 4)
  }

  test("LSTORE") {
    bytecode(LSTORE, 0x04)
    processor.storeLocal(LONG, 4)
  }

  test("DSTORE") {
    bytecode(DSTORE, 0x04)
    processor.storeLocal(DOUBLE, 4)
  }

  test("ISTORE_0") {
    bytecode(ISTORE_0)
    processor.storeLocal(INT, 0)
  }

  test("ISTORE_1") {
    bytecode(ISTORE_1)
    processor.storeLocal(INT, 1)
  }

  test("ISTORE_2") {
    bytecode(ISTORE_2)
    processor.storeLocal(INT, 2)
  }

  test("ISTORE_3") {
    bytecode(ISTORE_3)
    processor.storeLocal(INT, 3)
  }

  test("FSTORE_0") {
    bytecode(FSTORE_0)
    processor.storeLocal(FLOAT, 0)
  }

  test("FSTORE_1") {
    bytecode(FSTORE_1)
    processor.storeLocal(FLOAT, 1)
  }

  test("FSTORE_2") {
    bytecode(FSTORE_2)
    processor.storeLocal(FLOAT, 2)
  }

  test("FSTORE_3") {
    bytecode(FSTORE_3)
    processor.storeLocal(FLOAT, 3)
  }

  test("ASTORE_0") {
    bytecode(ASTORE_0)
    processor.storeLocal(CLASS, 0)
  }

  test("ASTORE_1") {
    bytecode(ASTORE_1)
    processor.storeLocal(CLASS, 1)
  }

  test("ASTORE_2") {
    bytecode(ASTORE_2)
    processor.storeLocal(CLASS, 2)
  }

  test("ASTORE_3") {
    bytecode(ASTORE_3)
    processor.storeLocal(CLASS, 3)
  }

  test("LSTORE_0") {
    bytecode(LSTORE_0)
    processor.storeLocal(LONG, 0)
  }

  test("LSTORE_1") {
    bytecode(LSTORE_1)
    processor.storeLocal(LONG, 1)
  }

  test("LSTORE_2") {
    bytecode(LSTORE_2)
    processor.storeLocal(LONG, 2)
  }

  test("LSTORE_3") {
    bytecode(LSTORE_3)
    processor.storeLocal(LONG, 3)
  }

  test("DSTORE_0") {
    bytecode(DSTORE_0)
    processor.storeLocal(DOUBLE, 0)
  }

  test("DSTORE_1") {
    bytecode(DSTORE_1)
    processor.storeLocal(DOUBLE, 1)
  }

  test("DSTORE_2") {
    bytecode(DSTORE_2)
    processor.storeLocal(DOUBLE, 2)
  }

  test("DSTORE_3") {
    bytecode(DSTORE_3)
    processor.storeLocal(DOUBLE, 3)
  }

  test("IINC") {
    bytecode(IINC, 3, 8)
    processor.increment(3, 8)
  }

  test("WIDEILOAD") {
    bytecode(WIDE, ILOAD, 0x01, 0x20)
    processor.pushLocal(INT, 0x0120)
  }

  test("WIDEFLOAD") {
    bytecode(WIDE, FLOAD, 0x01, 0x20)
    processor.pushLocal(FLOAT, 0x0120)
  }

  test("WIDEALOAD") {
    bytecode(WIDE, ALOAD, 0x01, 0x20)
    processor.pushLocal(CLASS, 0x0120)
  }

  test("WIDELLOAD") {
    bytecode(WIDE, LLOAD, 0x01, 0x20)
    processor.pushLocal(LONG, 0x0120)
  }

  test("WIDEDLOAD") {
    bytecode(WIDE, DLOAD, 0x01, 0x20)
    processor.pushLocal(DOUBLE, 0x0120)
  }

  test("WIDEISTORE") {
    bytecode(WIDE, ISTORE, 0x01, 0x20)
    processor.storeLocal(INT, 0x0120)
  }

  test("WIDEFSTORE") {
    bytecode(WIDE, FSTORE, 0x01, 0x20)
    processor.storeLocal(FLOAT, 0x0120)
  }

  test("WIDEASTORE") {
    bytecode(WIDE, ASTORE, 0x01, 0x20)
    processor.storeLocal(CLASS, 0x0120)
  }

  test("WIDELSTORE") {
    bytecode(WIDE, LSTORE, 0x01, 0x20)
    processor.storeLocal(LONG, 0x0120)
  }

  test("WIDEDSTORE") {
    bytecode(WIDE, DSTORE, 0x01, 0x20)
    processor.storeLocal(DOUBLE, 0x0120)
  }

  test("WIDEIINC") {
    bytecode(WIDE, IINC, 0x01, 0x20, 0x12, 0x34)
    processor.increment(0x0120, 0x1234)
  }

  test("WIDERET") {
    bytecode(WIDE, RET, 0x01, 0x20)
    processor.ret(0x0120)
  }

  test("MONITORENTER") {
    bytecode(MONITORENTER)
    processor.monitorEnter()
  }

  test("MONITOREXIT") {
    bytecode(MONITOREXIT)
    processor.monitorExit()
  }

  test("LDC") {
    bytecode(LDC, 0xAA)
    processor.pushCPEntry(0xAA)
  }

  test("LDC_W") {
    bytecode(LDC_W, 0xAA, 0xBB)
    processor.pushCPEntry(0xAABB)
  }

  test("LDC2_W") {
    bytecode(LDC2_W, 0xAA, 0xBB)
    processor.pushCPEntry(0xAABB)
  }

  test("GETFIELD") {
    bytecode(GETFIELD, 0xAA, 0xBB)
    processor.fieldOp(0xAABB, FieldAccessKind.GETFIELD)
  }

  test("PUTFIELD") {
    bytecode(PUTFIELD, 0xAA, 0xBB)
    processor.fieldOp(0xAABB, FieldAccessKind.PUTFIELD)
  }

  test("GETSTATIC") {
    bytecode(GETSTATIC, 0xAA, 0xBB)
    processor.fieldOp(0xAABB, FieldAccessKind.GETSTATIC)
  }

  test("PUTSTATIC") {
    bytecode(PUTSTATIC, 0xAA, 0xBB)
    processor.fieldOp(0xAABB, FieldAccessKind.PUTSTATIC)
  }

  test("NEWARRAY") {
    bytecode(NEWARRAY, 6)
    processor.newPrimitiveArray(BytecodeTypeKind.FLOAT)
  }

  test("ANEWARRAY") {
    bytecode(ANEWARRAY, 0xAA, 0xBB)
    processor.newObjectArray(0xAABB)
  }

  test("MULTIANEWARRAY") {
    bytecode(MULTIANEWARRAY, 0xAA, 0xBB, 0x07)
    processor.newMultiObjectArray(0xAABB, 7)
  }

  test("ARRAYLENGTH") {
    bytecode(ARRAYLENGTH)
    processor.arrayLength()
  }

  test("BALOAD") {
    bytecode(BALOAD)
    processor.arrayGet(BytecodeTypeKind.BYTE)
  }

  test("SALOAD") {
    bytecode(SALOAD)
    processor.arrayGet(BytecodeTypeKind.SHORT)
  }

  test("CALOAD") {
    bytecode(CALOAD)
    processor.arrayGet(BytecodeTypeKind.CHAR)
  }

  test("FALOAD") {
    bytecode(FALOAD)
    processor.arrayGet(BytecodeTypeKind.FLOAT)
  }

  test("IALOAD") {
    bytecode(IALOAD)
    processor.arrayGet(BytecodeTypeKind.INT)
  }

  test("LALOAD") {
    bytecode(LALOAD)
    processor.arrayGet(BytecodeTypeKind.LONG)
  }

  test("DALOAD") {
    bytecode(DALOAD)
    processor.arrayGet(BytecodeTypeKind.DOUBLE)
  }

  test("AALOAD") {
    bytecode(AALOAD)
    processor.arrayGet(BytecodeTypeKind.CLASS)
  }

  test("BASTORE") {
    bytecode(BASTORE)
    processor.arrayPut(BytecodeTypeKind.BYTE)
  }

  test("SASTORE") {
    bytecode(SASTORE)
    processor.arrayPut(BytecodeTypeKind.SHORT)
  }

  test("CASTORE") {
    bytecode(CASTORE)
    processor.arrayPut(BytecodeTypeKind.CHAR)
  }

  test("FASTORE") {
    bytecode(FASTORE)
    processor.arrayPut(BytecodeTypeKind.FLOAT)
  }

  test("IASTORE") {
    bytecode(IASTORE)
    processor.arrayPut(BytecodeTypeKind.INT)
  }

  test("LASTORE") {
    bytecode(LASTORE)
    processor.arrayPut(BytecodeTypeKind.LONG)
  }

  test("DASTORE") {
    bytecode(DASTORE)
    processor.arrayPut(BytecodeTypeKind.DOUBLE)
  }

  test("AASTORE") {
    bytecode(AASTORE)
    processor.arrayPut(BytecodeTypeKind.CLASS)
  }

  test("NEW") {
    bytecode(NEW, 0xAA, 0xBB)
    processor.doNew(0xAABB)
  }

  test("INSTANCEOF") {
    bytecode(INSTANCEOF, 0xAA, 0xBB)
    processor.instanceOf(0xAABB)
  }

  test("CHECKCAST") {
    bytecode(CHECKCAST, 0xAA, 0xBB)
    processor.checkCast(0xAABB)
  }

  test("INVOKESTATIC") {
    bytecode(INVOKESTATIC, 0xAA, 0xBB)
    processor.invoke(0xAABB, MethodAccessKind.STATIC)
  }

  test("INVOKEVIRTUAL") {
    bytecode(INVOKEVIRTUAL, 0xAA, 0xBB)
    processor.invoke(0xAABB, MethodAccessKind.VIRTUAL)
  }

  test("INVOKESPECIAL") {
    bytecode(INVOKESPECIAL, 0xAA, 0xBB)
    processor.invoke(0xAABB, MethodAccessKind.SPECIAL)
  }

  test("INVOKEINTERFACE") {
    bytecode(INVOKEINTERFACE, 0xAA, 0xBB, 0x02, 0x00)
    processor.invoke(0xAABB, MethodAccessKind.INTERFACE)
  }

  test("ATHROW") {
    bytecode(ATHROW)
    processor.doThrow()
  }
}