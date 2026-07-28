/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.arm64

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.AsmError.{error, require}
import com.huawei.excelsior.jet.assembler.Width.*
import com.huawei.excelsior.jet.assembler.*
import com.huawei.excelsior.jet.assembler.arm64.ExtendMode.{UXTW, UXTX}
import com.huawei.excelsior.jet.assembler.arm64.MemAddrMode.*
import com.huawei.excelsior.jet.assembler.arm64.ShiftMode.{LSL, ROR}
import com.huawei.excelsior.jet.assembler.arm64.Enums.*
import com.huawei.excelsior.jet.assembler.arm64.IRegister.W.WZR
import com.huawei.excelsior.jet.assembler.arm64.IRegister.X.{SP, XZR}
import com.huawei.excelsior.jet.assembler.arm64.MemAtomicOp.nameModifier
import com.huawei.excelsior.jet.assembler.arm64.immediates.*
import xscala.util.MathUtils.*

/** Binary encoding of ARM64 instructions
  *
  * @author orangebyte256
  * @author paul
  */
private[arm64] object Bits {

  def nop = hint(0x0, 0)

  ////////////////////////////////////////////////////////////////////////////////
  // Addresses and constants calculation

  def adr(rd: IRegister.X, offset: Int) = {
    require(noSP(rd), "bad arguments for ADR Rd, label")
    pcRelativeAddressing(0, offset, rd)
  }

  // method is public only for test purpose DO NOT USE IT SOMEWHERE ELSE
  def adrp(rd: IRegister.X, offset: Int) = {
    require(noSP(rd), "bad arguments for ADRP Rd, label")
    pcRelativeAddressing(1, offset, rd)
  }

  def movk(rd: IRegister, imm: ShiftedImm16) = {
    require(noSP(rd) && imm != null, "bad arguments for MOVK Rd, imm16, shift")
    moveWideImmediate(3, imm, rd)
  }

  def movn(rd: IRegister, imm: ShiftedImm16) = {
    require(noSP(rd) && imm != null, "bad arguments for MOVN Rd, imm16, shift")
    moveWideImmediate(0, imm, rd)
  }

  def movz(rd: IRegister, imm: ShiftedImm16) = {
    require(noSP(rd) && imm != null, "bad arguments for MOVZ Rd, imm16, shift")
    moveWideImmediate(2, imm, rd)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Add and subtract

  def addSub(op: AddSubOp, rd: IRegister, rn: IRegister, imm: ShiftedImm12): Int = {
    val regsOk = (if (op.S == 1) noSP(rd) else noZR(rd)) && noZR(rn) && sameWidth(rd, rn)
    if (!regsOk || imm == null) {
      error(s"bad arguments for $op Rd, Rn, #imm")
    }
    addSubtractImmediate(op.op, op.S, imm, rn, rd)
  }

  def addSub(op: AddSubOp, rd: IRegister, rn: IRegister, imm: Int): Int = addSub(op, rd, rn, ShiftedImm12.encodeOrNull(imm))

  private def addSubSR(op: AddSubOp, rd: IRegister, rn: IRegister, rm: IRegister, mode: ShiftMode, shiftAmount: Int): Int = {
    if (!noSP(rd, rn) && sameWidth(rd, rn, rm) && (mode == LSL) && shiftAmount >= 0 && shiftAmount <= 4) {
      return addSubXR(op, rd, rn, rm, if (rd.width == W32) UXTW else UXTX, shiftAmount)
    }
    val regsOk = noSP(rd, rn, rm) && sameWidth(rd, rn, rm)
    if (!regsOk || (mode == ROR) || !isNBits(shiftAmount, log2(rd.width.nbits))) {
      error(s"bad arguments for $op Rd, Rn, Rm, {shift}")
    }
    addSubtractShiftedRegister(op.op, op.S, mode.encoding, rm, shiftAmount, rn, rd)
  }

  private def addSubXR(op: AddSubOp, rd: IRegister, rn: IRegister, rm: IRegister, mode: ExtendMode, shiftAmount: Int) = {
    val option = mode.encoding
    val regsOk = (if (op.S == 1) noSP(rd) else noZR(rd)) && noZR(rn) && noSP(rm) &&
      sameWidth(rd, rn) && rm.width <= rd.width
    val optionOk = (rd.width == W32) || ((option & 3) == 3) == (rm.width == W64)
    if (!regsOk || !optionOk || shiftAmount < 0 || shiftAmount > 4) {
      error(s"bad arguments for $op Rd, Rn, Rm, {extend}")
    }
    addSubtractExtendedRegister(op.op, op.S, 0, rm, option, shiftAmount, rn, rd)
  }

  def addSub(op: AddSubOp, rd: IRegister, rn: IRegister, rm: Arg.RArith) = rm match {
    case rm: IRegister =>
      addSubSR(op, rd, rn, rm, LSL, 0)
    case sr: Arg.ShiftedReg =>
      addSubSR(op, rd, rn, sr.rm, sr.mode, sr.amount)
    case xr: Arg.ExtendedReg =>
      addSubXR(op, rd, rn, xr.rm, xr.mode, xr.amount)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Logic instructions

  def logical(op: LogicalOp, rd: IRegister, rn: IRegister, imm: Long) = {
    val bitMaskImm = BitMaskImm.encodeOrNull(imm, rd.width)
    val regsOk = (if (op == LogicalOp.ANDS) noSP(rd) else noZR(rd)) && noSP(rn) && sameWidth(rd, rn)
    if (!regsOk || bitMaskImm == null) {
      error(s"bad arguments for ${op.name(false)} Rd, Rn, #imm")
    }
    logicalImmediate(op.opc, bitMaskImm, rn, rd)
  }

  private def logicalSR(op: LogicalOp, N: Boolean, rd: IRegister, rn: IRegister, rm: IRegister, mode: ShiftMode, shiftAmount: Int) = {
    val regsOk = noSP(rd, rn, rm) && sameWidth(rd, rn, rm)
    if (!regsOk || !isNBits(shiftAmount, log2(rd.width.nbits))) {
      error(s"bad arguments for ${op.name(N)} Rd, Rn, Rm, {shift}")
    }
    logicalShiftedRegister(op.opc, mode.encoding, if (N) 1 else 0, rm, shiftAmount, rn, rd)
  }

  def logical(op: LogicalOp, N: Boolean, rd: IRegister, rn: IRegister, rm: Arg.RLogical) = rm match {
    case rm: IRegister =>
      logicalSR(op, N, rd, rn, rm, LSL, 0)
    case sr: Arg.ShiftedReg =>
      logicalSR(op, N, rd, rn, sr.rm, sr.mode, sr.amount)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Condition select & compare

  def select(op: SelectOp, rd: IRegister, rn: IRegister, rm: IRegister, cond: CC) = {
    val ok = noSP(rd, rn, rm) && sameWidth(rd, rn, rm)
    if (!ok) {
      error(s"bad arguments for $op Rd, Rn, Rm, cond")
    }
    conditionalSelect(op.op, 0, rm, cond, op.op2, rn, rd)
  }

  def ccmp(rn: IRegister, rm: IRegister, nzcv: Int, cond: CC) = {
    val ok = noSP(rn, rm) && sameWidth(rn, rm) && isNBits(nzcv, 4)
    require(ok, "bad arguments for CCMP Rn, Rm, nzcv, cond")
    conditionalCompareRegister(1, 1, rm, cond, 0, rn, 0, nzcv)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Multiply & divide

  def udiv(rd: IRegister, rn: IRegister, rm: IRegister) = {
    require(noSP(rd, rn, rm) && sameWidth(rd, rn, rm), "bad arguments for UDIV Rd, Rn, Rm")
    dataProcessing2(0, rm, 0x02, rn, rd)
  }

  def sdiv(rd: IRegister, rn: IRegister, rm: IRegister) = {
    require(noSP(rd, rn, rm) && sameWidth(rd, rn, rm), "bad arguments for SDIV Rd, Rn, Rm")
    dataProcessing2(0, rm, 0x03, rn, rd)
  }

  def madd(rd: IRegister, rn: IRegister, rm: IRegister, ra: IRegister) = {
    require(noSP(rd, rn, rm, ra) && sameWidth(rd, rn, rm, ra), "bad arguments for MADD Rd, Rn, Rm, Ra")
    dataProcessing3(0, 0, rm, 0, ra, rn, rd)
  }

  def msub(rd: IRegister, rn: IRegister, rm: IRegister, ra: IRegister) = {
    require(noSP(rd, rn, rm, ra) && sameWidth(rd, rn, rm, ra), "bad arguments for MSUB Rd, Rn, Rm, Ra")
    dataProcessing3(0, 0, rm, 1, ra, rn, rd)
  }

  def smaddl(rd: IRegister.X, rn: IRegister.W, rm: IRegister.W, ra: IRegister.X) = {
    require(noSP(rd, rn, rm, ra), "bad arguments for SMADDL Rd, Rn, Rm, Ra")
    dataProcessing3(0, 1, rm, 0, ra, rn, rd)
  }

  def umaddl(rd: IRegister.X, rn: IRegister.W, rm: IRegister.W, ra: IRegister.X) = {
    require(noSP(rd, rn, rm, ra), "bad arguments for UMADDL Rd, Rn, Rm, Ra")
    dataProcessing3(0, 5, rm, 0, ra, rn, rd)
  }

  def smulh(rd: IRegister.X, rn: IRegister.X, rm: IRegister.X) = {
    require(noSP(rd, rn, rm), "bad arguments for SMULH Rd, Rn, Rm")
    dataProcessing3(0, 2, rm, 0, XZR, rn, rd)
  }

  def umulh(rd: IRegister.X, rn: IRegister.X, rm: IRegister.X) = {
    require(noSP(rd, rn, rm), "bad arguments for UMULH Rd, Rn, Rm")
    dataProcessing3(0, 6, rm, 0, XZR, rn, rd)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Other data-processing instructions

  def ubfm(rd: IRegister, rn: IRegister, immr: Int, imms: Int) = {
    require(noSP(rd, rn) && sameWidth(rd, rn) && isNBits(immr, 6) && isNBits(imms, 6), "bad arguments for UBFM Rd, Rn, immr, imms")
    bitfield(2, w64Bit(rd), immr, imms, rn, rd)
  }

  def sbfm(rd: IRegister, rn: IRegister, immr: Int, imms: Int) = {
    require(noSP(rd, rn) && sameWidth(rd, rn) && isNBits(immr, 6) && isNBits(imms, 6), "bad arguments for SBFM Rd, Rn, immr, imms")
    bitfield(0, w64Bit(rd), immr, imms, rn, rd)
  }

  def lslv(rd: IRegister, rn: IRegister, rm: IRegister) = {
    require(noSP(rd, rn, rm) && sameWidth(rd, rn, rm), "bad arguments for LSLV Rd, Rn, Rm")
    dataProcessing2(0, rm, 0x08, rn, rd)
  }

  def lsrv(rd: IRegister, rn: IRegister, rm: IRegister) = {
    require(noSP(rd, rn, rm) && sameWidth(rd, rn, rm), "bad arguments for LSRV Rd, Rn, Rm")
    dataProcessing2(0, rm, 0x09, rn, rd)
  }

  def asrv(rd: IRegister, rn: IRegister, rm: IRegister) = {
    require(noSP(rd, rn, rm) && sameWidth(rd, rn, rm), "bad arguments for ASRV Rd, Rn, Rm")
    dataProcessing2(0, rm, 0x0a, rn, rd)
  }

  def clz(rd: IRegister, rn: IRegister) = {
    require(noSP(rd, rn) && sameWidth(rd, rn), "bad arguments for CLZ Rd, Rn")
    dataProcessing1(0, 0x00, 0x04, rn, rd)
  }

  def rbit(rd: IRegister, rn: IRegister) = {
    require(noSP(rd, rn) && sameWidth(rd, rn), "bad arguments for RBIT Rd, Rn")
    dataProcessing1(0, 0x00, 0x00, rn, rd)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Instructions related to memory

  def dmb(option: DBOption) = barrier(option.encoding, 5, 0x1f)

  def ldrLiteral(rt: Register, offset: Int) = {
    require(noSP(rt), "bad arguments for LDR Rt, label")
    require(rt.width <= W64, "not supported width of Rt register") // TODO: support V regs
    loadLiteral(w64Bit(rt), fRegBit(rt), offset, rt)
  }

  def ldrswLiteral(rt: IRegister.X, offset: Int) = {
    require(noSP(rt), "bad arguments for LDRSW Rt, label")
    loadLiteral(2, 0, offset, rt)
  }

  private def validRegWidthForMemOp(rt: Register, width: Width) =
    (width == rt.width) || (width < rt.width && rt.isInstanceOf[IRegister.W])

  private def checkArgsForLoadStoreReg(op: MemOp, width: Width, rt: Register, mode: MemAddrMode, prfop: Int, modeOk: Boolean): Unit = {
    val ok = modeOk && (op match {
      case MemOp.PRFM =>
        isNBits(prfop, 5) && ((width == W64) && (mode != PRE_IDX) && (mode != POST_IDX))
      case MemOp.LDSX =>
        noSP(rt) && (width < rt.width && rt.isInstanceOf[IRegister])
      case MemOp.LD | MemOp.ST =>
        noSP(rt) && validRegWidthForMemOp(rt, width)
    })
    if (!ok) {
      val arg1 = if (op == MemOp.PRFM) "<prfop>" else "Rt"
      error(s"bad arguments for ${op.name("R", width)} $arg1, ${mode.text}")
    }
  }

  private def opc_loadStoreReg(op: MemOp, width: Width, rt: Register): Int = op match {
    case MemOp.ST => if (width == W128) 2 else 0
    case MemOp.LD => if (width == W128) 3 else 1
    case MemOp.LDSX => 3 ^ w64Bit(rt)
    case MemOp.PRFM => 2
  }

  private def loadStoreRegImm(op: MemOp, width0: Width, rt: Register, m: Arg.MemRI, prfop: Int) = {
    val width = if (width0 == WPTR) W64 else width0
    val uimm12 = m.offset >> width.log2bytes
    val useUImmFormat = (m.mode == REG_IMM) && isAligned(m.offset, width.nbytes) && isNBits(uimm12, 12)
    val immOk = useUImmFormat || isNBitsSigned(m.offset, 9)
    checkArgsForLoadStoreReg(op, width, rt, m.mode, prfop, noZR(m.rn) && immOk &&
      (op == MemOp.PRFM || !m.mode.isWBack || diffLocation(rt, m.rn)))

    val V = if (op != MemOp.PRFM) fRegBit(rt) else 0
    val size = width.log2bytes & 3
    val opc = opc_loadStoreReg(op, width, rt)
    val rtEnc = if (op != MemOp.PRFM) rt.encoding else prfop
    if (useUImmFormat) {
      loadStoreUnsignedImmediate(size, V, opc, uimm12, m.rn, rtEnc)
    } else {
      val mode = m.mode match {
        case PRE_IDX => 3
        case POST_IDX => 1
        case _ => 0
      }
      loadStoreSignedImmediate(size, V, opc, m.offset, mode, m.rn, rtEnc)
    }
  }

  private def loadStoreRegReg(op: MemOp, width0: Width, rt: Register, m: Arg.MemRR, prfop: Int) = {
    val width = if (width0 == WPTR) W64 else width0
    checkArgsForLoadStoreReg(op, width, rt, m.mode, prfop, noZR(m.rn) && noSP(m.rm))

    val V = if (op != MemOp.PRFM) fRegBit(rt) else 0
    val size = width.log2bytes & 3
    val opc = opc_loadStoreReg(op, width, rt)
    val option = (if (m.signExt) 6 else 2) | (if (m.rm.isInstanceOf[IRegister.X]) 1 else 0)
    val S = if (m.scaled) 1 else 0
    val rtEnc = if (op != MemOp.PRFM) rt.encoding else prfop
    loadStoreRegisterOffset(size, V, opc, m.rm, option, S, m.rn, rtEnc)
  }

  private def loadStoreReg(op: MemOp, width: Width, rt: Register, m: Arg.Mem, prfop: Int): Int = m match {
    case m: Arg.MemRR =>
      assert(m.mode == REG_REG)
      loadStoreRegReg(op, width, rt, m, prfop)
    case m: Arg.MemRI =>
      assert(m.mode != REG_REG)
      loadStoreRegImm(op, width, rt, m, prfop)
  }

  def loadStoreReg(op: MemOp, width: Width, rt: Register, m: Arg.Mem): Int = {
    assert(op != MemOp.PRFM)
    loadStoreReg(op, width, rt, m, 0)
  }

  def prfm(prfop: Int, m: Arg.Mem) = loadStoreReg(MemOp.PRFM, W64, null, m, prfop)

  def loadStorePair(op: MemOp, rt1: Register, rt2: Register, m: Arg.MemRI): Int = {
    assert(m.mode != UNSCALED)
    assert((op != MemOp.PRFM) && (op != MemOp.LDSX)) // LDPSW is not supported yet
    val imm7 = m.offset >> rt1.width.log2bytes

    val regsOk = noSP(rt2, rt1) && noZR(m.rn) && sameWidth(rt2, rt1) && fRegBit(rt2) == fRegBit(rt1) &&
      (op == MemOp.ST || diffLocation(rt1, rt2)) && (!m.mode.isWBack || (diffLocation(m.rn, rt1) && diffLocation(m.rn, rt2)))
    val offsetOk = isAligned(m.offset, rt1.width.nbytes) && isNBitsSigned(imm7, 7)
    if (!regsOk || !offsetOk) {
      error(s"bad arguments for ${op.name("P", rt1.width)} Rt1, Rt2, ${m.mode.text}")
    }

    val V = fRegBit(rt1)
    val higherBitsRt = rt1.width.nbytes >> 3
    val opc = higherBitsRt << (1 - V)
    val mode = m.mode match {
      case PRE_IDX => 3
      case POST_IDX => 1
      case _ => 2
    }
    loadStorePair(opc, V, mode, if (op == MemOp.ST) 0 else 1, bits(imm7, 0, 6), rt2, m.rn, rt1)
  }

  def loadStoreSpecial(op: MemOpX, width0: Width, rs: IRegister.W, rt: IRegister, rn: IRegister.X) = {
    val width = if (width0 == WPTR) W64 else width0
    val stx = !op.isLoad && op.exclusive
    val regsOk = (if (stx) noSP(rs) else rs == WZR) && noSP(rt) && noZR(rn) && validRegWidthForMemOp(rt, width) &&
      (!stx || (diffLocation(rs, rt) && diffLocation(rs, rn)))
    if (!regsOk) {
      error(s"bad arguments for $op${nameModifier(width)} ${if (stx) "Rs, " else ""}Rt, [Xn|SP]")
    }
    val size = width.log2bytes
    val o0 = if (op.ordered) 1 else 0
    val o2 = if (op.exclusive) 0 else 1
    val L = if (op.isLoad) 1 else 0
    loadStoreExclusive(size, o2, L, 0, rs, o0, getZR(rt), rn, rt)
  }

  def cas(width0: Width, rs: IRegister, rt: IRegister, rn: IRegister.X, ord: MemoryOrdering) = {
    val width = if (width0 == WPTR) W64 else width0
    val regsOk = noSP(rs, rt) && noZR(rn) && sameWidth(rs, rt) && validRegWidthForMemOp(rt, width)
    if (!regsOk) {
      error(s"bad arguments for CAS${nameModifier(ord, width)} Rs, Rt, [Xn|SP]")
    }
    val size = width.log2bytes
    loadStoreExclusive(size, 1, ord.a, 1, rs, ord.r, getZR(rt), rn, rt)
  }

  def memAtomic(op: MemAtomicOp, width0: Width, rs: IRegister, rt: IRegister, rn: IRegister.X, ord: MemoryOrdering) = {
    val width = if (width0 == WPTR) W64 else width0
    val regsOk = noSP(rs, rt) && noZR(rn) && sameWidth(rs, rt) && validRegWidthForMemOp(rt, width)
    if (!regsOk) {
      error(s"bad arguments for ${op.format(ord, width, rt)}")
    }
    val size = width.log2bytes
    atomicMemoryOp(size, 0, ord.a, ord.r, rs, op.o3, op.opc, rn, rt)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Instructions related to flow of execution instructions

  def b_bl(offset: Int, link: Boolean) = unconditionalBranchImmediate(if (link) 1 else 0, offset)

  def b_cond(cond: CC, offset: Int) = conditionalBranchImmediate(0, 0, offset, cond)

  def br(rn: IRegister.X) = {
    require(noSP(rn), "bad arguments for BR Rn")
    unconditionalBranchRegister(0x0, 0x1f, 0x00, rn, 0x00)
  }

  def blr(rn: IRegister.X) = {
    require(noSP(rn), "bad arguments for BLR Rn")
    unconditionalBranchRegister(0x1, 0x1f, 0x00, rn, 0x00)
  }

  def ret(rn: IRegister.X) = {
    require(noSP(rn), "bad arguments for RET Rn")
    unconditionalBranchRegister(0x2, 0x1f, 0x00, rn, 0x00)
  }

  def cb_z(nz: Boolean, rt: IRegister, offset: Int) = {
    if (!noSP(rt)) {
      error(s"bad arguments for ${(if (nz) "CBNZ" else "CBZ")} Rt, label")
    }
    compareBranchImmediate(if (nz) 1 else 0, offset, rt)
  }

  def tb_z(nz: Boolean, rt: IRegister, imm: Int, offset: Int) = {
    val ok = noSP(rt) && 0 <= imm && imm < rt.width.nbits
    if (!ok) {
      error(s"bad arguments for ${if (nz) "TBNZ" else "TBZ"} Rt, imm, label")
    }
    testBranchImmediate(if (nz) 1 else 0, imm, offset, rt)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Floating-point instructions

  private def fp1(opcode: Int, rn: VFPRegister, rd: VFPRegister) = fpDataProcessing1(0, 0, ftype(rn), opcode, rn, rd)

  def fcvtzs(rd: IRegister, rn: VFPRegister) = {
    require(noSP(rd), "bad arguments for FCVTZS Rd, Rn")
    convertFPAndI(w64Bit(rd), 0, ftype(rn), 3, 0, rn, rd)
  }

  def scvtf(rd: VFPRegister, rn: IRegister) = {
    require(noSP(rn), "bad arguments for SCVTF Rd, Rn")
    convertFPAndI(w64Bit(rn), 0, ftype(rd), 0, 2, rn, rd)
  }

  def ucvtf(rd: VFPRegister, rn: IRegister) = {
    require(noSP(rn), "bad arguments for UCVTF Rd, Rn")
    convertFPAndI(w64Bit(rn), 0, ftype(rd), 0, 3, rn, rd)
  }

  def fmov(rd: VFPRegister, imm: Double) = {
    val floatImm = FloatImm.encodeOrNull(imm, rd.width)
    require(floatImm != null, "bad arguments for FMOV Rd, imm")
    fpImmediate(0, 0, floatImm, 0x00, rd)
  }

  def fmov(rd: Register, rn: Register) = {
    require(noSP(rd, rn) && sameWidth(rd, rn), "bad arguments for FMOV Rd, Rn")
    (rd, rn) match {
      case (rd: VFPRegister, rn: IRegister) =>
        convertFPAndI(w64Bit(rn), 0, ftype(rd), 0, 7, rn, rd)
      case (rd: IRegister, rn: VFPRegister) =>
        convertFPAndI(w64Bit(rd), 0, ftype(rn), 0, 6, rn, rd)
      case (rd: VFPRegister, rn: VFPRegister) =>
        fp1(0x00, rn, rd)
      case _ =>
        shouldNotReachHere("both arguments of FMOV cannot be IReg")
    }
  }

  def fcvt(rd: VFPRegister, rn: VFPRegister) = {
    require(!sameWidth(rd, rn), "bad arguments for FCVT Rd, Rn")
    fp1(0x04 | ftype(rd), rn, rd)
  }

  def fabs(rd: VFPRegister, rn: VFPRegister) = {
    require(sameWidth(rd, rn), "bad arguments for FABS Rd, Rn")
    fp1(0x01, rn, rd)
  }

  def fneg(rd: VFPRegister, rn: VFPRegister) = {
    require(sameWidth(rd, rn), "bad arguments for FNEG Rd, Rn")
    fp1(0x02, rn, rd)
  }

  def fsqrt(rd: VFPRegister, rn: VFPRegister) = {
    require(sameWidth(rd, rn), "bad arguments for FSQRT Rd, Rn")
    fp1(0x03, rn, rd)
  }

  def frintz(rd: VFPRegister, rn: VFPRegister) = {
    require(sameWidth(rd, rn), "bad arguments for FRINTZ Rd, Rn")
    fp1(0x0b, rn, rd)
  }

  private def fcmp(withZero: Boolean, rn: VFPRegister, rm: Int) =
    fpCompare(0, 0, ftype(rn), rm, 0, rn, if (withZero) 0x08 else 0x00)

  def fcmp(rn: VFPRegister, rm: VFPRegister): Int = {
    require(sameWidth(rn, rm), "bad arguments for FCMP Rn, Rm")
    fcmp(false, rn, rm.encoding)
  }

  def fcmp(rn: VFPRegister, imm: Double): Int = {
    require(imm == 0.0, "bad arguments for FCMP Rn, 0.0")
    fcmp(true, rn, 0)
  }

  def fp2(op: FP2Op, rd: VFPRegister, rn: VFPRegister, rm: VFPRegister) = {
    val ok = sameWidth(rd, rn, rm)
    if (!ok) {
      error(s"bad arguments for $op Rd, Rn, Rm")
    }
    fpDataProcessing2(0, 0, rm, op.opcode, rn, rd)
  }

  def ins(rd: VFPRegister.V, elemWidth: Width, index: Int, rn: IRegister) = {
    val esize = elemWidth.nbytes
    val regsOk = elemWidth == rn.width || (elemWidth < rn.width && rn.isInstanceOf[IRegister.W])
    val indexOk = (index >= 0) && (index < 16) && (index * esize < 16)
    if (esize > 8 || !regsOk || !indexOk) {
      error(s"bad arguments for INS Vd.T[i], Rn")
    }
    val imm5 = ((index << 1) | 1) << elemWidth.log2bytes
    simdCopy(1, 0, imm5, 0x3, rn, rd)
  }

  def umov(rd: IRegister, rn: VFPRegister.V, elemWidth: Width, index: Int) = {
    val esize = elemWidth.nbytes
    val regsOk = elemWidth == rd.width || (elemWidth < rd.width && rd.isInstanceOf[IRegister.W])
    val indexOk = (index >= 0) && (index < 16) && (index * esize < 16)
    if (!(esize > 0 && esize <= 8) || !regsOk || !indexOk) {
      error(s"bad arguments for UMOV Rd, Vn.T[i]")
    }
    val imm5 = ((index << 1) | 1) << elemWidth.log2bytes
    simdCopy(if (elemWidth == W64) 1 else 0, 0, imm5, 0x7, rn, rd)
  }

  def cnt(rd: VFPRegister.V, rn: VFPRegister.V, vlen: Int) = {
    if (vlen != 8 && vlen != 16) {
      error(s"bad arguments for CNT Vd.T, Vn.T")
    }
    simdMisc(vlen >>> 4, 0, 0, 0x05, rn, rd)
  }

  def addv(rd: VFPRegister.V, dstWidth: Width, rn: VFPRegister.V, vlen: Int) = {
    val dataSize = dstWidth.nbytes * vlen
    val widthOk = dstWidth == W8 || dstWidth == W16 || dstWidth == W32
    if (!widthOk || (vlen < 4) || !(dataSize == 8 || dataSize == 16)) {
      error(s"bad arguments for ADDV Vd, Vn.T")
    }
    simdAcrossLanes(dataSize >>> 4, 0, dstWidth.log2bytes, 0x1b, rn, rd)
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Utils

  private def w64Bit(reg: Register) = (reg.width: @unchecked) match {
    case W64 => 1
    case W32 => 0
  }

  private def ftype(reg: VFPRegister) = (reg.width: @unchecked) match {
    case W16 => 3
    case W32 => 0
    case W64 => 1
  }

  private def fRegBit(reg: Register) = reg match {
    case _: VFPRegister => 1
    case _ => 0
  }

  private def diffLocation(r1: Register, r2: Register) = r1.as(W64) != r2.as(W64)

  private def sameWidth(r1: Register, r2: Register): Boolean = r1.width == r2.width

  private def sameWidth(r1: Register, r2: Register, r3: Register): Boolean =
    sameWidth(r1, r2) && sameWidth(r2, r3)

  private def sameWidth(r1: Register, r2: Register, r3: Register, r4: Register): Boolean =
    sameWidth(r1, r2, r3) && sameWidth(r3, r4)

  /** Return zero register depending on reg's width. */
  def getZR(reg: IRegister): IRegister = if (reg.width == W32) WZR else XZR

  def isZR(r1: Register): Boolean = (r1 == WZR) || (r1 == XZR)

  def noZR(r1: Register): Boolean = !isZR(r1)

  def noSP(r1: Register): Boolean = r1 != SP

  def noSP(r1: Register, r2: Register): Boolean = noSP(r1) && noSP(r2)

  def noSP(r1: Register, r2: Register, r3: Register): Boolean = noSP(r1, r2) && noSP(r3)

  def noSP(r1: Register, r2: Register, r3: Register, r4: Register): Boolean = noSP(r1, r2, r3) && noSP(r4)

  ////////////////////////////////////////////////////////////////////////////////
  // Instruction encoders

  ///////////////////////////////////////////////////////
  // region C4.1.2    Data Processing - Immediate

  // PC-rel. addressing
  private def pcRelativeAddressing(op: Int, offset: Int, rd: IRegister): Int = {
    assert(isNBits(op, 1))
    assert(isNBitsSigned(offset, 21))
    val immlo = bits(offset, 0, 1)
    val immhi = bits(offset, 2, 20)
    val pattern = 0x10_00_00_00
    pattern | op << 31 | immlo << 29 | immhi << 5 | rd.encoding
  }

  // Add/subtract (immediate)
  private def addSubtractImmediate(op: Int, S: Int, imm: ShiftedImm12, rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(op, 1))
    assert(isNBits(S, 1))
    val pattern = 0x11_00_00_00
    pattern | w64Bit(rd) << 31 | op << 30 | S << 29 | imm.shift << 22 | imm.imm12 << 10 |
      rn.encoding << 5 | rd.encoding
  }

  // Add/subtract (immediate, with tags) -- not implemented

  // Logical (immediate)
  private def logicalImmediate(opc: Int, imm: BitMaskImm, rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(opc, 2))
    val pattern = 0x12_00_00_00
    pattern | w64Bit(rd) << 31 | opc << 29 | imm.N << 22 | imm.immr << 16 | imm.imms << 10 |
      rn.encoding << 5 | rd.encoding
  }

  // Move wide (immediate)
  private def moveWideImmediate(opc: Int, imm: ShiftedImm16, rd: Register): Int = {
    assert(isNBits(opc, 2))
    val pattern = 0x12_80_00_00
    pattern | w64Bit(rd) << 31 | opc << 29 | imm.hw << 21 | imm.imm16 << 5 | rd.encoding
  }

  // Bitfield
  private def bitfield(opc: Int, N: Int, immr: Int, imms: Int, rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(opc, 2))
    assert(isNBits(N, 1))
    assert(isNBits(immr, 6))
    assert(isNBits(imms, 6))
    val pattern = 0x13_00_00_00
    pattern | w64Bit(rd) << 31 | opc << 29 | N << 22 | immr << 16 | imms << 10 | rn.encoding << 5 | rd.encoding
  }

  // Extract -- not implemented
  // endregion

  ///////////////////////////////////////////////////////
  // region C4.1.3    Branches, Exception Generating and System instructions

  // Hints
  private def hint(CRm: Int, op2: Int) = system(0, 0, 3, 0x2, CRm, op2, 0x1f)

  // Barriers
  private def barrier(CRm: Int, op2: Int, rt: Int) = system(0, 0, 3, 0x3, CRm, op2, rt)

  // System instructions, including:
  //  - Exception generation, Hints, Barriers, PSTATE, System register move
  private def system(L: Int, op0: Int, op1: Int, CRn: Int, CRm: Int, op2: Int, rt: Int): Int = {
    assert(isNBits(L, 1))
    assert(isNBits(op0, 2))
    assert(isNBits(op1, 3))
    assert(isNBits(CRn, 4))
    assert(isNBits(CRm, 4))
    assert(isNBits(op2, 3))
    assert(isNBits(rt, 5))
    val pattern = 0xd5_00_00_00
    pattern | L << 21 | op0 << 19 | op1 << 16 | CRn << 12 | CRm << 8 | op2 << 5 | rt
  }

  // Unconditional branch (register)
  private def unconditionalBranchRegister(opc: Int, op2: Int, op3: Int, rn: Register, op4: Int): Int = {
    assert(isNBits(opc, 4))
    assert(isNBits(op2, 5))
    assert(isNBits(op3, 6))
    assert(isNBits(op4, 5))
    val pattern = 0xd6_00_00_00
    pattern | opc << 21 | op2 << 16 | op3 << 10 | rn.encoding << 5 | op4
  }

  // Unconditional branch (immediate))
  private def unconditionalBranchImmediate(op: Int, offset: Int): Int = {
    assert(isNBits(op, 1))
    assert(isNBitsSigned(offset, 28) && isAligned(offset, 4))
    val pattern = 0x14_00_00_00
    pattern | op << 31 | bits(offset, 2, 27)
  }

  // Conditional branch (immediate)
  private def conditionalBranchImmediate(o1: Int, o0: Int, offset: Int, cond: CC): Int = {
    assert(isNBits(o1, 1))
    assert(isNBits(o0, 1))
    assert(isNBitsSigned(offset, 21) && isAligned(offset, 4))
    val pattern = 0x54_00_00_00
    pattern | o1 << 24 | bits(offset, 2, 20) << 5 | o0 << 4 | cond.encoding
  }

  // Compare and branch (immediate)
  private def compareBranchImmediate(op: Int, offset: Int, rt: Register): Int = {
    assert(isNBits(op, 1))
    assert(isNBitsSigned(offset, 21) && isAligned(offset, 4))
    val pattern = 0x34_00_00_00
    pattern | w64Bit(rt) << 31 | op << 24 | bits(offset, 2, 20) << 5 | rt.encoding
  }

  // Test and branch (immediate)
  private def testBranchImmediate(op: Int, imm: Int, offset: Int, rt: Register): Int = {
    assert(isNBits(op, 1))
    assert(isNBits(imm, 6))
    assert(isNBitsSigned(offset, 16) && isAligned(offset, 4))
    val b5 = bit(imm, 5)
    val b40 = bits(imm, 0, 4)
    val pattern = 0x36_00_00_00
    pattern | b5 << 31 | op << 24 | b40 << 19 | bits(offset, 2, 15) << 5 | rt.encoding
  }
  // endregion

  ///////////////////////////////////////////////////////
  // region C4.1.4 Loads and stores

  // Load register (literal)
  private def loadLiteral(opc: Int, V: Int, offset: Int, rt: Register): Int = {
    assert(isNBits(opc, 2))
    assert(isNBits(V, 1))
    assert(isNBitsSigned(offset, 21) && isAligned(offset, 4))
    val pattern = 0x18_00_00_00
    pattern | opc << 30 | V << 26 | bits(offset, 2, 20) << 5 | rt.encoding
  }

  // Load/store register pair (post-indexed, pre-indexed, offset, no-allocate offset)
  private def loadStorePair(opc: Int, V: Int, mode: Int, L: Int, imm7: Int, rt2: Register,
                            rn: IRegister, rt: Register): Int = {
    assert(isNBits(opc, 2))
    assert(isNBits(V, 1))
    assert(isNBits(mode, 2))
    assert(isNBits(L, 1))
    assert(isNBits(imm7, 7))
    val pattern = 0x28_00_00_00
    pattern | opc << 30 | V << 26 | mode << 23 | L << 22 | imm7 << 15 |
      rt2.encoding << 10 | rn.encoding << 5 | rt.encoding
  }

  // Load/store register (unsigned immediate)
  private def loadStoreUnsignedImmediate(size: Int, V: Int, opc: Int, imm12: Int,
                                         rn: IRegister, rt: Int): Int = {
    assert(isNBits(size, 2))
    assert(isNBits(V, 1))
    assert(isNBits(opc, 2))
    assert(isNBits(imm12, 12))
    assert(isNBits(rt, 5))
    val pattern = 0x39_00_00_00
    pattern | size << 30 | V << 26 | opc << 22 | imm12 << 10 | rn.encoding << 5 | rt
  }

  // Load/store register (unscaled immediate, immediade post-indexed, immediate pre-indexed)
  private def loadStoreSignedImmediate(size: Int, V: Int, opc: Int, simm: Int, mode: Int,
                                       rn: IRegister, rt: Int): Int = {
    assert(isNBits(size, 2))
    assert(isNBits(V, 1))
    assert(isNBits(opc, 2))
    assert(isNBitsSigned(simm, 9))
    assert(isNBits(mode, 2))
    assert(isNBits(rt, 5))
    val imm9 = bits(simm, 0, 8)
    val pattern = 0x38_00_00_00
    pattern | size << 30 | V << 26 | opc << 22 | imm9 << 12 | mode << 10 | rn.encoding << 5 | rt
  }

  // Load/store register (register offset)
  private def loadStoreRegisterOffset(size: Int, V: Int, opc: Int, rm: IRegister, option: Int, S: Int,
                                      rn: IRegister, rt: Int): Int = {
    assert(isNBits(size, 2))
    assert(isNBits(V, 1))
    assert(isNBits(opc, 2))
    assert(isNBits(option, 3))
    assert(isNBits(S, 1))
    assert(isNBits(rt, 5))
    val pattern = 0x38_20_08_00
    pattern | size << 30 | V << 26 | opc << 22 | rm.encoding << 16 | option << 13 | S << 12 | rn.encoding << 5 | rt
  }

  // Atomic memory operations
  private def atomicMemoryOp(size: Int, V: Int, A: Int, R: Int, rs: IRegister, o3: Int, opc: Int,
                             rn: IRegister, rt: IRegister): Int = {
    assert(isNBits(size, 2))
    assert(isNBits(V, 1))
    assert(isNBits(A, 1))
    assert(isNBits(R, 1))
    assert(isNBits(o3, 1))
    assert(isNBits(opc, 3))
    val pattern = 0x38_20_00_00
    pattern | size << 30 | V << 26 | A << 23 | R << 22 | rs.encoding << 16 |
      o3 << 15 | opc << 12 | rn.encoding << 5 | rt.encoding
  }

  // Load/store exclusive
  private def loadStoreExclusive(size: Int, o2: Int, L: Int, o1: Int, rs: IRegister, o0: Int, rt2: IRegister,
                                 rn: IRegister, rt: IRegister): Int = {
    assert(isNBits(size, 2))
    assert(isNBits(o2, 1))
    assert(isNBits(L, 1))
    assert(isNBits(o1, 1))
    assert(isNBits(o0, 1))
    val pattern = 0x08_00_00_00
    pattern | size << 30 | o2 << 23 | L << 22 | o1 << 21 | rs.encoding << 16 |
      o0 << 15 | rt2.encoding << 10 | rn.encoding << 5 | rt.encoding
  }
  // endregion

  ///////////////////////////////////////////////////////
  // region C4.1.5 Data-processing (register)

  // Data-processing (1 source)
  private def dataProcessing1(S: Int, opcode2: Int, opcode: Int, rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(S, 1))
    assert(isNBits(opcode2, 5))
    assert(isNBits(opcode, 6))
    val pattern = 0x5a_c0_00_00
    pattern | w64Bit(rd) << 31 | S << 29 | opcode2 << 16 | opcode << 10 | rn.encoding << 5 | rd.encoding
  }

  // Data-processing (2 source)
  private def dataProcessing2(S: Int, rm: IRegister, opcode: Int, rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(S, 1))
    assert(isNBits(opcode, 6))
    val pattern = 0x1a_c0_00_00
    pattern | w64Bit(rd) << 31 | S << 29 | rm.encoding << 16 | opcode << 10 | rn.encoding << 5 | rd.encoding
  }

  // Data-processing (3 source)
  private def dataProcessing3(op54: Int, op31: Int, rm: IRegister, o0: Int,
                              ra: IRegister, rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(op54, 2))
    assert(isNBits(op31, 3))
    assert(isNBits(o0, 1))
    val pattern = 0x1b_00_00_00
    pattern | w64Bit(rd) << 31 | op54 << 29 | op31 << 21 | rm.encoding << 16 |
      o0 << 15 | ra.encoding << 10 | rn.encoding << 5 | rd.encoding
  }

  // Logical (shifted register)
  private def logicalShiftedRegister(opc: Int, shift: Int, N: Int, rm: Register, imm6: Int,
                                     rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(opc, 2))
    assert(isNBits(shift, 2))
    assert(isNBits(N, 1))
    assert(isNBits(imm6, 6))
    val pattern = 0x0a_00_00_00
    pattern | w64Bit(rd) << 31 | opc << 29 | shift << 22 | N << 21 | rm.encoding << 16 |
      imm6 << 10 | rn.encoding << 5 | rd.encoding
  }

  // Add/subtract (shifted register)
  private def addSubtractShiftedRegister(op: Int, S: Int, shift: Int, rm: Register, imm6: Int,
                                         rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(op, 1))
    assert(isNBits(S, 1))
    assert(isNBits(shift, 2))
    assert(isNBits(imm6, 6))
    val pattern = 0x0b_00_00_00
    pattern | w64Bit(rd) << 31 | op << 30 | S << 29 | shift << 22 | rm.encoding << 16 |
      imm6 << 10 | rn.encoding << 5 | rd.encoding
  }

  // Add/subtract (extended register)
  private def addSubtractExtendedRegister(op: Int, S: Int, opt: Int, rm: IRegister, option: Int, imm3: Int,
                                          rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(op, 1))
    assert(isNBits(S, 1))
    assert(isNBits(opt, 2))
    assert(isNBits(option, 3))
    assert(isNBits(imm3, 3))
    val pattern = 0x0b_20_00_00
    pattern | w64Bit(rd) << 31 | op << 30 | S << 29 | opt << 22 | rm.encoding << 16 |
      option << 13 | imm3 << 10 | rn.encoding << 5 | rd.encoding
  }

  // Add/subtract with carry -- not implemented

  // Conditional compare (register)
  private def conditionalCompareRegister(op: Int, S: Int, rm: IRegister, cond: CC, o2: Int,
                                         rn: IRegister, o3: Int, nzcv: Int): Int = {
    assert(isNBits(op, 1))
    assert(isNBits(S, 1))
    assert(isNBits(o2, 1))
    assert(isNBits(o3, 1))
    assert(isNBits(nzcv, 4))
    val pattern = 0x1a_40_00_00
    pattern | w64Bit(rm) << 31 | op << 30 | S << 29 | rm.encoding << 16 |
      cond.encoding << 12 | o2 << 10 | rn.encoding << 5 | o3 << 4 | nzcv
  }

  // Conditional compare (immediate) -- not implemented

  // Conditional select
  private def conditionalSelect(op: Int, S: Int, rm: IRegister, cond: CC, op2: Int,
                                rn: IRegister, rd: IRegister): Int = {
    assert(isNBits(op, 1))
    assert(isNBits(S, 1))
    assert(isNBits(op2, 2))
    val pattern = 0x1a_80_00_00
    pattern | w64Bit(rd) << 31 | op << 30 | S << 29 | rm.encoding << 16 |
      cond.encoding << 12 | op2 << 10 | rn.encoding << 5 | rd.encoding
  }
  // endregion

  ///////////////////////////////////////////////////////
  // region C4.1.6    Data Processing - Scalar Floating-Point and Advanced SIMD

  // Conversion between floating-point and integer
  private def convertFPAndI(sf: Int, S: Int, ptype: Int, rmode: Int, opcode: Int, rn: Register, rd: Register): Int = {
    assert(isNBits(sf, 1))
    assert(isNBits(S, 1))
    assert(isNBits(ptype, 2))
    assert(isNBits(rmode, 2))
    assert(isNBits(opcode, 3))
    val pattern = 0x1e_20_00_00
    pattern | sf << 31 | S << 29 | ptype << 22 | rmode << 19 | opcode << 16 | rn.encoding << 5 | rd.encoding
  }

  // Floating-point data-processing (1 source)
  private def fpDataProcessing1(M: Int, S: Int, ptype: Int, opcode: Int, rn: VFPRegister, rd: VFPRegister): Int = {
    assert(isNBits(M, 1))
    assert(isNBits(S, 1))
    assert(isNBits(ptype, 2))
    assert(isNBits(opcode, 6))
    val pattern = 0x1e_20_40_00
    pattern | M << 31 | S << 29 | ptype << 22 | opcode << 15 | rn.encoding << 5 | rd.encoding
  }

  // Floating-point compare
  private def fpCompare(M: Int, S: Int, ptype: Int, rm: Int, op: Int, rn: VFPRegister, opcode2: Int): Int = {
    assert(isNBits(M, 1))
    assert(isNBits(S, 1))
    assert(isNBits(ptype, 2))
    assert(isNBits(rm, 5))
    assert(isNBits(op, 2))
    assert(isNBits(opcode2, 5))
    val pattern = 0x1e_20_20_00
    pattern | M << 31 | S << 29 | ptype << 22 | rm << 16 | op << 14 | rn.encoding << 5 | opcode2
  }

  // Floating-point immediate
  private def fpImmediate(M: Int, S: Int, imm: FloatImm, imm5: Int, rd: VFPRegister): Int = {
    assert(isNBits(M, 1))
    assert(isNBits(S, 1))
    assert(isNBits(imm5, 5))
    val pattern = 0x1e_20_10_00
    pattern | M << 31 | S << 29 | ftype(rd) << 22 | imm.imm8 << 13 | imm5 << 5 | rd.encoding
  }

  // Floating-point conditional compare -- not implemented

  // Floating-point data-processing (2 source)
  private def fpDataProcessing2(M: Int, S: Int, rm: VFPRegister, opcode: Int, rn: VFPRegister, rd: VFPRegister): Int = {
    assert(isNBits(M, 1))
    assert(isNBits(S, 1))
    assert(isNBits(opcode, 4))
    val pattern = 0x1e_20_08_00
    pattern | M << 31 | S << 29 | ftype(rd) << 22 | rm.encoding << 16 |
      opcode << 12 | rn.encoding << 5 | rd.encoding
  }

  // Floating-point conditional select -- not implemented

  // Floating-point data-processing (3 source) -- not implemented

  // Advanced SIMD copy
  private def simdCopy(Q: Int, op: Int, imm5: Int, imm4: Int, rn: Register, rd: Register): Int = {
    assert(isNBits(Q, 1))
    assert(isNBits(op, 1))
    assert(isNBits(imm5, 5))
    assert(isNBits(imm4, 4))
    val pattern = 0x0e_00_04_00
    pattern | Q << 30 | op << 29 | imm5 << 16 | imm4 << 11 | rn.encoding << 5 | rd.encoding
  }

  // Advanced SIMD two-register miscellaneous
  private def simdMisc(Q: Int, U: Int, size: Int, opcode: Int, rn: VFPRegister, rd: VFPRegister): Int = {
    assert(isNBits(Q, 1))
    assert(isNBits(U, 1))
    assert(isNBits(size, 2))
    assert(isNBits(opcode, 5))
    val pattern = 0x0e_20_08_00
    pattern | Q << 30 | U << 29 | size << 22 | opcode << 12 | rn.encoding << 5 | rd.encoding
  }

  // Advanced SIMD across lanes
  private def simdAcrossLanes(Q: Int, U: Int, size: Int, opcode: Int, rn: VFPRegister, rd: VFPRegister): Int = {
    assert(isNBits(Q, 1))
    assert(isNBits(U, 1))
    assert(isNBits(size, 2))
    assert(isNBits(opcode, 5))
    val pattern = 0x0e_30_08_00
    pattern | Q << 30 | U << 29 | size << 22 | opcode << 12 | rn.encoding << 5 | rd.encoding
  }
  // endregion
}