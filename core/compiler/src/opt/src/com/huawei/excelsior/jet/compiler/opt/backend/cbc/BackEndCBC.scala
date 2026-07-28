/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend.cbc

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.cbc.Assembler
import com.huawei.excelsior.jet.assembler.cbc.Register.IR.{IR1, IR2, IR7}
import com.huawei.excelsior.jet.compiler.abi.XTableGenerator
import com.huawei.excelsior.jet.compiler.cbc.CBCFileGenerator
import com.huawei.excelsior.jet.compiler.opt.backend.BackEnd
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.FrameComponentCBC.{OHMSlotCBC, TypedFrameSlotCBC}
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.bgcm.PreferredCBC
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.codegen.CodeGeneratorCBC
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.post.PostProcessComponentCBC
import com.huawei.excelsior.jet.compiler.opt.backend.cbc.preparation.PreparationCBC
import com.huawei.excelsior.jet.compiler.opt.backend.codegen.{Code, CodeCBC}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.setOf
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.options.BoolOption.{PrintDeltaMaps, UseIsa12}
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, SignatureType}
import xscala.util.MathUtils
import xscala.util.MathUtils.{isNBits, isNBitsSigned}

import scala.PartialFunction.condOpt

trait BackEndCBC
  extends BackEnd
    with FrameComponentCBC
    with NodesDescriptionCBC
    with MachineDescriptionCBC
    with CodeGeneratorCBC
    with PreparationCBC
    with PreferredCBC
    with PostProcessComponentCBC { self: Universe =>

  private var codegen: CodeGeneratorImplCBC = _

  val Isa12Mode: Boolean = env.enabled(UseIsa12)

  val ir1Set = setOf(IR1)
  val ir2Set = setOf(IR2)
  val ir7Set = setOf(IR7)
  val stdTmp1StdTmp2Set = ir1Set | ir2Set
  val allParamIRegsExceptStdTmp1AndStdTmp2 = allParamIRegsSet &~ stdTmp1StdTmp2Set
  val allParamIRegsExceptIR7 = allParamIRegsSet &~ ir7Set

  val volatileIRegsSet = setOf(frame.abi.volatileIRegs)
  val volatileFRegsSet = setOf(frame.abi.volatileFRegs)

  def volatileSet(file: RegFile) = file match {
    case RegFile.IREG => volatileIRegsSet
    case RegFile.FREG => volatileFRegsSet
  }

  object Imm4   { def unapply(n: Node): Option[Int] = condOpt(n) { case IntegralConst(c) if isNBitsSigned(c, 4)  => c.toInt } }
  object Imm8   { def unapply(n: Node): Option[Int] = condOpt(n) { case IntegralConst(c) if isNBitsSigned(c, 8)  => c.toInt } }
  object Imm32  { def unapply(n: Node): Option[Int] = condOpt(n) { case IntegralConst(c) if isNBitsSigned(c, 32) => c.toInt } }

  object CmpAnyInstanceOf {
    def unapply(cmp: Cmp): Option[(Node, ClassType, Node)] = condOpt(cmp) {
      case ZeroComparison(n @ AnyInstanceOf(tpe, obj)) => (n, tpe, obj)
    }
  }

  override protected def makeCodeGeneratorImpl() = {
    assert(codegen == null)
    codegen = new CodeGeneratorImplCBC
    codegen
  }

  override def sendCode(code: Code): Unit = {
    val CodeCBC(segment, xinfo, exTable, liveness) = code

    if (env.enabled(PrintDeltaMaps)) {
      env.reportDeltaMaps(codeUnit, xinfo)
    }

    val xgen = new XTableGenerator(codeUnit.method, rtOffset)(env)
    val packedXInfo = xgen.packXInfo(xinfo, markedRegions = Seq.empty)

    val stackAllocatedTypeSigs = all[StackAlloc].map(_.slot).collect {
      case x: TypedFrameSlotCBC => x
    }.toArray.sortBy(_.index).map(_.tpe).toSeq

    val variablesSizeTypes = all[StackAlloc].map(_.slot).collect {
      case x: OHMSlotCBC => x
    }.toArray.sortBy(_.index).map(_.kind.allocType).toSeq

    val literalsOffset = asm match {
      case a: Assembler => a.literalsStart.position
      case _ => 0 // literals offset is not needed for new isa
    }

    CBCFileGenerator.sendCode(
      codeUnit.method, segment, literalsOffset, packedXInfo, exTable, liveness,
      tailParamCount, untypedStackSlotsCount,
      usedNonVolIRegsMask, usedNonVolFRegsMask, maxCalleeStackArgsCount,
      codegen.mayHaveNativeCalls, stackAllocatedTypeSigs, variablesSizeTypes)
    ensureNoDataSegments()
  }
}
