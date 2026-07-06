/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

import com.huawei.excelsior.common.Environment.{JC_STANDALONE, TARGET_CPU_ARCH, TARGET_OS}
import com.huawei.excelsior.common.LanguagePack
import com.huawei.excelsior.jet.compiler.abi.cbc.PlatformCBC
import com.huawei.excelsior.jet.compiler.cangjie.CangjieEnumInfo
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.{SpecialParamSet, SpecialParameter}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParameter.*
import com.huawei.excelsior.jet.compiler.symlevel.{MethodSignature, SignatureType}
import com.huawei.excelsior.jet.compiler.{CompilerSuite, Env, TypeProvider}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeType
import com.huawei.excelsior.jet.compiler.types.{ReferenceTypes, TypesToolbox}

import scala.collection.mutable

class ABISignatureSuite extends CompilerSuite with TypesToolbox {

  implicit def typeProvider: TypeProvider = env.getTypeProvider

  private val syms = mutable.ArrayBuffer.empty[FakeType]

  override def beforeEach(): Unit = {
    super.beforeEach()
    env.registerFake(syms.toSeq: _*)
    Env.setUnitTestsEnv()
    Env.init(PlatformCBC(TARGET_OS, TARGET_CPU_ARCH, JC_STANDALONE), isJIT = false, isWorkMode = true, isDynamicBundle = false, LanguagePack.CANGJIE, isStandalone = true)
  }

  def ctv(idx: Int) = ClassTypeVariable(idx)
  def ltv(idx: Int) = LocalTypeVariable(idx)

  // TODO: consider moving to TypesToolbox
  val coreObject = {
    val sym = makeSymClass("std.core:Object", null)
    syms += sym
    sym.markAsCangjieType()
    SignatureType.CangjieReference(sym)
  }

  val rec = {
    val sym = makeSymRecord("Rec")
    syms += sym
    makeSymField("f0", Int64, sym)
    makeSymField("f1", coreObject, sym)
    SignatureType.Record(sym)
  }

  val recFST = {
    val sym = makeSymRecord("RecFST")
    syms += sym
    makeSymField("f0", Int64, sym)
    makeSymField("f1", coreObject, sym)
    SignatureType.InstantiatedRecord(sym.getName, Seq(ctv(0)))
  }

  val recFSTInt = recFST.instantiate(Seq(Int64), Seq.empty)

  val recVST = {
    val sym = makeSymRecord("RecVST")
    syms += sym
    makeSymField("f0", Int64, sym)
    makeSymField("f1", coreObject, sym)
    makeSymField("f2", ctv(0), sym)
    SignatureType.InstantiatedRecord(sym.getName, Seq(ctv(0)))
  }

  val recVSTInt = recVST.instantiate(Seq(Int64), Seq.empty)

  val coreOption = {
    val sym = makeSymClass("std.core:Option", null)
    syms += sym
    sym.setCangjieEnumInfo(CangjieEnumInfo(Seq(
      CangjieEnumInfo.Constructor(Seq(ctv(0))), // some
      CangjieEnumInfo.Constructor(Seq())        // none
    )))
    OptionLikeEnum(sym.getName, Seq(ctv(0)), ctv(0))
  }

  val coreObjectOpt = coreOption.instantiate(Seq(coreObject), Seq.empty)
  val recFSTOpt = coreOption.instantiate(Seq(recFST), Seq.empty)
  val recFSTIntOpt = coreOption.instantiate(Seq(recFSTInt), Seq.empty)
  val recVSTOpt = coreOption.instantiate(Seq(recVST), Seq.empty)
  val recVSTIntOpt = coreOption.instantiate(Seq(recVSTInt), Seq.empty)

  for (
    ((sig, abiSig), pos) <- Seq(
      tp(CPointer(Int64), CPointer(Int64)),
      tp(ltv(0), Box(ltv(0))),
      tp(ctv(0), Box(ctv(0))),
      tp(coreObject, coreObject),
      tp(rec, rec),
      tp(recFST, recFST),
      tp(recFSTInt, recFSTInt),
      tp(recVST, Box(recVST)),
      tp(recVSTInt, recVSTInt),
      tp(coreOption, Box(coreOption)),
      tp(coreObjectOpt, coreObjectOpt),
      tp(recFSTOpt, recFSTOpt),
      tp(recFSTIntOpt, recFSTIntOpt),
      tp(recVSTOpt, Box(recVSTOpt)),
      tp(recVSTIntOpt, recVSTIntOpt),
    ) ++ Primitive.values.map(x => tp(x, x))
  ) {
    test(s"makeABISigType($sig)") {
      ABI.makeABISigType(sig) should be (abiSig)
    }
  }

  for (
    ((retType,  receiver,         hasMutParameter, hasThisTypeInfoParameter, isCFunc, hasOuterTypeInfo, genericParamsCount, abiParams, specialParams), pos) <- Seq(
      tp(Int64, None,             false,           false,                    false,   false,            0,
        Seq(),
        Seq()),
      tp(Int64, Some(coreObject), false,           false,                    false,   false,            0,
        Seq(coreObject),
        Seq(Receiver)),

      // Receiver
      tp(Int64, Some(recFST),     false,           false,                    false,   false,            0,
        Seq(recFST),
        Seq(Receiver)),
      tp(Int64, Some(recFST),     false,           true,                     false,   false,            0,
        Seq(recFST, Address),
        Seq(Receiver, SpecialParameter.ThisTypeInfo)),
      tp(Int64, Some(recFST),     false,           false,                    false,   true,             0,
        Seq(recFST, Address),
        Seq(Receiver, OuterTypeInfo)),
      tp(Int64, Some(recFST),     false,           true,                     false,   true,             0,
        Seq(recFST, Address, Address),
        Seq(Receiver, SpecialParameter.ThisTypeInfo, OuterTypeInfo)),
      tp(Int64, Some(recFST),     false,           true,                     false,   false,            2,
        Seq(recFST, Address, Address, Address),
        Seq(Receiver, GenericFuncParams, SpecialParameter.ThisTypeInfo)),
      tp(Int64, Some(recFST),     false,           false,                    false,   true,             2,
        Seq(recFST, Address, Address, Address),
        Seq(Receiver, GenericFuncParams, OuterTypeInfo)),
      tp(Int64, Some(recFST),     false,           true,                     false,   true,             2,
        Seq(recFST, Address, Address, Address, Address),
        Seq(Receiver, GenericFuncParams, SpecialParameter.ThisTypeInfo, OuterTypeInfo)),

      // Mut function
      tp(Int64, None,             true,            false,                    false,   false,            0,
        Seq(Address, coreObject),
        Seq(SMutRecord, SMutObject)),
      tp(Int64, None,             true,            true,                     false,   false,            0,
        Seq(Address, coreObject, Address),
        Seq(SMutRecord, SMutObject, SpecialParameter.ThisTypeInfo)),
      tp(Int64, None,             true,            false,                    false,   true,             0,
        Seq(Address, coreObject, Address),
        Seq(SMutRecord, SMutObject, OuterTypeInfo)),
      tp(Int64, None,             true,            true,                     false,   true,             0,
        Seq(Address, coreObject, Address, Address),
        Seq(SMutRecord, SMutObject, SpecialParameter.ThisTypeInfo, OuterTypeInfo)),
      tp(Int64, None,             true,            true,                     false,   false,            2,
        Seq(Address, coreObject, Address, Address, Address),
        Seq(SMutRecord, SMutObject, GenericFuncParams, SpecialParameter.ThisTypeInfo)),
      tp(Int64, None,             true,            false,                    false,   true,             2,
        Seq(Address, coreObject, Address, Address, Address),
        Seq(SMutRecord, SMutObject, GenericFuncParams, OuterTypeInfo)),
      tp(Int64, None,             true,            true,                     false,   true,             2,
        Seq(Address, coreObject, Address, Address, Address, Address),
        Seq(SMutRecord, SMutObject, GenericFuncParams, SpecialParameter.ThisTypeInfo, OuterTypeInfo)),

      // RetByVal
      tp(Unit, Some(recFST),      false,           false,                    false,   false,            0,
        Seq(Unit, recFST),
        Seq(RetByVal, Receiver)),
      tp(Unit, None,              false,           false,                    false,   false,            0,
        Seq(Unit),
        Seq(RetByVal)),
      tp(recFST, Some(recFST),    false,           false,                    false,   false,            0,
        Seq(recFST, recFST),
        Seq(RetByVal, Receiver)),
      tp(recFST, None,            false,           false,                    false,   false,            0,
        Seq(recFST),
        Seq(RetByVal)),
      tp(recVST, Some(recFST),    false,           false,                    false,   false,            0,
        Seq(Address, recFST),
        Seq(RetByVal, Receiver)),
      tp(recVST, None,            false,           false,                    false,   false,            0,
        Seq(Address),
        Seq(RetByVal)),
      tp(ctv(0), Some(recFST),    false,           false,                    false,   false,            0,
        Seq(Address, recFST),
        Seq(RetByVal, Receiver)),
      tp(ctv(0), None,            false,           false,                    false,   false,            0,
        Seq(Address),
        Seq(RetByVal)),
      tp(ltv(0), Some(recFST),    false,           false,                    false,   false,            0,
        Seq(Address, recFST),
        Seq(RetByVal, Receiver)),
      tp(ltv(0), None,            false,           false,                    false,   false,            0,
        Seq(Address),
        Seq(RetByVal)),
    )
  ) {
    test(s"makeABISignature($retType, $receiver, $hasMutParameter, $hasThisTypeInfoParameter, $isCFunc, $hasOuterTypeInfo, $genericParamsCount)") {
      val sourceSig = MethodSignature()(retType)
      val abiSig = MethodSignature(abiParams: _*)(ABI.makeABISigType(retType))
      ABI.makeABISignature(sourceSig, ABI.Description(receiver, hasMutParameter, hasThisTypeInfoParameter, isCFunc, hasOuterTypeInfo, hasRetByVal = false, genericParamsCount)) should be (abiSig, SpecialParamSet(specialParams))
    }
  }
}
