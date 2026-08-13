/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParameter.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.{SpecialParamSet, SpecialParameter}

class SpecialParamSetSuite extends CompilerSuite {

  def emptySet(): SpecialParamSet = {
    SpecialParamSet()
  }

  def onlyReceiver(): SpecialParamSet = {
    addReceiver(emptySet())
  }

  def onlyMut(): SpecialParamSet = {
    addMut(emptySet())
  }

  def onlyThisTypeInfo(): SpecialParamSet = {
    addThisTypeInfo(emptySet())
  }

  def allParams(): SpecialParamSet = {
    SpecialParamSet(Seq(Receiver, MutObject, ThisTypeInfo, RetByVal, MutRecord))
  }

  def addReceiver(specialParamSet: SpecialParamSet): SpecialParamSet = {
    specialParamSet.addElement(Receiver)
  }

  def addMut(specialParamSet: SpecialParamSet): SpecialParamSet = {
    specialParamSet.addElement(MutObject)
  }

  def addThisTypeInfo(specialParamSet: SpecialParamSet): SpecialParamSet = {
    specialParamSet.addElement(ThisTypeInfo)
  }

  def addRetByVal(specialParamSet: SpecialParamSet): SpecialParamSet = {
    specialParamSet.addElement(RetByVal)
  }

  def addMutRecord(specialParamSet: SpecialParamSet): SpecialParamSet = {
    specialParamSet.addElement(MutRecord)
  }

  def checkSetContent(result: SpecialParamSet, expected: Seq[SpecialParameter]): Unit = {
    result.elements.toSet shouldBe expected.toSet
  }

  test("simple inserting elements") {
    checkSetContent(addReceiver(emptySet()), Seq(Receiver))
    checkSetContent(addMut(emptySet()), Seq(MutObject))
    checkSetContent(addThisTypeInfo(emptySet()), Seq(ThisTypeInfo))
    checkSetContent(addRetByVal(emptySet()), Seq(RetByVal))
    checkSetContent(addMutRecord(emptySet()), Seq(MutRecord))
  }

  test("combined inserting elements") {
    checkSetContent(addMut(onlyReceiver()), Seq(Receiver, MutObject))
    checkSetContent(addReceiver(onlyMut()), Seq(MutObject, Receiver))
    checkSetContent(addThisTypeInfo(addMut(onlyReceiver())), Seq(Receiver, MutObject, ThisTypeInfo))
  }

  test("getting elements") {
    val specialParamSet = allParams()
    assert(specialParamSet.contains(Receiver))
    assert(specialParamSet.contains(MutObject))
    assert(specialParamSet.contains(ThisTypeInfo))
  }

  test("inserting duplicates") {
    val specialParamSet = allParams()
    intercept[AssertionError] {
      addMut(specialParamSet)
    }
  }
}
