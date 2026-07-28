/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.assembler.fixups.Relocation
import com.huawei.excelsior.jet.codeemitter.SymbolInfo
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.symlevel.ConstString
import com.huawei.excelsior.jet.compiler.symlevel.Method
import com.huawei.excelsior.jet.compiler.symlevel.Type

trait SymbolLinker extends SymbolInfo {

  /** Make new zeroended string encoded in UTF-8 or UTF-16 form and
    * placed into read-only data
    *
    * @param str - constant data
    * @return constant string symbol
    */
  def makeConstStringData(str: XString, bstr: Boolean): Symbol

  def makeStringRef(str: XString): Symbol

  /** Makes a new data segment with given contents and places it into read-only data.
    *
    * @param value constant data
    * @return symbol of generated constant data segment
    */
  def makeConstData(value: Array[Byte], align: Int): Symbol

  /** Allocates a new uninitialized (BSS) chunk of data with given size.
    * TODO: pass alignment.
    */
  def makeUninitializedData(size: Int): Symbol

  /** Makes a new symbol for initialized data segment. */
  def makeDataSymbol(): Symbol

  def getRTSGlobalSymbol(global: RTSGlobal): Symbol

  def getStaticFieldSymbol(host: Type, fieldOffset: Int): Symbol

  def getStringPoolEntry(host: Type, index: Int): ConstString

  /** Send generated data segment to compiler for adding it into obj file.
    * Segment should be bound to a symbol created with [[makeDataSymbol]].
    */
  def sendData(seg: Segment, method: Method): Unit

  /** Send bytecode segment to compiler for adding it into obj file. */
  def sendBytecode(seg: Segment): Unit
}
