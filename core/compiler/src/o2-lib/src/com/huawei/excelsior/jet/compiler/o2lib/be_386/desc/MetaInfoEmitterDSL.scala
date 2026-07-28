/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.be_386.desc

import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.MetaInfoEmitter.{MetaInfo, MetaInfoType}
import com.huawei.excelsior.jet.compiler.o2lib.be_386.desc.MetaInfoEmitterDSL.EntryWithValue.*

import scala.language.implicitConversions

/** DSL for emitting runtime metadata structures
  *
  * Following structure in AJ:
  * {{{
  * @Struct
  * class A {
  *     int intField1;
  *     int intField2;
  *     boolean flag1;
  *
  *     @Flat(length = 7)
  *     ByteBuffer padding;
  *     // implicit padding also supported
  *
  *     // Reference to other @Struct
  *     OtherStruct other;
  * }
  * }}}
  *
  * could be emitted like this:
  * {{{
  *   struct("A")(
  *     "intField2" -> genInt(...),
  *     "intField1" -> genInt(...),
  *     "other" -> fixup(...),
  *   )
  * }}}
  *
  * '''Note'''
  *
  * Field `flag1` isn't mentioned in [[struct]], so it will be defined as zero.
  *
  * If padding has implicit definition it also will be zeroed. Else it will work on a general basis.
  *
  * Fields order in [[struct]] is not important, fields will be ordered before writing.
  *
  * ''__Important__: [[struct]] function saves right hand side part of fields definition as zero-arguments lambda-function.
  * So you can change writable values between [[MetaInfo.define]] and [[MetaInfo.emit]] (but it is not advised).''
  *
  * @author qq
  */
object MetaInfoEmitterDSL {

  /** Types of non-condition values:
    *
    *  - `NumericValue` currently used only for zeroing.
    *  - `Action` contains right-side operation of field definition.
    */
  enum EntryWithValue {
    case NumericValue(v: Int)
    case Action(op: () => Any)
  }

  /** Alias for multiline `?` operator.
    *
    * For example:
    * {{{
    * struct(...)(
    *   "field1" -> flag ? genInt(...),
    *   "field2" -> flag ? fixup(...),
    *   "field3" -> flag ? genShort(...),
    * )
    * }}}
    *
    * Can be rewritten as:
    * {{{
    * struct(...)(
    *   when(flag)(
    *     "field1" -> genInt(...),
    *     "field2" -> fixup(...),
    *     "field3" -> genShort(...),
    *   )
    * )
    * }}}
    * */
  case class CondEntry private[MetaInfoEmitterDSL] (cond: Boolean)(val arr: Entry*)
  object CondEntry {
    def unapply(x: CondEntry): Option[(Boolean, Seq[Entry])] = Some(x.cond, x.arr)
  }

  def when(cond: Boolean)(arr: Entry*): CondEntry = CondEntry(cond)(arr*)

  type Entry = (String, EntryWithValue) | CondEntry

  /** Define and emit structure. */
  def struct(metaInfoType: MetaInfoType)(arr: Entry*): MetaInfo = {
    MetaInfo(metaInfoType).define(arr: _*).emit()
  }

  /** Continue to define and emit [[MetaInfo]]. Probably it should be used after using [[structDraft]]. */
  def struct(metaInfo: MetaInfo)(arr: Entry*): MetaInfo = {
    metaInfo.define(arr: _*).emit()
  }

  /** Define and emit flat objects. */
  def flatStruct(metaInfoType: MetaInfoType)(arr: Entry*): MetaInfo = {
    MetaInfo(metaInfoType).define(arr: _*)._emit()
  }

  /** Define and emit sub-struct --- that struct haven't their own list of allocated objects.
    * You should use it when you need to define struct while defining another struct (and it isn't flat).
    * */
  def subStruct(metaInfoType: MetaInfoType)(arr: Entry*): MetaInfo = {
    MetaInfo(metaInfoType).define(arr: _*).emit(subStructure = true)
  }

  /** Define fields, but after that struct will not be emitted. */
  def structDraft(metaInfoType: MetaInfoType)(arr: Entry*): MetaInfo = {
    MetaInfo(metaInfoType).define(arr: _*)
  }
  
  implicit inline def functionToAction(action: => Any): Action = Action(() => action)

  extension (b: Boolean) {
    /** Defines condition-based definition:
      * {{{
      *   "field" -> flag ? operation(...)
      * }}}
      * is equivalent to:
      * {{{
      *   "field" -> if (flag) operation(...) else NumericValue(0)
      * }}}
      *
      * In other words: if flag is true do `operation(...)`, else that field will be zeroed.
      * */
    def ?(action: => Any): EntryWithValue = if b then Action(() => action) else NumericValue(0)
  }
}
