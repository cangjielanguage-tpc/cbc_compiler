/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.llvm.bitcode

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.llvm.bitcode.AttributeKindCodes.*
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Attributes.AttributesList.*
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Bitcode.asUnsignedInt
import com.huawei.excelsior.jet.compiler.llvm.bitcode.Errors.{hopeThat, require}
import com.huawei.excelsior.jet.util.ScalaCollections.iterateUntilNone
import xscala.io.ByteBuffer
import xscala.text.Utf8Encoding
import xscala.util.MathUtils.{isNBits, isNBitsSigned}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Parsing of function attributes (aka param.attrs.). */
object Attributes {

  /** Single attribute element (e.g. zeroext, "disable-tail-calls"="false", ...), */
  trait Attribute

  /** One of predefined attributes (e.g. zeroext, noinline, ...). */
  sealed abstract class EnumAttribute extends Attribute
  object EnumAttribute {
    object Z_EXT extends EnumAttribute
  }

  /** Low-level bitcode entity representing group of attribute elements for the specific parameter. */
  private class AttributeGroup(

    /** Parameter index.
      *
      * It's equal to [[AttributesList]] if this group corresponds to the function itself.
      */
    val index: Int,

    val attrs: List[Attribute]
  ) {
    override def toString = s"$attrs @ $index"
  }

  /** Attributes for a function or for a call. */
  object AttributesList {
    private[bitcode] val EMPTY = new AttributesList(Map.empty)

    def apply(sets: collection.Map[Int, List[Attribute]]) =
      if (sets.isEmpty) EMPTY else new AttributesList(sets)

    val FUNCTION_IDX = -1
    val RET_VAL_IDX = 0
    val FIRST_PARAM_IDX = 1
  }

  class AttributesList(sets: collection.Map[Int, List[Attribute]]) {
    def allAttrs = sets.iterator

    private def getByIndex(index: Int) = sets.getOrElse(index, List.empty)

    def functionAttrs = getByIndex(FUNCTION_IDX)
    def returnValueAttrs = getByIndex(RET_VAL_IDX)
    def parameterAttrs(paramIdx: Int) = getByIndex(FIRST_PARAM_IDX + paramIdx)

    override def toString = sets.toArray sortBy (_._1) map { case (k, v) => s"$k: $v" } mkString ("{", ", ", "}")
  }

  object Scanner {
    private val IGNORED_STRING_ATTRIBUTES = Set(
      "cj2c",
      "cj_fast_call",
      "record_mut",
      "gc-leaf-function",
    )
  }

  class Scanner {
    private val attributeGroups = ArrayBuffer.empty[AttributeGroup]
    private val attributeLists = ArrayBuffer.empty[AttributesList]

    def decodeAttributeGroup(ctx: Bitstream.Context): Unit = {
      if (ctx.code != 3) {
        // ignore by default
        return
      }

      // PARAMATTR_GRP_CODE_ENTRY = 3  // ENTRY: [grpid, idx, attr0, attr1, ...]
      require(ctx.operandsCount >= 2, "PARAMATTR_GRP record must have at least 2 operands")

      val grpId = ctx.operand(0)

      val idx64 = ctx.operand(1)
      hopeThat(isNBits(idx64, 32), "unexpected idx %d", idx64)
      val idx = idx64.toInt

      def parseAttributes(): Iterator[Attribute] = {
        var i = 2

        def hasNext = i < ctx.operandsCount

        def nextLong(): Long = {
          require(hasNext, "out of operands during paramattrs parsing")
          val res = ctx.operand(i)
          i += 1
          res
        }

        def nextString(): String = {
          val buf = new ByteBuffer()
          var ch = nextLong()
          while (ch != 0) {
            require(isNBitsSigned(ch, 8), "attribute string character overflow %d", ch)
            buf.putByte(ch.toByte)
            ch = nextLong()
          }
          Utf8Encoding.decodeStringReplacing(buf.getBytesPointer, 0, buf.length)
        }

        @tailrec
        def nextAttribute(): Option[Attribute] = {
          if (!hasNext) return None

          asUnsignedInt(nextLong()) match {
            case 0 => // enum attribute
              asUnsignedInt(nextLong()) match {
                case ATTR_KIND_Z_EXT =>
                  Some(EnumAttribute.Z_EXT)

                case ATTR_KIND_NO_RETURN |
                     ATTR_KIND_NO_INLINE |
                     ATTR_KIND_OPTIMIZE_NONE |
                     ATTR_KIND_NO_ALIAS |
                     ATTR_KIND_NO_UNWIND |
                     ATTR_KIND_READ_NONE |
                     ATTR_KIND_READ_ONLY |
                     ATTR_KIND_SPECULATABLE |
                     ATTR_KIND_WILLRETURN |
                     ATTR_KIND_NOFREE |
                     ATTR_KIND_NOSYNC |
                     ATTR_KIND_NO_CALLBACK =>
                  // ignore them
                  nextAttribute()

                case ATTR_KIND_STRUCT_RET |
                     ATTR_KIND_BY_VAL =>
                  shouldNotReachHere("unsupported passing/returning of structure by value")

                case kind => shouldNotReachHere(s"unsupported enum attribute ($kind)")
              }

            case 1 => // integer attribute
              val kind = nextLong()
              val value = nextLong()
              shouldNotReachHere(s"unsupported integer attribute ($kind)")

            case 3 => // string without value attribute
              val kind = nextString()
              if (Scanner.IGNORED_STRING_ATTRIBUTES(kind)) {
                nextAttribute()
              } else {
                shouldNotReachHere(s"unsupported string attribute without value ($kind)")
              }

            case 4 => // string with value attribute
              val kind = nextString()
              val value = nextString()
              shouldNotReachHere(s"unsupported string attribute with value ($kind)")

            case 5 | 6 =>
              val kind = nextLong()
              shouldNotReachHere(s"unsupported type attribute ($kind)")

            case op => shouldNotReachHere(s"unexpected attribute kind ($op)")
          }
        }

        iterateUntilNone(nextAttribute())(_ => nextAttribute())
      }

      attributeGroups += new AttributeGroup(idx, parseAttributes().toList)
    }

    def decodeAttributes(ctx: Bitstream.Context): Unit = {
      // PARAMATTR_CODE_ENTRY_OLD = 1, // ENTRY: [paramidx0, attr0,
      hopeThat(ctx.code != 1, "unsupported PARAMATTR_OLD")

      if (ctx.code != 2) {
        // ignore by default
        return
      }

      val attrsByIndex = mutable.HashMap.empty[Int, List[Attribute]]

      // PARAMATTR_CODE_ENTRY = 2,     // ENTRY: [attrgrp0, attrgrp1, ...]
      for (i <- 0 until ctx.operandsCount) {
        val grpId = ctx.operand(i)
        val realId = grpId - 1
        hopeThat(0 <= realId && realId < attributeGroups.size, "unexpected attributes group %d", grpId)

        val group = attributeGroups(realId.toInt)
        val attrs = group.attrs

        if (attrs.isEmpty) {
          // It's a popular case due to ignoring most of the attributes.
        } else {
          val idx = group.index
          attrsByIndex(idx) = attrsByIndex.getOrElse(idx, List.empty) ++ attrs
        }
      }
      attributeLists += AttributesList(attrsByIndex)
    }

    def getResult = attributeLists
  }
}
