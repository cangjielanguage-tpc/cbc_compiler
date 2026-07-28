/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import xscala.util.StringOps.*

import scala.collection.mutable


/** Data annotation parsing.
  * @example `@Data(data = "S00ffB0f")` will generate a segment: FFH 03H
  * 
  * Grammar: {{{
  *  S -> (Int | FieldRef)+
  *  Int -> Bxx | Sxxxx | Ixxxxxxxx | Lxxxxxxxxxxxxxxxx
  *  FieldRef -> Afull_class_name;
  *  
  *  x -- hex digit
  * }}}
  */
object DataAnnotationParsing {
  abstract sealed class Types

  case class FieldRef(s: String) extends Types
  case class Integer(widthInBytes: Int, x: Long) extends Types {
    assert(widthInBytes > 0)
  }

  def parse(s: String): Seq[Types] = {
    val result = mutable.ArrayBuffer.empty[Types]
    var currPos = 0
    while (currPos < s.length) {
      if (s(currPos) == 'A') {
        val end = s.indexOf(';', currPos)
        if (end == -1) {
          shouldNotReachHere(s"Didn't find end of FieldRef path (;) in $s")
        }
        val symbol = s.substring(currPos + 1, s.indexOf(';', currPos))
        currPos += end - currPos + 1
        result.addOne(FieldRef(symbol))
      } else {
        val numLength = s(currPos) match {
          case 'B' => 2
          case 'S' => 4
          case 'I' => 8
          case 'L' => 16
        }
        val numberStr = s.substring(currPos + 1, currPos + 1 + numLength)

        // Parse hex number to long
        val number = numberStr.toUnsignedHexOption.getOrElse(shouldNotReachHere(s"Cannot parse hex number $numberStr"))

        currPos += numLength + 1
        result.addOne(Integer(numLength / 2, number))
      }
    }
    result.toSeq
  }
}
