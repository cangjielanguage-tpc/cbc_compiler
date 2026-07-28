/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.XString

import scala.collection.mutable.ArrayBuffer

/** Implementation of [[JStringsModule.StringBuffer.appendf()]].
  * Implementation notes:
  * We use [[scala.collection.StringOps]] for formatting output.
  */
object FormatImpl {
  def appendf(buffer: JStringsModule.StringBuffer, format: String, args: Any*): Unit = {
    val sb = new StringBuilder
    val newArgs = ArrayBuffer.empty[Any]
    var argcnt = 0
    var fcnt = 0
    while (fcnt < format.length) {
      var ch = format.charAt(fcnt)
      ch match {
        case '\\' =>
          fcnt += 1
          if (fcnt >= format.length) throw new Error("invalid format string: '\\' at end of string")
          ch = format.charAt(fcnt)
          ch match {
            case 'n' => ch = '\n'
            case 'r' => ch = '\r'
            case 't' => ch = '\t'
            case '\\' => ch = '\\'
            case '"' => ch = '"'
            case _ => throw new Error("unsupported escape sequence \\" + ch)
          }
          sb.append(ch)
        case '%' =>
          fcnt += 1
          if (fcnt >= format.length) throw new Error("invalid format string: '%' at end of string")
          ch = format.charAt(fcnt)
          if (ch == '%') {
            sb.append("%%")
          } else {
            val arg = args(argcnt)
            argcnt += 1
            ch match {
              case 'S' =>
                if (arg == null) {
                  newArgs += null
                } else if (!arg.isInstanceOf[XString]) {
                  throw new Error("invalid format argument, type " + arg.getClass + " (expected XString)")
                } else {
                  newArgs += arg.toString
                }
                sb.append("%s")
              case 'O' => throw new Error("unimplemented")
              case 's' =>
                arg match {
                  case _: String =>
                    sb.append("%s")
                    newArgs += arg
                  case _ => throw new Error("invalid format argument, type " + arg.getClass + " (expected String)")
                }
              case 'u' =>
                arg match {
                  case i: Int =>
                    sb.append("%d")
                    newArgs += (i & 0xffffffffL)
                  case _ => throw new Error("invalid format argument, type " + arg.getClass + " (expected int)")
                }
              case _ =>
                // handle other cases by Java formatter. However we should handle "*"
                // for width and precision first as it is not supported by Java formatter.
                if (format.substring(fcnt).startsWith(".*c")) {
                  arg match {
                    case i: Int =>
                      sb.append('%')
                      if (i != 0) sb.append(arg)
                      sb.append('c')
                      fcnt += 2
                      newArgs += args(argcnt)
                      argcnt += 1
                    case _ => throw new Error("invalid format argument, type " + arg.getClass + " (expected int)")
                  }
                } else if (ch == '*') {
                  arg match {
                    case i: Int =>
                      sb.append('%')
                      if (i != 0) sb.append(arg)
                      newArgs += args(argcnt)
                      argcnt += 1
                    case _ => throw new Error("invalid format argument, type " + arg.getClass + " (expected int)")
                  }
                } else {
                  sb.append('%')
                  sb.append(ch)
                  newArgs += arg
                }
            }
          }
        case _ => sb.append(ch)
      }
      fcnt += 1
    }
    val out = sb.toString().format(newArgs.toSeq*)
    buffer.appendString(XString(out))
  }
}
