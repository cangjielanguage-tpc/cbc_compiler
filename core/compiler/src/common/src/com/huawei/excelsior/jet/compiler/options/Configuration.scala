/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.options

import com.huawei.excelsior.jet.common.XString
import xscala.util.StringOps.asciiIsWhitespace

import scala.collection.mutable

/** Configuration is a map from compiler options to their values.
  *
  * @author ikireev
  * @author conwor
  */
object Configuration {

  def parse(input: String): mutable.LinkedHashMap[Option[_], Any] = { // TODO: refactor JITEnvironment and restore `collection.Map`
    val parser = try {
      new Parser(XString(input))
    } catch {
      case x: Throwable => throw new InternalError(s"Compiler options parser error, could not parse configuration: $input", x)
    }
    parser.configuration
  }

  /** Parse compile options with standard syntax
    *
    * TODO: ensure thread-safety. Rename/split to show immutability.
    *
    * {{{
    * SetupDirective      = SetBoolOption | ShortSetBoolOption | SetOtherOption | DeclareBoolOption | DeclareOtherOption
    * SetBoolOption       = '-' name ( '+' | '-' )
    * ShortSetBoolOption  = ( '+' | '-' ) name
    * SetOtherOption      = '-' name '=' [ value ]
    * DeclareBoolOption   = '-' name ':' ( '+' | '-' )
    * DeclareOtherOption  = '-' name ':=' [ value ]
    * }}}
    */
  private class Parser(input: XString) {

    private[Configuration] val configuration = mutable.LinkedHashMap.empty[Option[_], Any]
    private val length = input.length

    locally {
      val endPos = parse()
      assert(endPos == this.length)
    }

    private def addOption(name: XString, value: Any): Unit = {
      val option = Option.byName(name.toString)
      if (option != null) {
        configuration(option) = option.parse(value.toString)
      }
    }

    private def isWhitespace(b: Byte) = (b & 0xFF).toChar.asciiIsWhitespace

    private def skipWhitespaces(position: Int): Int = {
      var pos = position
      while (pos < length && isWhitespace(input.charAt(pos))) {
        pos += 1
      }
      pos
    }

    private def parseQuoted(position: Int): Int = {
      var quote = 0
      var pos = skipWhitespaces(position)
      while (pos < length) {
        val c = input.charAt(pos)
        c match {
          case '\"' | '\'' =>
            if (c == quote) {
              return pos + 1
            } else {
              if (quote == 0) {
                quote = c
              }
              pos += 1
            }

          case _ =>
            if (isWhitespace(c) && quote == 0) {
              return pos
            } else {
              pos += 1
            }
        }
      }
      pos
    }

    private def parseOtherOption(name: XString, pos: Int): Int = {
      val newPos = skipWhitespaces(pos)
      if (newPos != pos) {
        addOption(name, XString.empty)
        newPos
      } else {
        val valueEnd = parseQuoted(pos)
        val c = input.charAt(pos)
        val value = if (c == input.charAt(valueEnd - 1) && (c == '\'' || c == '\"')) {
          input.substring(pos + 1, valueEnd - 1)
        } else {
          input.substring(pos, valueEnd)
        }
        addOption(name, value)
        skipWhitespaces(valueEnd)
      }
    }

    private def parseOneExpr(isOn: Boolean, position: Int): Int = {
      var pos = position
      val begin = pos
      var c = 0
      var isJavaIdentifier = true
      while (isJavaIdentifier && pos < length) {
        c = input.charAt(pos)
        if (begin == pos && Character.isJavaIdentifierStart(c.toChar) ||
          begin != pos && Character.isJavaIdentifierPart(c.toChar)) {
          pos += 1
        } else {
          isJavaIdentifier = false
        }
      }
      if (begin != pos) {
        val name = input.substring(begin, pos)
        val newPos = skipWhitespaces(pos)
        if (newPos != pos || pos == length) {
          addOption(name, isOn)
          newPos
        } else if (!isOn) {
          val isDeclaration = c == ':'
          if (isDeclaration) {
            pos += 1
            if (pos < length) {
              c = input.charAt(pos)
            } else {
              throw new InternalError(s"Compiler options parser error: unexpected end of line after declaration mark \"-$name:\"")
            }
          }
          c match {
            case '+' | '-' =>
              addOption(name, c == '+')
              skipWhitespaces(pos + 1)

            case '=' =>
              parseOtherOption(name, pos + 1)

            case _ =>
              if (isDeclaration) {
                throw new InternalError(s"Compiler options parser error: unexpected symbol after declaration mark \"-$name:<?>\"")
              } else {
                throw new InternalError(s"Compiler options parser error: unexpected symbol after option/equation name \"-$name<?>\"")
              }
          }
        }  else {
          throw new InternalError(s"Compiler options parser error: unexpected symbol after short option name \"+$name<?>\"")
        }
      } else {
        throw new InternalError("Compiler options parser error: illegal option/equation name")
      }
    }

    private def parse(): Int = {
      var pos = skipWhitespaces(0)
      while (pos < length) {
        val c = input.charAt(pos)
        c match {
          case '-' |  '+' =>
            pos = parseOneExpr(c == '+', pos + 1)

          case _ =>
            if (isWhitespace(c)) {
              pos += 1
            } else {
              throw new InternalError(s"Compiler options parser error: unexpected symbol before directive \"${c.toChar}\"")
            }
        }
      }
      pos
    }
  }
}
