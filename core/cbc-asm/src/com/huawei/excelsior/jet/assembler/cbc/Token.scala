/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.cbc.Token.StringLit

import java.math.BigInteger
import scala.PartialFunction.condOpt
import scala.annotation.tailrec

/**
 * Base trait for all tokens in the CBC assembler.
 * Position is stored as (start, end) offset in the input string.
 */
sealed trait Token {
  def begin: Int
  def end: Int
  def isEnd: Boolean = false

  def is(p: Token.StructuralKind): Boolean = this match {
    case Token.Structural(_, _, actual) => p == actual
    case _ => false
  }

  def is(p: Token.KeywordKind): Boolean = this match {
    case Token.Keyword(_, _, actual) => p == actual
    case _ => false
  }
}

object Token {
  // Structural tokens - now case classes to store positions
  case class Structural(begin: Int, end: Int, sym: StructuralKind) extends Token
  case class Trivia(begin: Int, end: Int) extends Token

  // Literals
  case class StringLit(begin: Int, end: Int, value: String) extends Token
  case class IntegerLit(begin: Int, end: Int, value: Long) extends Token
  case class FloatLit(begin: Int, end: Int, value: Double) extends Token
  case class BoolLit(begin: Int, end: Int, value: Boolean) extends Token

  // Identifiers and keywords - each keyword is its own case class
  case class Identifier(begin: Int, end: Int, value: String) extends Token
  case class Keyword(begin: Int, end: Int, kw: KeywordKind) extends Token

  // Registers
  case class IR(begin: Int, end: Int) extends Token
  case class FR(begin: Int, end: Int) extends Token
  case class StackSlot(begin: Int, end: Int, value: Long) extends Token

  // Special markers
  case class Eol(begin: Int, end: Int) extends Token {
    override def isEnd: Boolean = true
  }
  case class Error(begin: Int, end: Int, msg: String) extends Token

  enum StructuralKind(val str: String) {
    // listed in descending order by size
    case Percent2 extends StructuralKind("%%")
    case Percent extends StructuralKind("%")
    case Eq extends StructuralKind("=")
    case Hash extends StructuralKind("#")
    case LBracket extends StructuralKind("[")
    case RBracket extends StructuralKind("]")
    case LParen extends StructuralKind("(")
    case RParen extends StructuralKind(")")
    case Comma extends StructuralKind(",")
  }

  enum KeywordKind(val str: String) {
    case Maintype extends KeywordKind("@main_type")
    case Cbcdeps extends KeywordKind("@cbc_deps")
    case Aotdeps extends KeywordKind("@aot_deps")
    case Foreignlibs extends KeywordKind("@foreign_libs")
    case Type extends KeywordKind("@type")
    case Flags extends KeywordKind("@flags")
    case Super extends KeywordKind("@super")
    case Enum extends KeywordKind("@enum")
    case TypeVars extends KeywordKind("@type_vars")
    case Constraints extends KeywordKind("@constraints")
    case Interfaces extends KeywordKind("@interfaces")
    case UnionFields extends KeywordKind("@union_fields")
    case EnumKind extends KeywordKind("@enum_kind")
    case Field extends KeywordKind("@field")
    case Fieldval extends KeywordKind("@value")
    case Method extends KeywordKind("@method")
    case End extends KeywordKind("@end")
    case Methodref extends KeywordKind("@method_ref")
    case Fieldref extends KeywordKind("@field_ref")
    case Link extends KeywordKind("@link")
    case MethodTypeName extends KeywordKind("@method_type_name")
    case SourceFile extends KeywordKind("@source_file")
    case UntypedCount extends KeywordKind("@untyped_count")
    case SavedIregs extends KeywordKind("@saved_iregs")
    case SavedFregs extends KeywordKind("@saved_fregs")
    case TypedSlots extends KeywordKind("@typed_slots")
    case Code extends KeywordKind("@code")
    case Ref extends KeywordKind("@ref")
    case Rec extends KeywordKind("@rec")
    case AotRef extends KeywordKind("@aref")
    case AotRec extends KeywordKind("@arec")
    case NullableOption extends KeywordKind("@nopt")
    case UnionOption extends KeywordKind("@uopt")
    case LiveRef extends KeywordKind("@live.ref")
    case LivePrim extends KeywordKind("@live.prim")
    case LiveRec extends KeywordKind("@live.rec")
    case Dead extends KeywordKind("@dead")
    case AotDirect extends KeywordKind("@aot.direct")
    case AotVirtual extends KeywordKind("@aot.virtual")
    case AotInterface extends KeywordKind("@aot.interface")
    case AotStatic extends KeywordKind("@aot.static")
    case AotInstance extends KeywordKind("@aot.instance")

    case IRZ  extends KeywordKind("IRZ")
    case IR1  extends KeywordKind("IR1")
    case IR2  extends KeywordKind("IR2")
    case IR3  extends KeywordKind("IR3")
    case IR4  extends KeywordKind("IR4")
    case IR5  extends KeywordKind("IR5")
    case IR6  extends KeywordKind("IR6")
    case IR7  extends KeywordKind("IR7")
    case IR8  extends KeywordKind("IR8")
    case IR9  extends KeywordKind("IR9")
    case IR10 extends KeywordKind("IR10")
    case IR11 extends KeywordKind("IR11")
    case IR12 extends KeywordKind("IR12")
    case IR13 extends KeywordKind("IR13")

    case FR0  extends KeywordKind("FR0")
    case FR1  extends KeywordKind("FR1")
    case FR2  extends KeywordKind("FR2")
    case FR3  extends KeywordKind("FR3")
    case FR4  extends KeywordKind("FR4")
    case FR5  extends KeywordKind("FR5")
    case FR6  extends KeywordKind("FR6")
    case FR7  extends KeywordKind("FR7")
    case FR8  extends KeywordKind("FR8")
    case FR9  extends KeywordKind("FR9")
    case FR10 extends KeywordKind("FR10")
    case FR11 extends KeywordKind("FR11")
    case FR12 extends KeywordKind("FR12")
    case FR13 extends KeywordKind("FR13")
    case FR14 extends KeywordKind("FR14")
    case FR15 extends KeywordKind("FR15")

    case Void    extends KeywordKind("Void")
    case Unit    extends KeywordKind("Unit")
    case Boolean extends KeywordKind("Bool")
    case I8      extends KeywordKind("I8")
    case U8      extends KeywordKind("U8")
    case I16     extends KeywordKind("I16")
    case U16     extends KeywordKind("U16")
    case I32     extends KeywordKind("I32")
    case U32     extends KeywordKind("U32")
    case UChar32 extends KeywordKind("Char")
    case I64     extends KeywordKind("I64")
    case U64     extends KeywordKind("U64")
    case IAddr   extends KeywordKind("IAddr")
    case UAddr   extends KeywordKind("UAddr")
    case F16     extends KeywordKind("F16")
    case F32     extends KeywordKind("F32")
    case F64     extends KeywordKind("F64")
    case PTR     extends KeywordKind("PTR")
    case Box     extends KeywordKind("Box")

    case EQ       extends KeywordKind("EQ")
    case NE       extends KeywordKind("NE")
    case GE       extends KeywordKind("GE")
    case GT       extends KeywordKind("GT")
    case LT       extends KeywordKind("LT")
    case LE       extends KeywordKind("LE")
    case REQ      extends KeywordKind("REQ")
    case RNE      extends KeywordKind("RNE")
    case UGE      extends KeywordKind("UGE")
    case UGT      extends KeywordKind("UGT")
    case ULE      extends KeywordKind("ULE")
    case ULT      extends KeywordKind("ULT")
    case TESTZ    extends KeywordKind("TESTZ")
    case TESTNZ   extends KeywordKind("TESTNZ")
    case TESTBIT  extends KeywordKind("TESTBIT")
    case TESTNBIT extends KeywordKind("TESTNBIT")
    case FEQ      extends KeywordKind("FEQ")
    case FNE      extends KeywordKind("FNE")
    case FGE      extends KeywordKind("FGE")
    case FNGE     extends KeywordKind("FNGE")
    case FGT      extends KeywordKind("FGT")
    case FNGT     extends KeywordKind("FNGT")
    case FLT      extends KeywordKind("FLT")
    case FNLT     extends KeywordKind("FNLT")
    case FLE      extends KeywordKind("FLE")
    case FNLE     extends KeywordKind("FNLE")
  }

  object KeywordKind {
    val map = KeywordKind.values.map(v => (v.str, v)).toMap
  }
}

class AsmTokenizer(input: String, private var pos: Int = 0) {
  private val data = input.toCharArray
  private val tokens = new collection.mutable.ListBuffer[Token]

  private val END: Char = 0

  private def next(): Char = {
    // ignore unicode handling
    if (pos == data.length) return END
    val res = data(pos)
    pos += 1
    res
  }

  private def current: Char = {
    if (pos == data.length) return END
    data(pos)
  }

  private def isEof: Boolean = current == END

  private def putBack(c: Char): Unit = {
    pos -= 1
    assert(data(pos) == c)
  }

  private def isDigit(ch: Char): Boolean = ch >= '0' && ch <= '9'

  private def isHexDigit(ch: Char): Boolean =
    (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F')

  private def isIdentStart(ch: Char): Boolean =
    (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'

  private def isIdent(ch: Char): Boolean =
    isIdentStart(ch) || (ch >= '0' && ch <= '9') || ch == '.' || ch == ':'

  private def isWhiteSpace(c: Char): Boolean = c match {
    case ' ' => true
    case '\n' => true
    case '\r' => true
    case '\f' => true
    case '\t' => true
    case _ => false
  }

  private def readHexNumber(adjustment: Int, negative: Boolean): Token = {
    val start = pos
    while (isHexDigit(current)) next()
    val value = BigInteger(String(data, start, pos - start), 16).longValue()
    val nvalue = if (negative) -value else value
    Token.IntegerLit(start + adjustment, pos, nvalue)
  }

  private def readBinaryNumber(adjustment: Int, negative: Boolean): Token = {
    val start = pos
    while (current == '0' || current == '1') next()
    val value = BigInteger(String(data, start, pos - start), 2).longValue()
    val nvalue = if (negative) -value else value
    Token.IntegerLit(start + adjustment, pos, nvalue)
  }

  private def readStackSlot(): Token = {
    val start = pos
    assert(current == '$')
    pos += 1
    val numStart = pos
    while (isDigit(current)) next()
    val value = BigInteger(String(data, numStart, pos - numStart), 10).longValue()
    Token.StackSlot(start, pos, value)
  }

  private def readDecimalOrFloat(adjustment: Int, negative: Boolean): Token = {
    val start = pos
    while (isDigit(current)) next()
    if (current == '.') {
      next()
      while (isDigit(current)) next()
      try {
        val value = java.lang.Double.parseDouble(String(data, start, pos - start))
        val nvalue = if (negative) -value else value
        Token.FloatLit(start + adjustment, pos, nvalue)
      } catch {
        case _: NumberFormatException =>
          Token.Error(start + adjustment, pos, "incorrect floating point number")
      }
    }
    val value = BigInteger(String(data, start, pos - start), 10).longValue()
    val nvalue = if (negative) -value else value
    Token.IntegerLit(start + adjustment, pos, nvalue)
  }

  private def readNumber(adjustment: Int, negative: Boolean): Token = {
    if (current == '0') {
      next()
      if (current == 'x' || current == 'X') {
        next()
        readHexNumber(adjustment - 2, negative)
      } else if (current == 'b' || current == 'B') {
        next()
        readBinaryNumber(adjustment - 2, negative)
      } else {
        putBack('0')
        readDecimalOrFloat(adjustment, negative)
      }
    } else {
      readDecimalOrFloat(adjustment, negative)
    }
  }

  private def readIdentifier(): Token = {
    val start = pos

    val identStr = new StringBuilder
    while (!isEof && isIdent(current)) {
      identStr.append(next())
    }

    val tokenStr = identStr.toString
    Token.KeywordKind.map.get(tokenStr) match {
      case Some(kw) => Token.Keyword(start, pos, kw)
      case _ => Token.Identifier(start, pos, tokenStr)
    }
  }

  private def readAnyIdentifier(): Token = {
    val start = pos
    assert(current == '`')
    pos += 1

    val identStr = new StringBuilder
    while (!isEof && current != '`') {
      identStr.append(next())
    }
    if (current != '`') {
      return Token.Error(start, pos, "unterminated '`'")
    }
    pos += 1
    Token.Identifier(start, pos, identStr.toString())
  }

  private def readAnnot(): Token = {
    val start = pos
    assert(current == '@')
    pos += 1
    val identStr = new StringBuilder("@")
    while (!isEof && isIdent(current)) {
      identStr.append(next())
    }

    val tokenStr = identStr.toString
    Token.KeywordKind.map.get(tokenStr) match {
      case Some(kw) => Token.Keyword(start, pos, kw)
      case _ => Token.Error(start, pos, "unknown annot")
    }
  }

  private def readString(): Token = {
    val start = pos
    assert(current == '"')
    next()
    val strVal = new StringBuilder
    while (!isEof) {
      if (current == '"') {
        next()
        return StringLit(start, pos, strVal.toString())
      } else if (current == '\\' && !isEof) {
        next() // skip once
        strVal.append(next())
      } else if (current == '\n') {
        return Token.Error(start, pos, "Unterminated string")
      } else {
        strVal.append(next())
      }
    }
    Token.Error(start, pos, "Unterminated string")
  }

  private def readWhitespace(): Token = {
    val start = pos
    while (isWhiteSpace(current)) next()
    Token.Trivia(start, pos)
  }

  private def readComment(): Token = {
    val start = pos
    assert(current == ';')
    while (current != '\n' && current != '\r' && !isEof) next()
    Token.Trivia(start, pos)
  }

  private def startsWith(str: String): Boolean = {
    str.iterator.sameElements(data.view.slice(pos, pos + str.length))
  }

  private def readStructural(): Option[Token] = {
    val start = pos
    for (kind <- Token.StructuralKind.values) {
      if (startsWith(kind.str)) {
        pos += kind.str.length
        return Some(Token.Structural(start, pos, kind))
      }
    }
    None
  }

  def tokenize(): Seq[Token] = {

    @tailrec
    def process(): Unit = {
      current match {
        case END =>
          tokens += Token.Eol(pos, pos)
          return
        case ';' => tokens += readComment()
        case '-' => next(); tokens += readNumber(1, negative = true)
        case c if isDigit(c) => tokens += readNumber(0, negative = false)
        case c if isIdentStart(c) => tokens += readIdentifier()
        case '@' => tokens += readAnnot()
        case '$' => tokens += readStackSlot()
        case '"' => tokens += readString()
        case '`' => tokens += readAnyIdentifier()
        case c if isWhiteSpace(c) => tokens += readWhitespace()
        case c =>
          readStructural() match {
            case Some(tk) => tokens += tk
            case _ =>
              val start = pos
              next()
              tokens += Token.Error(start, pos, "unknown token")
          }
      }
      process()
    }

    process()
    tokens.result()
  }
}
