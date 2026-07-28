/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as CharClass
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.CPEntryModes.CPEntryMode
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.ErrorMessages.*
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{ErrMsg, JStringsModule as js, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind.Unchecked
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

import scala.annotation.tailrec

class ScanIf {

  private[projectsystem] var value: Boolean = _
  private[projectsystem] var elze: Boolean = _
  private[projectsystem] var elsif: Boolean = _
  private[projectsystem] var up: ScanIf = _

}


class Scan {

  private val jstrFalse: XString = js.newJString("FALSE")
  private val jstrTrue: XString = js.newJString("TRUE")
  private val ident: Char = '\u0001'
  private val string: Char = '\u0002'

  /*RO*/ var in: xfs.TextFile = _
  private[projectsystem] var linebf = new js.StringBuffer()
  /*RO*/ var lineno: Int = _
  private[projectsystem] var ifs: ScanIf = _
  private[projectsystem] var srcbf = new js.StringBuffer()
  private[projectsystem] var token: XString = _
  private[projectsystem] var cpentry: XString = _
  private[projectsystem] var cpentrymode: CPEntryMode = _
  // the bellow fields used by Preprocessor method
  private[projectsystem] var ps: Int = _
  private[projectsystem] var syps: Int = _
  private[projectsystem] var sy: Char = _
  private[projectsystem] var error: Boolean = _
  private[projectsystem] var skip: Boolean = _

  protected def wrongSyntax(f: xfs.TextFile, line: Int, ps: Int, err: ErrMsg): Unit = {
    val msg = env.errors.getMsg(err)
    val fname = FS.HOST.toPlatform(f.getName)
    if (ps < 0) {
      env.errors.message(msg_error_in_file, fname, line, msg)
    } else {
      env.errors.message(msg_error_in_file_ps, fname, line, ps, msg)
    }
  }

  @tailrec
  private def onScanIf(i: ScanIf): Boolean = i == null || i.value != i.elze && onScanIf(i.up)

  private def isLetter(ch: Char): Boolean = {
    if (CharClass.isWhiteSpace(ch)) {
      return false
    }
    ch match {
      case '!' |
           '(' |
           ')' |
           '=' |
           '#' |
           '&' |
           '<' |
           '>' |
           '+' |
           '-' |
           '\u0000' =>
        false
      case _ =>
        true
    }
  }

  def readText(f: xfs.TextFile): Boolean = {
    /* if error then returns TRUE */
    this.in = f
    this.ifs = null
    var lm = 1
    this.linebf.trunc(0)
    val backslashconcatlines = !this.isInstanceOf[ScanPro] || this.asInstanceOf[ScanPro].backslashconcatlines
    while (true) {
      this.lineno = lm
      this.srcbf.trunc(0)
      var line = f.readLine()
      while (line != null) {
        lm += 1
        var i = line.length
        var wasWhitespace = false
        while (i > 0 && CharClass.isWhiteSpace(line.charAt(i - 1))) {
          i -= 1
          wasWhitespace = true
        }
        if (i == 0 || wasWhitespace || !backslashconcatlines || line.charAt(i - 1) != '\\' || f.readRes != xfs.endOfLine) {
          this.srcbf.appendString(line.substring(0, i))
          line = null
        } else {
          this.srcbf.appendString(line.substring(0, i - 1))
          line = f.readLine()
        }
      }
      if (this.preprocessor()) {
        return true
      }
      if (f.readRes == xfs.endOfInput) {
        if (env.context != null) {
          if (this.cpentry == null) {
            env.errors.message(ErrMsg471)
          } else {
            env.errors.message(ErrMsg475)
          }
          return true
        } else {
          return false
        }
      }
      assert(f.readRes == xfs.endOfLine)
    }
    throw new AssertionError()
  }

  def preprocessor(): Boolean = {
    this.token = null
    if (this.macros()) {
      return true
    }
    this.error = false
    this.skip = false
    this.setPos(0)
    this.setSymbolPos(0)
    this.next()
    if (this.getSymbol == '!') {
      this.next()
      if (!this.isSymbolIdent) {
        if (onScanIf(this.ifs)) {
          return this.Do()
        }
        return false
      } else if (this.token.equals2("IF") && this.cpentry == null) {
        this.next()
        val if_lex = new ScanIf()
        if_lex.elze = false
        if_lex.elsif = false
        if_lex.up = this.ifs
        val v = this.expression()
        if_lex.value = this.boolean(v)
        if (!this.isSymbolIdent || !this.token.equals2("THEN")) {
          this.syntaxError()
          return true
        }
        this.next()
        if (!this.isLineEnd) {
          this.syntaxError()
          return true
        }
        this.ifs = if_lex
        return this.error
      } else if (this.token.equals2("ELSIF")) {
        if (this.ifs == null || this.ifs.elze) {
          this.syntaxError()
          return true
        }
        this.next()
        val if_lex = new ScanIf()
        if_lex.elze = false
        if_lex.elsif = true
        if_lex.up = this.ifs
        val v = this.expression()
        if_lex.value = this.boolean(v)
        if (!this.isSymbolIdent || !this.token.equals2("THEN")) {
          this.syntaxError()
          return true
        }
        this.next()
        if (!this.isLineEnd) {
          this.syntaxError()
          return true
        }
        this.ifs.elze = true
        this.ifs = if_lex
        return this.error
      } else if (this.token.equals2("ELSE")) {
        if (this.ifs == null || this.ifs.elze) {
          this.syntaxError()
          return true
        }
        this.next()
        if (!this.isLineEnd) {
          this.syntaxError()
          return true
        }
        this.ifs.elze = true
        return false
      } else if (this.token.equals2("END") && this.cpentry == null) {
        this.next()
        if (this.ifs == null || !this.isLineEnd) {
          this.syntaxError()
          return true
        }
        while (this.ifs.elsif) {
          this.ifs = this.ifs.up
        }
        this.ifs = this.ifs.up
        return false
      } else if (!onScanIf(this.ifs)) {
        if (this.token.equals2("CLASSPATHENTRY") || this.token.equals2("BUNDLEENTRY") || this.token.equals2("CLASSLOADERENTRY")) {
          if (this.cpentry != null) {
            this.syntaxError()
            return true
          }
          this.cpentry = js.jstrEmpty
        } else if (this.token.equals2("END")) {
          assert(this.cpentry != null)
          this.cpentry = null
        }
        return false
      } else if (this.token.equals2("MESSAGE")) {
        this.next()
        val v = this.expression()
        if (!this.isLineEnd) {
          this.syntaxError()
          return true
        }
        val fname = FS.HOST.toPlatform(this.in.getName)
        env.errors.message(msg_error_in_file, fname, this.lineno, v)
        return this.error
      } else if (this.token.equals2("SET")) {
        this.next()
        if (!this.isSymbolIdent) {
          this.syntaxError()
          return true
        }
        val v = this.token
        this.next()
        return this.optionValue(v)
      } else if (this.token.equals2("NEW")) {
        this.next()
        if (!this.isSymbolIdent) {
          this.syntaxError()
          return true
        }
        val v = this.token
        this.next()
        // Treat user defined options & equations as Unchecked.
        // How can they affect compilation? If they can, they must be defined in the compiler
        if (this.getSymbol == '+' || this.getSymbol == '-') {
          env.config.newOptionJS(v, value = false, Unchecked)
        } else if (this.getSymbol == '=') {
          env.config.newEquationJS(v, Unchecked)
        }
        return this.optionValue(v)
      }
    } else if (!onScanIf(this.ifs)) {
      return false
    }
    this.Do()
  }

  def optionValue(nm: XString): Boolean = {
    if (this.getSymbol == '+' || this.getSymbol == '-') {
      env.config.setOptionJS(nm, this.getSymbol == '+')
      this.next()
      if (!this.isLineEnd) {
        this.syntaxError()
        return true
      }
      if (env.config.res != env.ok) {
        this.notDefined()
        return true
      }
    } else if (this.getSymbol == '=') {
      this.nextAll()
      env.config.setEquationJS(nm, this.token)
      if (env.config.res != env.ok) {
        this.notDefined()
        return true
      }
    } else {
      this.syntaxError()
      return true
    }
    false
  }

  def macros(): Boolean = {
    var i = 0
    var st = '\u0000'
    this.linebf.trunc(0)
    while (i <= this.srcbf.length) {
      var ch = this.srcbfCharAt(i)
      i += 1
      if (ch == '\u0000') {
        return false
      }
      if ((ch == '\"' || ch == '\'') && st == '\u0000') {
        st = ch
        this.linebf.appendChar(ch)
      } else if (st != '\u0000' && ch == st) {
        st = '\u0000'
        this.linebf.appendChar(ch)
      } else if (st == '\u0000' && ch == '$') {
        ch = this.srcbfCharAt(i)
        i += 1
        if (ch == '!') {
          var d = FS.getPath(this.in.getName)
          if (d.length == 0) {
            d = js.jstrDot
          }
          val k = d.length - 1
          if (d.charAt(k) == '/') {
            d = d.substring(0, k)
          }
          this.linebf.appendString(d)
        } else if (ch == '(') {
          i = this.macro0(i)
        } else if (ch == '$') {
          this.linebf.appendChar('$')
        } else {
          this.linebf.appendChar('$')
          this.linebf.appendChar(ch)
        }
      } else {
        this.linebf.appendChar(ch)
      }
    }
    false
  }

  def macro0(iPar: Int): Int = {
    var i = iPar
    val sb = new js.StringBuffer()
    var ch = this.srcbfCharAt(i)
    while (ch != '\u0000' && ch != ')') {
      sb.appendChar(ch)
      i += 1
      ch = this.srcbfCharAt(i)
    }
    val s = sb.toJString
    if (ch != ')') {
      /* put everything back */
      this.linebf.append("$(")
      this.linebf.appendString(s)
      return i
    }
    i += 1
    var x = env.config.equationJS(s)
    if (x == null) {
      x = js.jstrEmpty
    }
    this.linebf.appendString(x)
    i
  }

  def simple(): XString = {
    var z = this.term()
    var exit = false
    while (!exit) {
      if (this.getSymbol == '+') {
        this.next()
        val y = this.term()
        assert(z != null)
        z = z.concat(y)
      } else if (!this.isSymbolIdent) {
        exit = true
      } else if (this.token.equals2("OR")) {
        val zv = this.boolean(z)
        this.next()
        val sv = this.skip
        this.skip = zv
        val y = this.term()
        this.skip = sv
        if (zv || this.boolean(y)) {
          z = jstrTrue
        } else {
          z = jstrFalse
        }
      } else {
        exit = true
      }
    }
    z
  }

  def term(): XString = {
    var z = this.factor()
    var exit = false
    while (!exit) {
      if (!this.isSymbolIdent) {
        exit = true
      } else if (this.token.equals2("AND")) {
        val zv = this.boolean(z)
        this.next()
        val sv = this.skip
        this.skip = !zv
        val y = this.factor()
        this.skip = sv
        if (zv && this.boolean(y)) {
          z = jstrTrue
        } else {
          z = jstrFalse
        }
      } else {
        exit = true
      }
    }
    z
  }

  def factor(): XString = {
    var z: XString = null

    if (this.getSymbol == '(') {
      this.next()
      z = this.expression()
      if (this.getSymbol != ')') {
        this.syntaxError()
      } else {
        this.next()
      }
    } else if (this.getSymbol == string) {
      z = this.token
      this.next()
    } else if (!this.isSymbolIdent) {
      this.syntaxError()
      z = js.jstrEmpty
    } else if (this.token.equals2("NOT")) {
      this.next()
      val x = this.factor()
      if (!this.boolean(x)) {
        z = jstrTrue
      } else {
        z = jstrFalse
      }
    } else if (this.token.equals2("DEFINED")) {
      this.next()
      if (!this.isSymbolIdent) {
        this.syntaxError()
        return js.jstrEmpty
      }
      var v = env.config.optionJS(this.token)
      v = env.config.res == env.ok
      if (!v) {
        val o = env.config.equationJS(this.token)
        v = env.config.res == env.ok && o != null
      }
      this.next()
      if (v) {
        z = jstrTrue
      } else {
        z = jstrFalse
      }
    } else if (this.skip) {
      z = jstrFalse
      this.next()
    } else {
      var v = env.config.optionJS(this.token)
      if (env.config.res != env.ok) {
        val o = env.config.equationJS(this.token)
        if (o == null) {
          z = js.jstrEmpty
        } else {
          z = o
        }
        if (env.config.res != env.ok) {
          this.notDefined()
        }
      } else if (v) {
        z = jstrTrue
      } else {
        z = jstrFalse
      }
      this.next()
    }
    z
  }

  def expression(): XString = {
    var res: Boolean = false

    var z = this.simple()
    if (this.getSymbol == '=' || this.getSymbol == '#' || this.getSymbol == '<' || this.getSymbol == '>') {
      val op = this.getSymbol
      z = z.toUpperCase
      this.next()
      var y = this.simple()
      y = y.toUpperCase
      if (op == '=') {
        res = z.equals(y)
      } else if (op == '#') {
        res = !z.equals(y)
      } else if (op == '<') {
        res = z.compareTo(y) < 0
      } else {  /*op=">"*/
        res = z.compareTo(y) > 0
      }
      if (res) {
        z = jstrTrue
      } else {
        z = jstrFalse
      }
    }
    z
  }

  def boolean(x: XString): Boolean = {
    if (jstrFalse.equals(x)) {
      false
    } else if (jstrTrue.equals(x)) {
      true
    } else {
      this.typeError()
      false
    }
  }

  def notDefined(): Unit = {
    if (!onScanIf(this.ifs)) {
      return
    }
    this.setError(msg_not_defined)
  }

  def typeError(): Unit = {
    this.setError(msg_type_error)
  }

  def syntaxError(): Unit = {
    this.setError(msg_syntax_error)
  }

  def setError(err: ErrMsg): Unit = {
    this.error = true
    wrongSyntax(this.in, this.lineno, this.getSymbolPos + 1, err)
  }

  def nextAll(): Unit = {
    this.token = this.linebf.toJString.substring(this.getPos).trim()
    this.setPos(this.linebf.length)
  }

  def next(): Unit = {
    val sb = new js.StringBuffer()
    while (CharClass.isWhiteSpace(this.linebfCurChar())) {
      this.advance()
    }
    this.setSymbolPos(this.getPos)
    infiniteLoop {
      val ch = this.linebfCurChar()
      if (ch == '\"') {
        this.advance()
        while (this.linebfCurChar() != '\u0000' && this.linebfCurChar() != '\"') {
          sb.appendChar(this.linebfCurChar())
          this.advance()
        }
        this.token = sb.toJString
        this.setSymbol(string)
        if (this.linebfCurChar() == '\"') {
          this.advance()
        }
        return
      } else if (isLetter(ch)) {
        sb.appendChar(ch)
        this.advance()
      } else {
        if (sb.length == 0) {
          this.setSymbol(ch)
          this.advance()
        } else {
          this.setSymbol(ident)
          this.token = sb.toJString.toUpperCase
        }
        return
      }
    }
  }

  def srcbfCharAt(ps: Int): Char = {
    if (ps < this.srcbf.length) {
      this.srcbf.charAtAsChar(ps)
    } else {
      '\u0000'
    }
  }

  def getLinebfAsString: XString = this.linebf.toJString

  def getSymbolPos: Int = this.syps

  def setSymbolPos(pos: Int): Unit = {
    this.syps = pos
  }

  def isLineEnd: Boolean = this.sy == '\u0000'

  def isSymbolIdent: Boolean = this.sy == ident

  def getSymbol: Char = this.sy

  def setSymbol(ch: Char): Unit = {
    this.sy = ch
  }

  def getPos: Int = this.ps

  def setPos(pos: Int): Unit = {
    this.ps = pos
  }

  def advance(): Unit = {
    this.ps += 1
  }

  def linebfCurChar(): Char = this.linebfCharAt(this.ps)

  def linebfCharAt(ps: Int): Char = {
    if (ps < this.linebf.length) {
      this.linebf.charAtAsChar(ps)
    } else {
      '\u0000'
    }
  }

  /** Returns true to stop iteration */
  def Do(): Boolean = true

}
