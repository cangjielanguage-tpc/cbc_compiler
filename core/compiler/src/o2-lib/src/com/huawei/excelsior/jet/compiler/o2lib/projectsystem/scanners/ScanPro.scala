/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.projectsystem.scanners

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as CharClass
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.CPEntryModes.{cpe_app, cpe_equinox, cpe_error}
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.ErrorMessages.{configResToMsg, msg_error_in_file, msg_syntax_error}
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.MemorySizeParser.parseMemorySize
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.{AbstractProject, CPEntryModes, DirIterator}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xcModesModule as xcModes, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, PortableRegCompModule as RegComp}
import xscala.util.StringOps.asciiToUpperCase
import xscala.util.UByte

class ScanPro(var project: AbstractProject[_] = null) extends Scan {

  private val LOOKUP: String = "LOOKUP"
  val COMPILERHEAP: String = "COMPILERHEAP"
  private val HEAPLIMIT: String = "HEAPLIMIT"

  private val msg_equ_after_mod = ErrMsg459

  private val DirectiveStrs: Array[String] = Array[String](
    "!MODULE ",
    "!PACKAGE ",
    "!ONLYPACKAGE ",
    "!BATCH",
    "!BATCHNOREC",
    "!DIRECTORY",
    "!PUSH",
    "!POP",
    "!CLASSPATHENTRY",
    "!BUNDLEENTRY",
    "!CLASSLOADERENTRY",
    "!END",
    "None",
  )

  // Type DirectiveStr
  type Directive = UByte
  val Module: Directive = UByte(0)
  val Package: Directive = UByte(1)
  val OnlyPackage: Directive = UByte(2)
  val Batch: Directive = UByte(3)
  val BatchNoRec: Directive = UByte(4)
  val Directory: Directive = UByte(5)
  val Push: Directive = UByte(6)
  val Pop: Directive = UByte(7)
  val ClasspathEntry: Directive = UByte(8)
  val BundleEntry: Directive = UByte(9)
  val ClassloaderEntry: Directive = UByte(10)
  val CPEnd: Directive = UByte(11)
  val None: Directive = UByte(12)

  var config: Boolean = false     /* in  */
  private var options: Boolean = true     /* in  */
  private var push: Boolean = false     /* in  */
  var isPDBOpened: Boolean = false
  private[projectsystem] var backslashconcatlines: Boolean = env.config.option("backslashconcatlines")
  private var nonspace: Int = _ /* position of first nonspace character in line */

  override def Do(): Boolean = {
    if (this.isEmptyLine) { // nothing todo
      return false
    }

    var currDirective = this.getDirective

    currDirective match {
      case Push =>
        if (this.push || this.cpentry != null) {
          wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
        }
        this.ensureOpenedPDB()
        this.push = true
        this.options = true
        env.config.push()
        return false
      case ClasspathEntry |
           BundleEntry |
           ClassloaderEntry =>
        if (this.cpentry != null || this.push) {
          wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
        }
        this.ensureOpenedPDB()
        if (currDirective == ClassloaderEntry) {
          val args = this.getDirectiveArgs(2)
          this.cpentry = args(1)
          this.cpentrymode = CPEntryModes.toCpeMode(args(0))
          if (this.cpentrymode == cpe_error) {
            project.setErr()
            return false
          }
        } else {
          val args = this.getDirectiveArgs(1)
          this.cpentry = args(0)
          if (currDirective == ClasspathEntry) {
            this.cpentrymode = cpe_app
          } else {
            this.cpentrymode = cpe_equinox
          }
        }
        this.options = true
        env.config.push()
        return false
      case Module |
           Package |
           Batch |
           BatchNoRec |
           OnlyPackage |
           Directory |
           CPEnd =>
        if (this.config) {
          wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
          return true
        } else if (this.project == null) {
          return false
        } else {
          this.options = false
        }
      case _ =>
    }

    // fall through
    if (this.options) {
      var name = env.config.parse(this.getLinebfAsString)
      if (env.config.res > env.ok) {
        this.optMessage(env.config.res, name)
      } else if (env.config.res == env.isEquation && name.equals2(LOOKUP)) {
        name = env.config.equation(LOOKUP)
        val ps = xfs.sys.parseRed(name)
        if (ps >= 0) {
          wrongSyntax(this.in, this.lineno, ps, msg_syntax_error)
        }
      } else if (env.config.res == env.isEquation && name.equals2(COMPILERHEAP)) {
        val compilerheap = parseMemorySize(env.config.equation(COMPILERHEAP))
        if (compilerheap >= 0) {
          // we have no convenient way to control compiler heap yet
        } else {
          wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
          env.exit(msg_syntax_error.no)
        }
      } else if (env.config.res == env.isEquation && name.equals2(HEAPLIMIT)) {
        val heaplimit = parseMemorySize(env.config.equation(HEAPLIMIT))
        if (heaplimit < 0) {
          wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
          env.exit(msg_syntax_error.no)
        }
      }
    } else {
      assert(this.project != null)

      currDirective = this.getDirective

      currDirective match {
        case Pop =>
          if (!this.push) {
            wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
          }
          this.push = false
          this.options = false
          env.config.pop()
          return false
        case CPEnd =>
          if (this.cpentry == null) {
            wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
          }

          if (this.cpentrymode == cpe_app) {
            this.project.appendClasspathEntry(this.cpentry)
          } else {
            this.project.appendClassloaderEntry(this.cpentry, this.cpentrymode, bidInInternalForm = false, userDef = true)
          }

          this.cpentry = null
          this.options = false
          env.config.pop()
          return false
        case _ =>
      }

      // fall through
      if (this.cpentry != null) {
        wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
      }

      this.ensureOpenedPDB()

      if (!appendModules(this.project, this.linebf.toJString)) {
        wrongSyntax(this.in, this.lineno, -1, msg_syntax_error)
      }
    }
    false
  }

  private def appendModules(p: AbstractProject[_], s: XString): Boolean = {
    var d: DirIterator = new DirIterator()

    val st = new CustomStringTokenizer(s, 0)
    val ch = st.skipWhiteSpaces()
    if (st.hasMoreTokens) {
      if (ch == '!') {
        var w = st.nextWord(cap = true)
        if (w.equals2("MODULE")) {
          w = st.nextUntilEnd()
          if (!js.jstrEmpty.equals(w)) {
            p.appendFile(w)
          }
        } else if (w.equals2("BATCH") || w.equals2("BATCHNOREC") || w.equals2("PACKAGE") || w.equals2("ONLYPACKAGE") || w.equals2("DIRECTORY")) {
          val dir = w.equals2("DIRECTORY")
          d.mode = 0
          d.recurse = w.equals2("BATCH") || w.equals2("PACKAGE") || dir
          val r = st.nextWord(cap = false)
          if (!js.jstrEmpty.equals(r)) {
            val compRes = RegComp.compile(r)
            d.pat = compRes.reg
            if (d.pat != null) {
              w = st.nextUntilEnd()
              if (!js.jstrEmpty.equals(w)) {
                if (dir) {
                  d.dir = xfs.sys.createFileDescriptor(w)
                  if (!d.dir.exists) {
                    env.errors.message(ErrMsg461, w, js.jstrEmpty)
                    return false
                  }
                  d.name = js.jstrEmpty
                } else {
                  d.dir = xfs.sys.lookupDir(w, r)
                  d.name = w
                }
                d.p = p
                while (d.dir != null) {
                  if (!d.dir.exists) {
                    env.errors.envError(ErrMsg467, w)
                    p.setErr()
                    return false
                  }
                  env.info.print("Reading %S ...\\n", d.dir.getName)
                  if (d.dir.iterateDir(d)) {
                  }
                  d.dir = d.dir.next
                }
              }
            }
          }
        } else {
          return false
        }
      } else if (ch == '-' || ch == '+') {
        env.errors.fault(msg_equ_after_mod, s)
      } else {
        assert(ch != '\u0000')
        return ch == '%'
      }
    }
    true
  }

  // if path starts with / on Windows then it is treated as "root of the current drive"
  // if path to imported project file is full, then project file can reside on other disk
  // get disc name and append to outputdir
  def ensureOpenedPDB(): Unit = {
    if (!this.isPDBOpened) {
      this.project.openPDB()
      this.isPDBOpened = true
    }
  }

  /*
  ** Foreach d in ds calls isDirective,
  ** if call succeeds returns d, if none succeeds returns None
  */
  def getDirective: Directive = {
    for (d <- Module to CPEnd) {
      if (this.isDirective(d)) {
        return d
      }
    }
    None
  }

  def isEmptyLine: Boolean = {
    this.nonspace = 0

    while (CharClass.isWhiteSpace(this.linebfCharAt(this.nonspace))) {
      this.nonspace += 1
    }
    if (this.linebf.length == this.nonspace || this.linebfCharAt(this.nonspace) == '%') {
      return true
    }

    false
  }

  /*
  ** Checks whether current line contains given directive (# None) (starting from pos) or not
  ** Returns true on success
  */
  def isDirective(d: Directive): Boolean = {
    assert(d != None)
    var i = 0
    var j = this.nonspace
    val dir = DirectiveStrs(d.toInt)

    while (i < dir.length) {
      if (this.linebfCharAt(j).asciiToUpperCase != dir(i)) {
        return false
      }
      i += 1
      j += 1
    }

    true
  }

  def optMessage(res: Int, name: XString): Unit = {
    val err = configResToMsg(res)
    var msg = env.errors.getMsg(err)
    if (err != msg_syntax_error) {
      msg = js.format(js.TODO2(msg), js.TODO2(name))
    }
    val fname = FS.HOST.toPlatform(this.in.getName)
    if (err == ErrMsg320 || err == ErrMsg322) {
      // suppress silently in Cangjie
    } else {
      env.errors.silentMessage(msg_error_in_file, fname, this.lineno, msg)
    }
  }

  private class CustomStringTokenizer (str: XString, var pos: Int) {

    def nextUntilEnd(): XString = {
      val res = this.str.substring(this.pos).trim()
      this.pos = this.str.length // invalidate tokenizer, we do not need it later
      if (res.isEmpty) {
        return res
      }
      val q = res.charAt(0)
      if (q == '\"' || q == '\'') {
        var pos = res.indexOf(q, 1)
        if (pos <= 0) {
          pos = res.length
        }
        res.substring(1, pos)
      } else {
        res
      }
    }

    def nextWord(cap: Boolean): XString = {
      val sb = new js.StringBuffer()
      val q = this.skipWhiteSpaces()
      if (q == '\"' || q == '\'') {
        var c = this.nextChar()
        while (this.hasMoreTokens && c != q) {
          if (cap) {
            c = c.asciiToUpperCase
          }
          sb.appendChar(c)
          c = this.nextChar()
        }
      } else {
        var c = q
        while (this.hasMoreTokens && !CharClass.isWhiteSpace(c)) {
          if (cap) {
            c = c.asciiToUpperCase
          }
          sb.appendChar(c)
          c = this.nextChar()
        }
      }
      sb.toJString
    }

    /** skips whitespaces
        RETURN: first nonspace character: after whitespaces
      */
    def skipWhiteSpaces(): Char = {
      if (!this.hasMoreTokens) {
        return '\u0000'
      }
      var ch = this.nextChar()
      while (this.hasMoreTokens && CharClass.isWhiteSpace(ch)) {
        ch = this.nextChar()
      }
      if (!this.hasMoreTokens) {
        return '\u0000'
      }
      ch
    }

    def nextChar(): Char = {
      val ch = this.str.charAtAsChar(this.pos)
      this.pos += 1
      ch
    }

    def hasMoreTokens: Boolean = this.pos < this.str.length

  }

  def getDirectiveArgs(numOfArgs: Int): Array[XString] = {
    val st = new CustomStringTokenizer(this.linebf.toJString, this.nonspace)
    assert(st.nextChar() == '!')
    assert(numOfArgs > 0)
    st.nextWord(cap = false) // skip directive
    val args = new Array[XString](numOfArgs)
    for (j <- 0 to numOfArgs - 2) {
      args(j) = st.nextWord(cap = true)
    }
    args(numOfArgs - 1) = st.nextUntilEnd()
    args
  }

}
