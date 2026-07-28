/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.driver.O2LibFatalError
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as cc
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xcModesModule as xcModes, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, MemoryManagementModule as mm}
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.io.{TextOutput, stderr, stdout}
import xscala.util.StringOps.asciiToUpperCase
import xscala.util.UInt

object xmErrorsModule {

  private class Info extends env.Info {
    private[xmErrorsModule] var starttime: UInt = _ /* time */
    private[xmErrorsModule] var totaltime: UInt = _

    override def reset(): Unit = {
      super.reset()
      this.totaltime = UInt(0)
    }

    override def report(): Unit = {
      assert(showed)
      showed = false
      var t = env.time() - this.starttime
      if (t <= UInt(0)) {
        t = UInt(0)
      }
      if (env.decor contains env.dc_report) {
        if (!(env.decor contains env.dc_header) && this.filename != null) {
          this.print("%S\\n  ", this.filename)
        }
        this.print("; bytes(%d), time %2d.%02d", this.lines, (t / UInt(100)).toInt, (t % UInt(100)).toInt)
        if (this.newSF) {
          this.print(", new symfile")
        }
        this.print("\\n")
      }
      this.totaltime = this.totaltime + t
    }

    override def header(): Unit = {
      var nm: XString = null
      var xixi: Int = 0

      //  i.module:=NIL;
      this.lines = 0
      this.starttime = env.time()
      if (this.filename == null) {
        nm = js.jstrEmpty
      } else {
        nm = this.filename
      }
      var maxnum = makeobjClassAmount
      if (maxnum < jarsClassAmount) {
        maxnum = jarsClassAmount
      }
      if (maxnum < cacheClassAmount) {
        maxnum = cacheClassAmount
      }
      if (env.decor contains env.dc_header) {
        if (env.stage == env.FRONT) {
          this.print("%4d/%d: %S\\n", classAmount, maxnum, nm)
        } else {
          if (classAmount <= 0) {
            xixi = 0
          } else {
            xixi = O2JSupport.div((classAmount - backendCounter) * 100, classAmount)
          }
          if (this.worker == 0) {
            this.print("%3d%% done, %d/%d to go: %S\\n", xixi, backendCounter, classAmount, nm)
          } else if (xcModes.workerMode) {
            this.print("%S\\n", nm)
          } else {
            this.print("\\n%3d%% done, %d/%d to go: %S sent to worker %d\\n", xixi, backendCounter, classAmount, nm, this.worker)
          }
        }
      }
    }
  }

  private class Node {
    private[xmErrorsModule] var msg: XString = _
    private[xmErrorsModule] var fnm: XString = _
    private[xmErrorsModule] var l: Int = _
    private[xmErrorsModule] var type0: Char = _
    private[xmErrorsModule] var msgno: Int = _
    private[xmErrorsModule] var next: Node = _

    def lss(fnm: XString, l: Int): Boolean = {
      assert(this != null)
      if (fnm == null) {
        return true
      }
      if (this.fnm == null) {
        return false
      }
      val c = this.fnm.compareTo(fnm)
      if (c != 0) {
        return c < 0
      }
      this.l < l
    }
  }


  private class Errors extends env.Errors {
    private[xmErrorsModule] var fmt: XString = _  /* ERRFMT equation value                */
    private[xmErrorsModule] var node: Node = _    /* sorted list of error messages        */
    private[xmErrorsModule] var trap: Boolean = _ /* exception trap is available          */
    private[xmErrorsModule] var errno: Int = _

    override def reset(): Unit = {
      super.reset()
      this.node = null
      this.trap = false
    }

    override def getMsg(err: ErrMsg): XString = {
      if (isWorkMode) {
        if (err == ErrMsg950) { // not enough mem
          printMem(doPrintErr = false)
        }
      }
      this.errno = err.no
      js.newJString(err.format)
    }

    override def printMsg(type0: Char, fmt: XString, msgno: Int, x: Any*): Unit = {
      var errorMsg: XString = null
      if (type0 == 'm') {
        env.info.forcePrint(js.TODO2(fmt), x: _*)
        env.info.forcePrint("\\n")
      } else {
        val l = Int.MaxValue
        val str = js.format(js.TODO2(fmt), x: _*)
        //      ModifyCounters(e,type);
        if (type0 == 'v') { /* the same as 'm' for non-shell */
          env.info.forcePrint(js.TODO2(fmt), x: _*)
          env.info.forcePrint("\\n")
        } else {
          if (this.fmt == null) {
            checkFormat(this)
          }
          val formatter = newMessageFormatter(this.fmt, str)
          formatter.formatMsg(type0, null, l, 0, this.errno)
          errorMsg = formatter.buf.toJString
          if (insert(this, null, l, type0, errorMsg, msgno)) {
            modifyCounters(this, type0)
          }
        }
      }
      this.errno = 0
      if (!this.trap) {
        showErrors()
      }
      var str = env.config.equation("ERRORLEVEL")
      var el = false
      if (str != null) {
        str = str.toUpperCase
        el = str.indexOf(type0.asciiToUpperCase) != -1
      }
      if (type0 == 'f' || el) {
        showErrors()
        if (!xcModes.workerMode) {
          env.exit(3)
        } else {
          assert(errorMsg != null)
          throw new O2LibFatalError(errorMsg.toString)
        }
      }
    }

    override def execute(action: => Unit): Unit = {
      try {
        val oldtrap = trap
        trap = true
        action
        trap = oldtrap
      } catch {
        case e: Throwable =>
          if (!env.config.option("XDEBUG")) {
            trap = false
            showErrors()
            var isNoMem = false
            stdout.printStackTrace(e)
            e match {
              case _: OutOfMemoryError =>
                isNoMem = true
                mm.compactHeap()
                printMem(doPrintErr = false)
              case _ =>
                abortCompilation(e)
            }
            if (!isNoMem && !xcModes.workerMode) {
              env.exit(1)
            }
          }
          throw e
      }
    }

    override def showErrors(): Unit = {
      xmErrorsModule.showErrors(this)
    }
  }

  /*----------------------------------------------------------------*/

  private class MessageFormatter {

    private[xmErrorsModule] var fmt: XString = _ // format string that is used to format message
    private[xmErrorsModule] var msg: XString = _ // message to format
    private[xmErrorsModule] var pos: Int = _ // current pos in "fmt" format string
    private[xmErrorsModule] var subfmt: XString = _ // sub format for the next message token
    private[xmErrorsModule] var arg: XString = _ // next message token
    private[xmErrorsModule] var buf: js.StringBuffer = null // current status of message formatting
    private[xmErrorsModule] var errpos: Int = _ // position of wrong syntaz in "fmt" format string

    def formatMsg(type0: Char, fnmPar: XString, l: Int, c: Int, errno: Int): Unit = {
      var fnm = fnmPar

      if (!this.hasMoreChars) {
        this.errpos = 0
        return
      }
      while (this.hasMoreChars) {
        this.skipWS()
        this.parseArgument()
        if (this.errpos >= 0) {
          return
        } else {
          val arg = this.arg.toUpperCase
          val fmt = js.TODO2(this.subfmt)
          if (arg.isEmpty) {
            this.buf.appendf(fmt)
          } else if (arg.equals2("LINE")) {
            if (l != Int.MaxValue) {
              this.buf.appendf(fmt, l)
            }
          } else if (arg.equals2("INLINECONTEXT")) {
          } else if (arg.equals2("COLUMN")) {
            if (c != Int.MaxValue) {
              this.buf.appendf(fmt, c + 1)
            }
          } else if (arg.equals2("ERRNO")) {
            this.buf.appendf(fmt, errno)
          } else if (arg.equals2("ERRMSG")) {
            this.buf.appendf(fmt, js.TODO2(this.msg))
          } else if (arg.equals2("FILE")) {
            if (fnm != null) {
              val pn = FS.HOST.toPlatform(fnm)
              this.buf.appendf(fmt, js.TODO2(pn))
            } else {
              this.buf.appendf(fmt, "***")
            }
          } else if (arg.equals2("MODULE")) {
            if (env.info.module == null) {
              this.buf.appendf(fmt, "***")
            } else {
              this.buf.appendf(fmt, js.TODO2(env.info.module.name))
            }
          } else if (arg.equals2("MODE")) {
            if (type0 == 'f') {
              this.buf.appendf(fmt, "FAULT")
            } else if (type0 == 'm') {
              this.buf.appendf(fmt, "MESSAGE")
            } else {
              throw new AssertionError
            }
          } else if (arg.equals2("LANGUAGE")) {
            this.buf.appendf(fmt, "Java")
          } else if (arg.equals2("UTILITY")) {
            val pn = env.args.programName
            this.buf.appendf(fmt, js.TODO2(pn))
          } else {
            this.errpos = this.pos
          }
        }
      }
    }

    def parseArgument(): Unit = {
      this.subfmt = js.jstrEmpty
      this.skipWS()
      var ch = this.curChar()
      if (ch == '\'' || ch == '\"') {
        this.pos += 1
        val p = this.pos
        while (this.hasMoreChars && this.curChar() != ch) {
          this.pos += 1
        }
        if (!this.hasMoreChars) {
          this.errpos = this.pos
          return
        }
        this.subfmt = this.fmt.substring(p, this.pos)
        this.pos += 1
      } else {
        this.errpos = this.pos
        return
      }
      this.skipWS()
      val argbuf = new js.StringBuffer()
      if (this.curChar() == ',') {
        loop {
          this.pos += 1
          if (!this.hasMoreChars) {
            break()
          }
          ch = this.curChar()
          if (cc.isLetter(ch) || cc.isNumeric(ch)) {
            argbuf.appendChar(ch)
          } else {
            break()
          }
        }
      }
      this.arg = argbuf.toJString
      this.skipWS()
      if (this.curChar() == ';') {
        this.pos += 1
      } else if (this.hasMoreChars) {
        this.errpos = this.pos
      }
    }

    def skipWS(): Unit = {
      while (this.hasMoreChars && cc.isWhiteSpace(this.curChar())) {
        this.pos += 1
      }
    }

    // curChar in fmt
    def curChar(): Char = {
      if (this.hasMoreChars) {
        this.fmt.charAtAsChar(this.pos)
      } else {
        '\u0000'
      }
    }

    // has more chars in fmt
    def hasMoreChars: Boolean = this.pos < this.fmt.length

  }

  /* ERRFMT */
  private val defaultErrFmt: String = "\"(%s\",file;\" %d\",line; \",%d\",column;\") [%.1s] \",mode; \"%s\\n\",errmsg;"
  private var showed: Boolean = false
  /*----------------------------------------------------------------*/
  var backendCounter: Int = _
  var classAmount: Int = _
  var parsedClassAmount: Int = _
  var makeobjClassAmount: Int = _
  var jarsClassAmount: Int = 0
  var cacheClassAmount: Int = 0

  private def showLine(s: XString): Unit = {
    /* print source text line, truncate if it is needed */
    val max: Int = 72
    var end: Int = 0

    if (max < s.length) {
      end = max
    } else {
      end = s.length
    }
    env.info.forcePrint("#%S", s.substring(0, end))
    if (max < s.length) {
      env.info.forcePrint("...")
    }
    env.info.forcePrint("\\n")
  }

  private def readLine(f: xfs.TextFile, linePar: Int, l: Int): XString = {
    var line = linePar

    while (line < l - 1) {
      val ln = f.readLine()
      if (ln == null) {
        return null
      } else {
        line += 1
      }
    }
    f.readLine()
  }

  private def showErrors(e: Errors): Unit = {
    var x: XString = null

    showed = true
    var n = e.node
    e.node = null
    var f: xfs.TextFile = null
    var fnm: XString = null
    var lineno = 0
    var line: XString = null

    while (n != null) {
      if (fnm != n.fnm) {
        if (f != null) {
          f.close()
        }
        f = null
        fnm = null
        if (n.fnm != null) {
          f = xfs.text.openToRead(n.fnm)
          if (f != null) {
            fnm = n.fnm
          }
        }
        lineno = 0
        line = null
      }
      x = n.msg
      e.lastError = x
      if (!env.config.option("SILENT")) {
        env.info.forcePrint("%S", x)
      }
      if (f != null && n.l > 0) {
        if (lineno < n.l) {
          line = readLine(f, lineno, n.l)
          lineno = n.l
        }
        if (!env.config.option("SILENT") && line != null) {
          showLine(line)
        }
      }
      n = n.next
    }
    if (f != null) {
      f.close()
    }
  }

  def abortCompilation(e: Throwable): Nothing = {
    val msg = TextOutput.asString(_.printStackTrace(e))
    env.errors.fault(ErrMsg450, msg)
  }

  def printMem(doPrintErr: Boolean): Unit = {
    if (doPrintErr) {
      stderr.println()
      stderr.print("------------------------ OUT OF MEMORY ------------------------")
      // we have no convenient way to control compiler heap yet
    }
    mm.printMem()
  }

  private def newMessageFormatter(errfmt: XString, msg: XString): MessageFormatter = {
    val this0 = new MessageFormatter()
    this0.fmt = errfmt
    this0.msg = msg
    this0.pos = 0
    this0.errpos = -1
    this0.buf = new js.StringBuffer()
    this0
  }

  private def checkFormat(e: Errors): Unit = {
    e.fmt = env.config.equation("ERRFMT")
    if (e.fmt != null) {
      val formatter = newMessageFormatter(e.fmt, js.jstrEmpty)
      formatter.formatMsg('m', null, 0, 0, 0)
      if (formatter.errpos >= 0) {
        env.errors.message(ErrMsg431, formatter.errpos)
        e.fmt = null
      }
    }
    if (e.fmt == null) {
      e.fmt = js.newJString(defaultErrFmt)
    }
  }

  private def insert(e: Errors, fnm: XString, l: Int, type0: Char, msg: XString, msgno: Int): Boolean = {
    var x = e.node
    var p: Node = null
    while (x != null && x.lss(fnm, l)) {
      p = x
      x = x.next
    }
    //  IF x #NIL THEN
    //    env.info.print("x.l = %d, x.c = %d, x.type = %c, x.msg = (start)\n%S(end)\n", x.l, x.c, x.type, x.msg);
    //  END;
    //  env.info.print("count = %d\n",  count);
    //  INC(count);
    //  env.info.print("l   = %d, c   = %d, type   = %c, msg   = (start)\n%S(end)\n",  l,  c,  type,  msg);
    if (x != null && x.fnm != null && fnm != null && fnm.equals(x.fnm) && x.l == l && x.type0 == type0 && msg.equals(x.msg)) {
      // kevin: msg is quite enough
      return false
    }
    val n = new Node()
    n.fnm = fnm
    n.l = l
    n.type0 = type0
    n.msg = msg
    n.msgno = msgno
    if (p == null) {
      n.next = e.node
      e.node = n
    } else {
      n.next = x
      p.next = n
    }
    true
  }

  private def modifyCounters(e: Errors, type0: Char): Unit = {
    if (type0 == 'f') {
      e.errDetected = true
    } else {
      assert(type0 == 'v' || type0 == 'm' || type0 == 'n')
    }
  }

  private def init(i: Info): Unit = {
    i.totaltime = UInt(0)
  }

  /*----------------------------------------------------------------*/
  def setManagers(): Unit = {
    val i = new Info()
    i.reset()
    init(i)
    env.info = i
    val e = new Errors()
    e.fmt = null
    e.reset()
    env.errors = e
  }
}
