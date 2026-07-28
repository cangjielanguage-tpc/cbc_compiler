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
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.ErrorMessages.msg_syntax_error
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.{AbstractProject, DirIterator}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule as xPDB
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.PortableRegCompModule as RegComp
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.{break, loop}
import xscala.util.StringOps.asciiToUpperCase

class ScanFus extends Scan {

  private val FUS_NONE: Int = 0
  private val FUS_METHODS: Int = 1
  private val FUS_FIELDS: Int = 2
  private val FUS_CLASSES: Int = 3
  private val FUS_PACKAGES: Int = 4
  private val FUS_JARS: Int = 5

  var project: AbstractProject[_] = _
  private var parseState: Int = _
  private var pos: Int = _

  override def preprocessor(): Boolean = {
    this.pos = 0
    this.skipws()
    if (this.srcbfCharAt(this.pos) == '#' || this.srcbfCharAt(this.pos) == '\u0000') {
      return false
    }
    if (this.srcbfCharAt(this.pos) == '[') {
      if (this.isSection("[METHODS]", FUS_METHODS) || this.isSection("[FIELDS]", FUS_FIELDS) || this.isSection("[CLASSES]", FUS_CLASSES) || this.isSection("[PACKAGES]", FUS_PACKAGES) || this.isSection("[JARS]", FUS_JARS)) {
        return false
      }
    } else {
      this.parseState match {
        case FUS_METHODS |
             FUS_FIELDS |
             FUS_CLASSES =>
          return this.onMethodsFieldsOrClasses(this.parseState)
        case FUS_PACKAGES =>
          return this.onPackages()
        case FUS_JARS =>
          wrongSyntax(this.in, this.lineno, this.pos + 1, ErrMsg522)
          return true
        case _ =>
      }
    }
    wrongSyntax(this.in, this.lineno, this.pos + 1, msg_syntax_error)
    true
  }
  //  ScanFus

  private def makeLookupPattern(type0: String, def0: String): XString = {
    val ext = env.config.equation(type0)
    if (ext != null) {
      js.format("*.%S", ext)
    } else {
      js.format("*.%s", def0)
    }
  }

  def onPackages(): Boolean = {
    //
    var d: DirIterator = new DirIterator()

    val jstr = this.getString
    var exc: XString = null
    if (jstr == null) {
      return true
    }
    this.skipws()
    if (this.srcbfCharAt(this.pos) == '*') {
      this.pos += 1
      exc = this.getString
      if (exc == null || !exc.equals2("Exceptions")) {
        wrongSyntax(this.in, this.lineno, this.pos + 1, msg_syntax_error)
        return true
      }
    }
    if (this.eolExpected()) {
      return true
    }
    d.recurse = true
    d.p = this.project
    var lookupDirPattern = makeLookupPattern("SYM", "sym")      // lookupDirPattern := '*.sym'
    var compRes = RegComp.compile(lookupDirPattern)
    d.pat = compRes.reg
    val place = xPDB.manager.findDirectory(jstr, xPDB.ContentType.SYM)
    if (place != null) {
      d.dir = place.getFileDescriptor
    } else {
      d.dir = xfs.sys.lookupDir(jstr, lookupDirPattern)
    }
    d.name = jstr
    d.mode = 1
    if (!d.dir.exists) {
      env.errors.fault(ErrMsg467, jstr)
    }
    while (d.dir != null) {
      if (d.dir.iterateDir(d)) {
      }
      d.dir = d.dir.next
    }
    lookupDirPattern = makeLookupPattern("JAVABC", "class") // lookupDirPattern := '*.class'
    compRes = RegComp.compile(lookupDirPattern)
    d.pat = compRes.reg
    d.dir = xfs.sys.lookupDir(jstr, lookupDirPattern)
    d.name = jstr
    d.mode = 2
    while (d.dir != null) {
      if (d.dir.iterateDir(d)) {
      }
      d.dir = d.dir.next
    }
    false
  }
  //

  def onMethodsFieldsOrClasses(what: Int): Boolean = {
    this.skipws()
    val pos0 = this.pos
    val jstr = this.getString
    if (jstr == null) {
      return true
    }
    val doError = env.config.option("error_on_invalid_fus_entry")
    what match {
      case FUS_METHODS |
           FUS_FIELDS =>
        // env.info.print('**fus: M/F   :  "%S"\n', jstr);
        val position = jstr.indexOf('.')
        if (position != -1) {
          if (position == 0) {
            wrongSyntax(this.in, this.lineno, pos0 + 1, msg_syntax_error) // unexpected '.' (at the 1st position)
            return true
          } else {
            this.project.findClassAndAppend(jstr.substring(0, position), doError)
          }
        } else {
          wrongSyntax(this.in, this.lineno, pos0 + jstr.length, msg_syntax_error) // '.' expected
          return true
        }
      case FUS_CLASSES =>
        // env.info.print('**fus: Class :  "%S"\n', jstr);
        this.project.findClassAndAppend(jstr, doError)
    }
    this.eolExpected()
  }

  def isSection(str: String, st: Int): Boolean = {
    var i = 0
    val length = str.length
    while (i < length) {
      if (str(i) != this.srcbfCharAt(i + this.pos).asciiToUpperCase) {
        return false
      }
      i += 1
    }
    while (CharClass.isWhiteSpace(this.srcbfCharAt(i + this.pos))) {
      i += 1
    }
    this.pos = i + this.pos
    if (this.srcbfCharAt(this.pos) == '\u0000') {
      this.parseState = st
      true
    } else {
      false
    }
  }

  def eolExpected(): Boolean = {
    this.skipws()
    if (this.srcbfCharAt(this.pos) == '\u0000') {
      return false
    }
    wrongSyntax(this.in, this.lineno, this.pos + 1, msg_syntax_error)
    true
  }

  def getString: XString = {
    // "string" or string up to space or '*' (length(string) must be > 0)
    var beg: Int = 0

    this.skipws()
    var c = this.srcbfCharAt(this.pos)
    if (c != '\'' && c != '\"') {
      c = '\u0000'
      beg = 0
    } else {
      beg = 1
    }
    var i = beg
    loop {
      if (c != '\u0000') {
        if (this.srcbfCharAt(this.pos + i) == c) {
          break()
        }
        if (this.srcbfCharAt(this.pos + i) == '\u0000') {
          this.pos = this.pos + i
          wrongSyntax(this.in, this.lineno, this.pos + 1, msg_syntax_error)
          return null
        }
      } else if (CharClass.isWhiteSpace(this.srcbfCharAt(this.pos + i)) || this.srcbfCharAt(this.pos + i) == '*' || this.srcbfCharAt(this.pos + i) == '\u0000') {
        break()
      }
      i += 1
    }
    if (i == beg) {
      this.pos = this.pos + beg
      wrongSyntax(this.in, this.lineno, this.pos + 1, msg_syntax_error)
      return null
    }
    val str = this.srcbf.toJString.substring(beg + this.pos, i + this.pos)
    this.pos = this.pos + i
    if (c != '\u0000') {
      this.pos += 1
    }
    str
  }

  def skipws(): Unit = {
    while (CharClass.isWhiteSpace(this.srcbfCharAt(this.pos))) {
      this.pos = this.pos + 1
    }
  }

  def initParse(): Unit = {
    this.parseState = FUS_NONE
  }

}
