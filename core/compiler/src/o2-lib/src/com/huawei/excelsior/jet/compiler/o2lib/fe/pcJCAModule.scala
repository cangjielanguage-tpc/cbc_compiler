/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.fe

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xcMakeModule as mk, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xjRTSModule as rts
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule
import com.huawei.excelsior.jet.compiler.options.BoolOption.HideInjectedFields
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.NonNullableWrapper
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.matching.Pattern
import xscala.util.Set32

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object pcJCAModule {
  /* Equation (!NOTINLINE=smth) */
  private class JCAEquation {
    var name: XString = _ // equation name
    var strl: List[XString] = Nil        // eq. value(s)
    var next: JCAEquation = _
  }

  /* Option (+CHECKINDEX) */
  private class JCAOption {
    var name: XString = _ // option name
    var value: Boolean = _
    var next: JCAOption = _
  }

  private object JCAOption {
    val values = Seq("CHECKNULL", "CHECKRANGE", "GENSTRICTSTACK", "CHECKINDEX", "CHECKARRSTORE", "PGOHOST")
  }

  /* Sections tree */
  class JCATree {
    private[pcJCAModule] var hash: Int = _         // sec. name hash
    private[pcJCAModule] var type0: Char = _            // 'm' or 'g'
    private[pcJCAModule] var name: XString = _ // section name
    private[pcJCAModule] var opts: JCAOption = _
    private[pcJCAModule] var eqs: JCAEquation = _
    private[pcJCAModule] var l: JCATree = _    // tree (sorted by 'hash')
    private[pcJCAModule] var r: JCATree = _
    private[pcJCAModule] var next: JCATree = _
  }

  private class JCAParser {
    private[pcJCAModule] var fnm: XString = _
    private[pcJCAModule] var str: XString = _
    private[pcJCAModule] var pos: Int = _
    private[pcJCAModule] var sec: JCATree = _

    def parseSection(): Unit = {
      var secnam: XString = null

      assert(this.curChar() == '@')
      this.pos += 1
      val type0 = this.curChar()
      if (type0 != 'm' && type0 != 'c' && type0 != 'g') {
        this.logError("bad section type (\'@m\', \'@c\' or \'@g\' expected)")
        return
      }
      this.pos += 1
      while (this.hasMoreInLine && this.curChar() == ' ') {
        this.pos += 1
      }
      // TODO: use trim() here
      if (!this.hasMoreInLine && (type0 == 'm' || type0 == 'c')) {
        this.logError("section name expected")
        return
      } else if (this.hasMoreInLine && type0 == 'g') {
        this.logError("unexpected name in \'g\' section")
        return
      }
      if (!this.hasMoreInLine) {
        secnam = js.jstrEmpty
      } else {
        secnam = this.extr()
      }
      this.sec = findInTree(type0, secnam, mkNew = false)
      if (this.sec != null) {
        this.logWarning("divided section definition")
      } else {
        this.sec = findInTree(type0, secnam, mkNew = true)
        type0 match {
          case 'g' =>
            globSection = this.sec
          case 'm' =>
            noMSection = false
          case 'c' =>
            noCSection = false
        }
      }
    }

    def parseEquation(): Unit = {
      if (this.sec == null) {
        this.logError("section definition expected")
        return
      }
      this.pos += 1
      val b = this.pos
      while (this.hasMoreInLine && this.curChar() != '=' && this.curChar() != ' ') {
        this.pos += 1
      }
      if (this.curChar() != '=') {
        this.logError("unexpected character in equation name")
        return
      }
      if (b == this.pos) {
        this.logError("equation name expected")
        return
      }
      val eqn = this.str.substring(b, this.pos)
      var p = findEqByName(this.sec, eqn)
      if (p == null) {
        p = new JCAEquation()
        p.name = eqn
        p.next = this.sec.eqs
        this.sec.eqs = p
      }
      this.pos += 1
      p.strl = this.extr() :: p.strl
    }

    def parseOption(): Unit = {
      if (this.sec == null) {
        this.logError("section definition expected")
        return
      }
      val ch = this.curChar()
      this.pos += 1
      if (!this.hasMoreInLine || this.curChar() == ' ') {
        this.logError("option name expected")
        return
      }
      val nam = this.extr()
      var p = findOptByName(this.sec, nam)
      if (p == null) {
        p = new JCAOption()
        p.name = nam
        p.next = this.sec.opts
        this.sec.opts = p
      }
      p.value = ch == '+'
    }

    def logWarning(msg: String): Unit = {
      log("*JCA: warning in %S - %s:\\n* %S\\n", this.fnm, msg, this.str)
    }

    def logError(msg: String): Unit = {
      forceLog("*JCA: error in %S - %s:\\n* %S\\n", this.fnm, msg, this.str)
    }

    def hasMoreInLine: Boolean = this.pos < this.str.length

    def curChar(): Char = this.str.charAtAsChar(this.pos)

    def extr(): XString = {
      // TODO: replace by trim()
      val r = this.str.substring(this.pos)
      var i = r.length
      while (i > 0 && r.charAt(i - 1) == ' ') {
        i -= 1
      }
      r.substring(0, i)
    }

  }

  //-----------------------------------------------------------------
  ////////// S t r i n g   t a b l e ////////////////////////////////


  /* Adds a string to the `tree` string tree
     if mkNew - a new line will always be added,
     otherwise, if there is already a line, it will find it; if not, it will add a new one.
     Returns strentry */
  /* Finds string in the string tree */
  ////////// S t r i n g   t a b l e ////////////////////////////////
  //-----------------------------------------------------------------
  /*-----------------------------------------------------------------
    Field tree
  -------------------------------------------------------------------*/

  private class FieldDesc {
    private[pcJCAModule] var name: XString = _
    private[pcJCAModule] var sig: XString = _
    private[pcJCAModule] var accf: Set32 = _
  }

  /*****************************************************************************

  *****************************************************************************/

  private var jcaTree: JCATree = _
  private var globSection: JCATree = _ // 'g' section
  private var externalStrl: List[XString] = Nil
  private var jcaNOTINLINEG: List[XString] = Nil // not inline methods
  private var jcaNOTINLINE_REGEX_G: List[XString] = Nil // not inline methods
  private var jcaALWAYSINLINEG: List[XString] = Nil // always inline methods
  private var jcaALWAYSINLINE_REGEX_G: List[XString] = Nil // always inline methods
  private var jcaNOLOCALGCPOINTSG: List[XString] = Nil // no GC points methods
  private var jcaINLINEWITHCONTEXTPOINTTESTG: List[XString] = Nil
  private var jcaUNROLLLOOPSG: List[XString] = Nil
  private var jcaTURBOCLINITEDG: List[XString] = Nil // classes that are statically intialized before application code execution
  private var jcaCODEADDRTARGETG: List[XString] = Nil
  private var noMSection: Boolean = true // flags to not search sections that do not exist
  private var noCSection: Boolean = true
  val jcaOpt: XString = js.newJString("JCADVISE")
  private val JRE_OVERRIDE: String = "JRE_OVERRIDE"
  private var jreOverrideMap = mutable.Map.empty[XString, XString] // (target class name -> replacement string)
  /*****************************************************************************

  *****************************************************************************/
  private var jcaKnownsafe: List[XString] = _
  private var iniKnownsafe: Boolean = false

  /****************************** Internal MIX **************************************/
  private def log(format: String, x: Any*): Unit = {
    env.info.print(format, x: _*)
  }

  def forceLog(format: String, x: Any*): Unit = {
    env.info.forcePrint(format, x: _*)
  }

  private def calcHash(str: XString): Int = {
    var h = 0
    for (j <- 0 until str.length) {
      h = (31 * h & 536870911) + O2JSupport.convCharToInt(str.charAtAsChar(j)).toShort.toInt
    }
    h & 4095
  }

  // Finds an equation in the section by eqname or returns NIL, if no such equation
  private def findEqByName(section: JCATree, eqname: XString): JCAEquation = {
    if (section == null) {
      return null
    }
    var p = section.eqs
    while (p != null) {
      if (eqname.equals(p.name)) {
        return p
      }
      p = p.next
    }
    null
  }

  // Finds an option in the section by optname or returns NIL, if no such option
  private def findOptByName(section: JCATree, optname: XString): JCAOption = {
    if (section == null) {
      return null
    }
    var p = section.opts
    while (p != null) {
      if (optname.equals(p.name)) {
        return p
      }
      p = p.next
    }
    null
  }

  private def error(msg: XString): Unit = {
    env.errors.fault(ErrMsg450, msg.toString)
  }

  private def parseMethod(sz: XString): pcO.Method = {
    var sig: XString = null
    var name: XString = null

    var method: pcO.Method = null

    // aa/bb/cc.foo(Ljava/lang/String;)  --> class="aa/bb/cc",  name="foo"
    // aa/bb/cc.<init>()                 --> class="aa/bb/cc",  name="<init>"
    // aa/bb/cc.foo                      --> class="aa/bb/cc",  name="foo"
    val dot = sz.lastIndexOf('.')
    if (dot <= 0) {
      return null
    }
    val class0 = sz.substring(0, dot)
    val brPos = sz.indexOf('(', dot)
    if (brPos == -1) {
      name = sz.substring(dot + 1)
      sig = null
    } else {
      name = sz.substring(dot + 1, brPos)
      sig = sz.substring(brPos)
    }

    val cls = pcO.findClass(js.intern(class0))

    if (cls != null) {
      if (!cls.isInActiveEnvironment) {
        return null
      }

      if (sig != null) {
        val msig = O2Env.env.parseMethodSignature(sig)
        method = cls.findLocalMethod(js.intern(name), msig)
      } else {
        method = cls.findLocalMethod(js.intern(name))
      }
    } else if (mk.isFromExcludedPackage(pcNames.newClassName(class0))) {
      return null
    }

    if (cls == null || method == null) {
      error(js.format("JCA entry not found %S", sz))
      return null
      //    ASSERT(FALSE); -- check that all entries in JCA are found: RETURN FALSE;
    }
    method
  }

  /* Checks that method with given name exists.
     If it does, returns the method name with proper signature.
     Otherwise, returns the name as is.
  */
  private def _check_method_name(sz: XString): XString = {
    val method = parseMethod(sz)

    if (method == null) {
      log("*JCA: warning: unknown name \'%S\'\\n", sz)
      return sz
    }

    method.getReadableName(need_class_name = true)
  }

  private def checkClassName(sz: XString): Unit = {
    val cls = pcO.findClass(js.intern(sz))
    if (cls == null) {
      error(js.format("JCA entry not found %S", sz))
    }
  }

  //
  private def _check_opts(pPar: JCAOption): Unit = {
    var p = pPar

    while (p != null) {
      if (!JCAOption.values.exists(option => p.name.equals2(option))) {
        log("*JCA: warning: Unknown option name \'%S\'\\n", p.name)
      }
      p = p.next
    }
  }

  //
  private def _check_eqs(pPar: JCAEquation): Unit = {
    var p = pPar

    while (p != null) {
      if (p.name.equals2("NOTINLINE") || p.name.equals2("INTRINSIC") || p.name.equals2("ALWAYS_INLINE") || p.name.equals2("NO_LOCAL_GC_POINTS") || p.name.equals2("PGO_INLINE_HEAVY") || p.name.equals2("INLINE_WITH_CONTEXT_POINT_TEST") || p.name.equals2("HEAVYSYNC") || p.name.equals2("CODE_ADDR_TARGET")) {
        p.strl = p.strl map _check_method_name

      } else if (p.name.equals2("KNOWN_SAFE")) {
        p.strl = p.strl map { s =>
          if (s.charAt(1) != ',') {
            error(js.newJString("JCA KNOWN_SAFE : expected <digit or \'-\'> \',\' <method name>"))
          }

          val suffix = _check_method_name(s.substring(2))
          s.substring(0, 2).concat(suffix)
        }

      } else if (p.name.equals2("NATIVEUSEFIELDS") || p.name.equals2("INJECT_FIELD") || p.name.equals2("HOT_SWITCH_CASES") || p.name.equals2("NON_NULL_FIELD") || p.name.equals2(JRE_OVERRIDE)) {
        // nothing to do

      } else if (p.name.equals2("TURBO_CLINITED")) {
        p.strl foreach checkClassName

      } else {
        log("*JCA: warning: Unknown equation name \'%S\'\\n", p.name)
      }
      p = p.next
    }
  }

  private def _jca_check(p: JCATree): Unit = {
    if (p == null) {
      return
    }
    if (p.type0 == 'm' && p.name != null) {
      p.name = _check_method_name(p.name)
    }
    _check_opts(p.opts)
    _check_eqs(p.eqs)
    _jca_check(p.l)
    _jca_check(p.r)
    _jca_check(p.next)
  }

  /**********************************************************************************/
  /* Checks JCA tree for consistency
  */
  def JCACheck(): Unit = {
    _jca_fix_signatures(jcaTree)
    _jca_check(jcaTree)
  }

  /* Return TRUE if the string list has 'sz' string
  */
  def isInStrlist(lst: List[XString], sz: XString): Boolean =
    lst contains sz

  /* Return TRUE if the string list has 'sz' string
  */
  def isInRegexStrlist(lst: List[XString], sz: XString): Boolean =
    lst exists { r => sz.toString matches r.toString}

  def convertWildcardToRegex(xs: List[XString]) = {
    // "abc*def" -> "\Qabc\E.*\Qdef\E"
    xs map { x =>
      XString(x.toString.split("\\*", -1).map(Pattern.quote).mkString(".*"))
    }
  }

  /* Finds set of values for the equation or returns NIL
  */
  def getEqValues(section: JCATree, eqname: String): List[XString] = {
    val p = findEqByName(section, js.newJString(eqname))

    if (p == null) {
      return Nil
    }

    p.strl
  }

  /* Finds an option in the section by the name and return its value
     1 - the value is ON
     0 - the value is OFF
    -1 - option was not found
  */
  def getOptValue(section: JCATree, optname: String): Int = {
    val p = findOptByName(section, js.newJString(optname))

    if (p == null) {
      return -1
    }

    if (p.value) {
      return 1
    }

    0
  }

  /* Returns TRUE if the method is in the !EXTERNAL=<fnm> of the global section
  */
  def isExternal(method: pcO.Method): Boolean =
    isInStrlist(externalStrl, method.getReadableName(need_class_name = true, need_full_sign = true))

  /*  Finds (if any) a section of type 'type' ('m' or 'g') named 'name' in
      parsed file and returns it or NIL
      (mkNew is only for this module, otherwise it must be FALSE)
      For a section of type 'g', the name must be ""
  */
  private def findInTreeMkn(p: JCATree, dir: Int, hash: Int, type0: Char, name: XString): JCATree = {
    /*==hash-p.hash*/
    val pNew = new JCATree()
    pNew.hash = hash
    pNew.type0 = type0
    pNew.name = name
    if (p != null) {
      if (dir < 0) {
        p.l = pNew
      } else if (dir > 0) {
        p.r = pNew
      } else {
        p.next = pNew
      }
    }
    pNew
  }

  private def findInTree(type0: Char, name: XString, mkNew: Boolean): JCATree = {
    if (type0 == 'g' && globSection != null) {
      return globSection
    }
    if (type0 == 'm' && noMSection && !mkNew) {
      return null
    }
    if (type0 == 'c' && noCSection && !mkNew) {
      return null
    }

    val hash = calcHash(name)
    if (jcaTree == null) {
      if (!mkNew) {
        return null
      }
      jcaTree = findInTreeMkn(null, 0, hash, type0, name)
      return jcaTree
    }
    var p = jcaTree
    infiniteLoop {
      if (hash < p.hash) {
        if (p.l == null) {
          if (mkNew) {
            p.l = findInTreeMkn(p, -1, hash, type0, name)
          }
          return p.l
        }
        p = p.l
      } else if (hash > p.hash) {
        if (p.r == null) {
          if (mkNew) {
            p.r = findInTreeMkn(p, 1, hash, type0, name)
          }
          return p.r
        }
        p = p.r
      } else if (type0 == p.type0 && name.equals(p.name)) {
        return p
      } else if (p.next == null) {
        if (mkNew) {
          p.next = findInTreeMkn(p, 0, hash, type0, name)
        }
        return p.next
      } else {
        p = p.next
      }
    }
  }

  private def _jca_fix_signatures(p: JCATree): Unit = {
    if (p == null) {
      return
    }










    /* This part duplicates @m nodes that contain no method signature,
       adding it to the new nodes.

       How that works: _check_method_name returns the method name with
       added signature (if there was none). findInTree method has a flag
       that, when set to TRUE, specifies it to create a new node if it doesn't
       exist. So, if p.name has the signature already, then nodeWithSig = p,
       and nothing happens. Otherwise, nodeWithSig is the new node with the
       signature, so we just copy eqs and opts from p.
    */
    if (p.type0 == 'm' && p.name != null) {
      val nodeWithSig = findInTree(p.type0, _check_method_name(p.name), mkNew = true)

      nodeWithSig.eqs = p.eqs
      nodeWithSig.opts = p.opts
    }

    _jca_fix_signatures(p.l)
    _jca_fix_signatures(p.r)
    _jca_fix_signatures(p.next)
  }

  def findMethodJCA(method: pcO.Method): JCATree = {
    if (jcaTree == null) {
      return null
    }

    findInTree('m', method.getReadableName(need_class_name = true), mkNew = false)
  }

  def findGlobalJCA(): JCATree = findInTree('g', js.jstrEmpty, mkNew = false)

  /** Parses the specified JCA file (if there is no parsed file already). */
  def setJcaFile(fnm: String): Unit = {
    if (jcaTree != null) {
      return
    }

    log("\\n*JCA: ----- jca file = %s -----\\n", fnm)

    val parser = new JCAParser()
    parser.fnm = js.newJString(fnm)
    parser.sec = null

    val fd = xfs.sys.lookup(parser.fnm)
    val file = fd.openTextFile()
    loop {
      parser.str = file.readLine()
      if (parser.str == null) { // end of input
        break()
      }
      parser.pos = 0
      parser.str = parser.str.trim()
      //    WHILE (parser.curChar() = ' ') DO parser.pos := parser.pos+1; END;
      if (parser.hasMoreInLine) {
        if (parser.curChar() == '@') {
          parser.parseSection()
        } else if (parser.curChar() == '-' || parser.curChar() == '+') {
          parser.parseOption()
        } else if (parser.curChar() == '!') {
          parser.parseEquation()
        } else if (parser.curChar() != '#' && parser.curChar() != '%') {
          parser.logError("line ignored")
        }
      }
    }
    file.close()
    externalStrl = getEqValues(findGlobalJCA(), "INTRINSIC")
    jcaNOTINLINEG = getEqValues(findGlobalJCA(), "NOTINLINE")
    jcaNOTINLINE_REGEX_G = convertWildcardToRegex(getEqValues(findGlobalJCA(), "NOTINLINE_WILDCARD"))
    jcaALWAYSINLINEG = getEqValues(findGlobalJCA(), "ALWAYS_INLINE")
    jcaALWAYSINLINE_REGEX_G = convertWildcardToRegex(getEqValues(findGlobalJCA(), "ALWAYS_INLINE_WILDCARD"))
    jcaNOLOCALGCPOINTSG = getEqValues(findGlobalJCA(), "NO_LOCAL_GC_POINTS")
    jcaINLINEWITHCONTEXTPOINTTESTG = getEqValues(findGlobalJCA(), "INLINE_WITH_CONTEXT_POINT_TEST")
    jcaUNROLLLOOPSG = getEqValues(findGlobalJCA(), "UNROLL_LOOPS")
    jcaTURBOCLINITEDG = getEqValues(findGlobalJCA(), "TURBO_CLINITED")
    jcaCODEADDRTARGETG = getEqValues(findGlobalJCA(), "CODE_ADDR_TARGET")

    jreOverrideMap = createJreOverrideMap(getEqValues(findGlobalJCA(), JRE_OVERRIDE))
  }

  private def parseFieldDesc(str: XString): FieldDesc = {
    val nameEnd = str.indexOf(':')
    val sigEnd = str.indexOf(':', nameEnd + 1)
    if (!(0 < nameEnd && nameEnd < sigEnd && sigEnd < str.length - 1)) {
      error(js.newJString("JCA INJECT_FIELD: expected <name> \':\' <sig> \':\' <modifiers> "))
    }

    val fd = new FieldDesc()
    fd.name = str.substring(0, nameEnd)
    fd.sig = str.substring(nameEnd + 1, sigEnd)
    fd.accf = js.parseInt(str.substring(sigEnd + 1)).toSet32
    assert(fd.accf == (fd.accf & rts.JMDF_FIELD_MASK))

    if (O2Env.env.enabled(HideInjectedFields)) {
      fd.accf += rts.mdf_injected.toUByte
    }

    fd
  }

  def markNonNullFields(clazz: pcO.Class): Unit = {
    val clazzSection = findInTree('c', clazz.getReadableName, mkNew = false)
    val fieldNames = getEqValues(clazzSection, "NON_NULL_FIELD")
    if (fieldNames != null) {
      for (fieldName <- fieldNames) {
        val f = clazz.findLocalField(fieldName, null)
        assert(f != null, s"JCA NON_NULL_FIELD: $fieldName not found")
        f.sig match {
          case t: SignatureType.JBCReference => f.sig = NonNullableWrapper(t)
          case t => shouldNotReachHere(s"JCA NON_NULL_FIELD: $fieldName has invalid type $t")
        }
      }
    }
  }

  def findJreOverride(name: XString): Option[XString] = {
    jreOverrideMap.get(name)
  }

  def injectFields(clazz: pcO.Class): Unit = {
    val clazzSection = findInTree('c', clazz.getReadableName, mkNew = false)
    val strl = getEqValues(clazzSection, "INJECT_FIELD")
    if (strl != null) {
      // Check numeration of declared fields
      assert(clazz.declaredFields.zipWithIndex.forall((f, idx) => f.getNumberInClassFile == idx))
      val parsedFieldsCount = clazz.declaredFieldsCount

      // Inject new fields and numerate them
      for ((s, idx) <- strl.zipWithIndex) {
        val fd = parseFieldDesc(s)
        val f = clazz.newField(fd.name, fd.sig, fd.accf, addSignatureImport = true)
        f.setNumberInClassFile(parsedFieldsCount + idx)
      }
    }
  }

  /** Returns true iff `method` should be always inlined to any context (if it is possible) */
  def isJCAInline(method: pcO.Method): Boolean = {
    val name = method.getReadableName(need_class_name = true)
    isInStrlist(jcaALWAYSINLINEG, name) || isInRegexStrlist(jcaALWAYSINLINE_REGEX_G, name)
  }

  /** Returns true iff `method` should be never inlined to any context */
  def isJCANoInline(method: pcO.Method): Boolean = {
    val name = method.getReadableName(need_class_name = true)
    isInStrlist(jcaNOTINLINEG, name) || isInRegexStrlist(jcaNOTINLINE_REGEX_G, name)
  }

  def isJCANoLocalGCPoints(method: pcO.Method): Boolean =
    isInStrlist(jcaNOLOCALGCPOINTSG, method.getReadableName(need_class_name = true))

  /** Returns true if `method` should be devirtualized using point test and inlined in context of subclass. */
  def isJCAInlineWithContextPointTest(method: pcO.Method): Boolean =
    isInStrlist(jcaINLINEWITHCONTEXTPOINTTESTG, method.getReadableName(need_class_name = true))

  def isJCAUnrollLoops(method: pcO.Method): Boolean =
    isInStrlist(jcaUNROLLLOOPSG, method.getReadableName(need_class_name = true))

  def isJCACodeAddrTarget(method: pcO.Method): Boolean =
    isInStrlist(jcaCODEADDRTARGETG, method.getReadableName(need_class_name = true))

  private def createJreOverrideMap(list: List[XString]): mutable.Map[XString, XString] = {
    val overrideMap = mutable.Map.empty[XString, XString]
    for (s <- list) {
      val i = s.indexOf(':') ensuring (_ != -1)
      val cname = s.substring(0, i)
      val r = s.substring(i + 1) ensuring (_.indexOf(':') == -1)
      overrideMap(cname) = r
    }
    overrideMap
  }

  /*
    Gets hot switch cases for a switch with given number of cases
    in given method.
    Returns an empty list if there are none (e.g. no HOT_SWITCH_CASES equation).

    Experimental use only.
    Equation format: <min cases inclusive>..<max cases inclusive>:<case1>,<case2>,...,<caseN>
    (note that there are no spaces around commas)
  */
  def getJCAHotSwitchCases(method: pcO.Method, cases: Int): ArrayBuffer[Int] = {
    var currentCaseStr: XString = null

    var hotSwitchCasesEq = getEqValues(findMethodJCA(method), "HOT_SWITCH_CASES")
    val hotSwitchCasesVals = new ArrayBuffer[Int]

    for (str <- hotSwitchCasesEq) {
      val rangeDividerPos = str.indexOf(js.newJString(".."))
      val rangeEndPos = str.indexOf(':')

      assert(rangeDividerPos < rangeEndPos)

      val rangeStart = js.parseInt(str.substring(0, rangeDividerPos))
      val rangeEnd = js.parseInt(str.substring(rangeDividerPos + 2, rangeEndPos))

      if (rangeStart <= cases && cases <= rangeEnd) {
        var currentCasePos = rangeEndPos

        while (currentCasePos != -1) {
          val nextCasePos = str.indexOf(',', currentCasePos + 1)

          if (nextCasePos == -1) {
            currentCaseStr = str.substring(currentCasePos + 1)
          } else {
            currentCaseStr = str.substring(currentCasePos + 1, nextCasePos)
          }

          hotSwitchCasesVals += js.parseInt(currentCaseStr)

          currentCasePos = nextCasePos
        }
      }
    }

    hotSwitchCasesVals
  }

  def isTurboClinited(c: pcO.Class): Boolean =
    isInStrlist(jcaTURBOCLINITEDG, c.getReadableName)

  private def searchInJca(str: XString, lst: List[XString]): Option[Int] = {
    for (s <- lst) {
      val ch = s.charAtAsChar(0) // TODO: check str length
      if (ch != '-' && (ch < '0' || ch > '9') || s.charAt(1) != ',') {
        env.errors.fault(ErrMsg450, "JCA KNOWN_SAFE : expected <digit or \'-\'> \',\' <method name>")
      }
      if (s.substring(2).equals(str)) {
        if (ch == '-') {
          return Some(-1)
        } else {
          return Some((O2JSupport.convCharToInt(ch).toShort - 48.toShort).toShort.toInt)
        }
      }
    }

    Option.empty
  }

  def findKnownSafeInfo(m: pcO.Method): Int = {
    if (!iniKnownsafe) {
      iniKnownsafe = true
      jcaKnownsafe = getEqValues(findGlobalJCA(), "KNOWN_SAFE")
    }

    searchInJca(m.getReadableName(need_class_name = true), jcaKnownsafe) match {
      case Some(rp) =>
        log("** ks on %S(%d)\\n", m.getReadableName(need_class_name = true), rp)
        rp
      case None => pcO.JCA_NO_KNOWN_SAFE_INFO
    }
  }
}
