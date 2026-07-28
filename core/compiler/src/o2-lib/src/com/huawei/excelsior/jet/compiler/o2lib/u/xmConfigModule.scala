/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.o2lib.opt.VZCModule as VZC
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as CharClass
import com.huawei.excelsior.jet.compiler.o2lib.u.xiEnvModule.{CompilerOptionSet, OptionsHolder, ok}
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, SynchronizedHashtableModule as SynchronizedHashtable, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

import scala.annotation.tailrec
import scala.collection.mutable

/** Standard implementation of Config manager */
object xmConfigModule {

  // Type Value
  private class Values {

    private[xmConfigModule] var value: env.Value = _
    private[xmConfigModule] var level: Int = _
    private[xmConfigModule] var next: Values = _

  }

  // Type AbstractOption
  /*
    Option wrapper that holds its current level
    and values of previous option levels to be able to roll back.
    See also: Save/Restore
  */

  private class Element[T <: env.AbstractOption] extends Object {

    private[xmConfigModule] var option: T = _
    private[xmConfigModule] var values: Values = _
    private[xmConfigModule] var level: Int = _
    private[xmConfigModule] var res: Int = _ /** cached result of initialization by default */

  }

  private class Synonym(name: XString, val ref: Element[env.Option]) extends env.Option(name) {
    require(ref != null)

    override def getValue: env.Value = this.ref.option.getValue

    override def setValue(value: env.Value): Unit = {
      throw new AssertionError // values for synonyms should not bet set directly (only via reference setValue).
    }

  }


  private class StackItem {

    private[xmConfigModule] var tags: env.CompilerOptionSet = _
    private[xmConfigModule] var next: StackItem = _

  }


  class Config extends OptionsHolder {

    var tags: CompilerOptionSet = CompilerOptionSet.empty
    var res: Int = ok /** result of operation */

    private[xmConfigModule] var level: Int = 0 /* state level */
    private[xmConfigModule] var opts = mutable.HashMap.empty[XString, Element[env.Option]].withDefaultValue(null)
    private[xmConfigModule] var equs = mutable.HashMap.empty[XString, Element[env.Equation]].withDefaultValue(null)
    private[xmConfigModule] var normalizeCache: Hashtable = SynchronizedHashtable.newHashtable/*<JString, JString>*/
    private[xmConfigModule] var stack: StackItem = null

    def pop(): Unit = {
      env.context = null
    }

    def push(): Unit = {
      env.context = new Context()
    }

    def newRuntimeContext(): Context = new Context()

    // Returns the name of parsed option/equation
    def parse(s: XString): XString = {
      this.res = env.ok
      var name: XString = null
      var p = skipSpaces(s, 0)
      if (p < s.length) {
        val ch = s.charAtAsChar(p)
        p += 1
        if (ch == '+' || ch == '-' || ch == ':') {
          name = this.parseOption(s, p, ch)
          if (this.res == env.ok) {
            this.res = env.isOption
          }
        } else if (ch == '#') {
          name = this.parseEquation(s, p)
          if (this.res == env.ok) {
            this.res = env.isEquation
          }
        } else if (ch != '%') {
          this.res = env.wrongSyntax
        }
      }
      name
    }

    // Returns name of parsed option
    def parseOption(s: XString, pPar: Int, tagPar: Char): XString = {
      var p = pPar
      var tag = tagPar
      var value: Boolean = false
      val nameBuf = new js.StringBuffer()
      val start = p

      p = getName(s, p, nameBuf)
      if (nameBuf.length == 0) {
        this.res = env.wrongSyntax
        return null
      }

      nameBuf.toUpperCase()
      var name = nameBuf.toJString

      p = skipSpaces(s, p)
      if (p < s.length && tag == '-') {
        val ch = s.charAt(p)
        if (ch == '-') {
          tag = '-'
          p += 1
        } else if (ch == '+') {
          tag = '+'
          p += 1
        } else if (ch == ':') {
          tag = ':'
          p += 1
          if (p < s.length && s.charAt(p) == '=') { /* dcl equation */
            name = this.parseEquation(s, start)
            if (this.res == env.ok) {
              this.res = env.isEquation
            }
            return name
          }
        } else if (ch == '=' || ch == '!') {
          name = this.parseEquation(s, start)
          if (this.res == env.ok) {
            this.res = env.isEquation
          }
          return name
        }
      }

      var syn = false

      if (tag == ':') {
        value = false
        if (p < s.length) {
          val ch = s.charAt(p)
          if (ch == '+' || ch == '-') {
            value = ch == '+'
            p += 1
            p = skipSpaces(s, p)
          } else if (ch == ':') {   /* Ned: was '=' */
            syn = true
            p += 1
            p = getName(s, p, nameBuf)
            if (nameBuf.length == 0) {
              this.res = env.wrongSyntax
              return name
            }
          }
        }
      } else {
        value = tag == '+'
      }

      if (p < s.length && s.charAt(p) != '%') {
        this.res = env.wrongSyntax
      } else if (syn) {
        this.synonymJS(nameBuf.toJString, name)
      } else if (tag == ':') {
        this.newOptionJS(name, value, Unchecked)
      } else {
        this.setOptionJS(name, value)
      }

      name
    }

    // Returns name of parsed equation
    def parseEquation(s: XString, pPar: Int): XString = {
      var p = pPar
      val nameBuf = new js.StringBuffer()
      p = getName(s, p, nameBuf)
      if (nameBuf.length == 0) {
        this.res = env.wrongSyntax
        return null
      }

      nameBuf.toUpperCase()
      val name = nameBuf.toJString

      p = skipSpaces(s, p)
      val len = s.length
      if (p < len) {
        val ch = s.charAt(p)
        if (ch == ':') {
          p += 1
          if (p < len && s.charAt(p) == '=') {
            p += 1
            p = skipSpaces(s, p)
            // Treat user defined equations as Unchecked.
            // How can they affect compilation? If they can, they must be defined in the compiler
            this.newEquationJS(name, Unchecked)
            if (this.res != env.ok) {
              return name
            }
          }
        } else if (ch == '=') {
          p += 1
          p = skipSpaces(s, p)
        } else if (ch == '!') {
          p += 1
          p = skipSpaces(s, p)
          this.newEquationJS(name, Unchecked)
          if (this.res != env.ok) {
            return name
          }
        }
      }

      var i = len - 1
      while (i >= p && CharClass.isWhiteSpace(s.charAt(i))) {
        i -= 1
      }
      val value = s.substring(p, i + 1)
      this.setEquationJS(name, value)

      name
    }

    def isValidTag(str: XString): Boolean = {
      val p = skipSpaces(str, 0)
      if (p >= str.length) {
        return true
      }
      val ch = str.charAt(p)
      ch == '+' || ch == '-' || ch == ':' || ch == '#'
    }

    def removeContext(context0: Context): Unit = {
      assert(xmConfigModule.stack != null && (xmConfigModule.stack.context eq context0))
      val cxt = env.context
      env.context = null
      xmConfigModule.stack = xmConfigModule.stack.next
      if (context0 != null && (xmConfigModule.stack == null || (xmConfigModule.stack.context ne context0))) {
        this.restore()
      }
      if (xmConfigModule.stack != null && xmConfigModule.stack.context != null && (xmConfigModule.stack.context ne context0)) {
        this.save()
        val context = xmConfigModule.stack.context
        setOptions(context)
        setEquations(context)
      }
      env.context = cxt
    }

    def setContext(context0: Context): Unit = {
      val cxt = env.context
      env.context = null
      if (xmConfigModule.stack != null && xmConfigModule.stack.context != null && (xmConfigModule.stack.context ne context0)) {
        this.restore()
      }
      if (context0 != null && (xmConfigModule.stack == null || (xmConfigModule.stack.context ne context0))) {
        this.save()
        val context = context0
        setOptions(context)
        setEquations(context)
      }
      val si = new ContextStack()
      si.context = context0
      si.next = xmConfigModule.stack
      xmConfigModule.stack = si
      env.context = cxt
    }

    def restore(): Unit = {
      assert(this.level > 0)
      this.removeElems(opts)
      this.removeElems(equs)
      this.tags = this.stack.tags
      this.stack = this.stack.next
      this.level -= 1
    }

    private def removeElems[T <: env.AbstractOption](elems: mutable.Map[XString, Element[T]]): Unit = {
      for (l <- elems.values.toList) {
        val opt = l.option
        if (l.level >= this.level) {
          opt.setValue(null)
          elems.remove(opt.name)

        } else {
          var i = l.values
          while (i != null && i.level >= this.level) {
            i = i.next
          }
          if (l.values ne i) {
            l.values = i
            if (i == null) {
              opt.setValue(null)
            } else {
              opt.setValue(i.value)
            }
          }
        }
      }
    }

    /*----------------------------------------------------------------*/
    def save(): Unit = {
      this.level += 1
      val p = new StackItem()
      p.tags = this.tags
      p.next = this.stack
      this.stack = p
    }

    def synonym(old: String, new0: String): Unit = {
      this.synonymJS(js.newJString(old), js.newJString(new0))
    }

    def synonymJS(old: XString, new0: XString): Unit = {
      val oldCanonicalName = this.normalizeNameJS(old)

      var o = findOpt(this, oldCanonicalName)
      if (o == null) {
        this.res = env.unknownOption
      }
      if (o != null) {
        val newCanonicalName = this.normalizeNameJS(new0)

        var n = findOpt(this, newCanonicalName)
        if (n != null) {
          this.res = env.definedOption
        } else {
          n = registerOpt(this, new Synonym(newCanonicalName, opts(oldCanonicalName)), this.level)
          if (n == null) {
            return
          }
          this.res = env.ok
        }
      }
    }

    override def options = (opts.valuesIterator ++ equs.valuesIterator) map (_.option)

    def option(name: String, initWithDefault: Boolean = true): Boolean = this.optionByCanonicalName(this.normalizeName(name), initWithDefault)

    def optionJS(name: XString, initWithDefault: Boolean = true): Boolean = this.optionByCanonicalName(this.normalizeNameJS(name), initWithDefault)

    def optionByCanonicalName(canonicalName: XString, initWithDefault: Boolean): Boolean = {
      val n = opts(canonicalName)

      if (n == null) {
        if (initWithDefault) {
          val defValue = VZC.getDefaultOption(canonicalName)
          val b = if (defValue == env.YES) {
            true
          } else {
            // Couldn't read default options from vzc before preinit,
            // so force set unknown options FALSE to eliminate multiple queries to vzc.
            // WARNING: There should not be options with Scala-side defined default values
            // on O2-side that are used before Scala-side preinit.
            false
          }
          // Treat unknown options as Unchecked.
          // How can they affect compilation? If they can, they must be defined in the compiler
          if (defValue == env.UNSPECIFIED) {
            this.registerOption(env.newOption(canonicalName, Unchecked), b)
            val n = opts(canonicalName)
            if (n != null) {
              n.res = env.unknownOption
            }
            this.res = env.unknownOption
          } else {
            this.registerOption(env.newOption(canonicalName, Checked), b)
            this.res = env.ok
          }
          b
        } else {
          this.res = env.unknownOption
          false
        }
      } else {
        this.res = n.res
        checkConsistency(n)
        n.option.getBooleanValue
      }
    }

    def newOption(name: String, value: Boolean, checked: SmartKind = Checked): Unit = {
      this.newOptionJS(js.newJString(name), value, checked)
    }

    def newOptionJS(name: XString, value: Boolean, checked: SmartKind = Checked): Unit = {
      this.registerOption(xiEnvModule.newOption(name, checked), value)
    }

    def registerOption(o: env.Option, value: Boolean): Unit = {
      val name = o.name
      val e = opts(name)
      if (e != null) {
        val res = if (e.res == env.ok) {
          env.definedOption
        } else {
          env.ok
        }
        this.setOptionJS(name, value)
        this.res = res
      } else {
        val n = registerOpt(this, o, this.level)
        if (n == null) {
          return
        }
        n.setNewBooleanValue(value)

        val e = opts(name)
        assert(e != null)
        e.res = env.ok
        val values = new Values()
        values.value = n.getValue
        values.next = null
        e.values = values
        e.values.level = this.level
        this.res = env.ok
      }
    }

    override def setOptionJS(name: XString, value: Boolean): Unit = {
      if (env.context != null) {
        assert(env.config eq this)
        env.context.setOptionJS(name, value)
        return
      }

      val e = findCreate(this, this.normalizeNameJS(name))
      if (e == null) {
        return
      }
      this.setOptionValue(e, value)
      e.res = env.ok
    }

    private def setOptionValue(n: Element[env.Option], value: Boolean): Unit = {
      if (n.option.getBooleanValue == value && n.values != null && n.values.level == this.level) {
        return
      }
      val o = n.option
      if (n.values == null || n.values.level != this.level) {
        o.setNewBooleanValue(value) // creates new value
        val v = new Values()
        v.value = o.getValue
        v.next = n.values
        v.level = this.level
        n.values = v
      } else {
        o.setBooleanValue(value) // update old value;
      }
    }

    def equationJS(name: XString): XString = this.equationByCanonicalName(this.normalizeNameJS(name))

    def equation(name: String): XString = this.equationByCanonicalName(this.normalizeName(name))

    def equationOrDefault(name: String, defaultVal: String): XString = {
      val res = equation(name)
      if (res != null) res else XString.ascii(defaultVal)
    }

    def equationByCanonicalName(canonicalName: XString): XString = {
      val n = equs(canonicalName)
      if (n == null) {
        val defValue = VZC.getDefaultEquation(canonicalName)
        // Treat unknown equations as Unchecked.
        // How can they affect compilation? If they can, they must be defined in the compiler
        val checked = if (defValue == null) Unchecked else Checked
        this.registerEquation(env.newEquation(canonicalName, checked))

        // Couldn't read default equations from vzc before preinit,
        // so force set unknown equations EMPTY to eliminate multiple queries to vzc.
        // WARNING: There should not be equations with Scala-side defined default values
        // on O2-side that are used before Scala-side preinit.
        this.setEquationJS(canonicalName, defValue)
        val n = equs(canonicalName)
        if (n != null) {
          n.res = env.unknownEquation
        }

        if (defValue == null) {
          this.res = env.unknownEquation
        } else {
          this.res = env.ok
        }
        defValue
      } else {
        this.res = n.res
        val e = n.option
        e.getStringValue
      }
    }

    override def setEquationJS(name: XString, value: XString): Unit = {
      var n: env.Equation = null

      if (env.context != null) {
        assert(env.config eq this)
        env.context.setEquationJS(name, value)
        return
      }

      val canonicalName = this.normalizeNameJS(name)
      var e = equs(canonicalName)
      if (e == null) {
        this.res = env.unknownEquation
        // Treat unknown equations as Unchecked.
        // How can they affect compilation? If they can, they must be defined in the compiler
        n = registerEqu(this, env.newEquation(canonicalName, Unchecked), this.level)
        if (n == null) {
          return
        }
        e = equs(canonicalName)
        assert(e != null)
      } else {
        n = e.option
        e.res = env.ok
        this.res = env.ok
      }

      n.setNewStringValue(value)

      val values = new Values()
      values.value = n.getValue
      values.level = this.level

      values.next = e.values
      e.values = values
    }

    def newEquation(name: String, checked: SmartKind = Checked): Unit = {
      this.newEquationJS(js.newJString(name), checked)
    }

    def newEquationJS(name: XString, checked: SmartKind = Checked): Unit = {
      this.registerEquation(xiEnvModule.newEquation(name, checked))
    }

    def registerEquation(e: env.Equation): Unit = {
      val elem = equs(e.name)
      if (elem != null) {
        if (elem.res == env.ok) {
          this.res = env.definedEquation
        } else {
          this.res = env.ok
        }
      } else {
        val n = registerEqu(this, e, this.level)
        if (n != null) {
          this.res = env.ok
        }
      }
    }

    def normalizeNameJS(name: XString): XString = this.normalizeNameImpl(js.intern(name))

    def normalizeName(name: String): XString = this.normalizeNameImpl(js.internJString(name))

    def normalizeNameImpl(name: XString): XString = {
      assert(name.nonEmpty)
      assert(name.charAt(0) != ' ')

      var norm = this.normalizeCache.get(name).asInstanceOf[XString]

      if (norm == null) {
        norm = name.toUpperCase
        this.normalizeCache.put(name, norm)
      }
      norm
    }

  }


  class Context private[xmConfigModule] () extends OptionsHolder {
    contextsCounter += 1

    private[xmConfigModule] var opts = mutable.HashMap.empty[XString, env.Option].withDefaultValue(null)
    private[xmConfigModule] var equs = mutable.HashMap.empty[XString, env.Equation].withDefaultValue(null)
    private[xmConfigModule] val hash: Int = contextsCounter

    override def hashCode: Int = {
      assert(this.hash != 0)
      this.hash
    }

    override def equals(oPar: Any): Boolean = {
      val o = oPar.asInstanceOf[AnyRef]

      o eq this
    }

    override def options: Iterator[xiEnvModule.AbstractOption] = opts.valuesIterator ++ equs.valuesIterator

    override def setOptionJS(name: XString, value: Boolean): Unit = {
      assert(env.config.isInstanceOf[Config])
      val cfg = env.config
      val canonicalName = cfg.normalizeNameJS(name)
      var opt = findOpt(cfg, canonicalName)
      if (opt == null) {
        env.config.res = env.unknownOption
      } else {
        // copy option from env.config to context
        opt = env.newOption(opt.name, opt.checked)
        opt.setNewBooleanValue(value)
        this.opts.put(opt.name, opt)
      }
    }

    //--------------------- Context Handling ---------------
    override def setEquationJS(name: XString, value: XString): Unit = {
      assert(env.config.isInstanceOf[Config])
      val cfg = env.config
      val canonicalName = cfg.normalizeNameJS(name)
      var n = findEqu(cfg, canonicalName)
      if (n == null) {
        env.config.res = env.unknownEquation
      } else {
        // copy equation from env.config to context
        n = env.newEquation(n.name, n.checked)
        n.setNewStringValue(value)
        this.equs.put(n.name, n)
      }
    }

  }



  private class ContextStack {

    private[xmConfigModule] var context: Context = _
    private[xmConfigModule] var next: ContextStack = _

  }

  /* Ned 24-Feb-94. */
  /* Sem 09-Sep-95. */
  /* Modifications
    16.06.96 Ned  Parse: change syntax for synonyms and declaring equations.
  */
  private var stack: ContextStack = _
  private var contextsCounter: Int = 0

  /*----------------------------------------------------------------*/
  def clearCaches(): Unit = {
    if (env.config != null) {
      val cfg = env.config
      if (isWorkMode) {
        env.info.print("\\nxmConfig.normalizeCache cleared [size=%d]\\n", cfg.normalizeCache.size)
      }
      cfg.normalizeCache = SynchronizedHashtable.newHashtable
    }
  }

  private def findOption[T <: env.AbstractOption](table: mutable.Map[XString, Element[T]], name: XString): env.AbstractOption = {
    // table.get(name).map(_.option).orNull
    val e = table(name)
    if (e != null) {
      e.option
    } else {
      null
    }
  }

  private def findEqu(c: Config, name: XString): env.Equation = findOption(c.equs, name).asInstanceOf[env.Equation]

  private def findOpt(c: Config, name: XString): env.Option = findOption(c.opts, name).asInstanceOf[env.Option]

  private def registerEqu(c: Config, n: env.Equation, lev: Int): env.Equation = {
    val e = c.opts(n.name)
    if (e != null && e.res == env.ok) {
      c.res = env.defineEquationWhenOptionDefined
      null

    } else {
      val e = new Element[env.Equation]()
      e.option = n
      e.level = lev
      e.values = null
      e.res = env.ok

      c.equs.put(n.name, e)

      n
    }
  }

  private def registerOpt(c: Config, opt: env.Option, lev: Int): env.Option = {
    val e = c.equs(opt.name)
    if (e != null && e.res == env.ok) {
      c.res = env.defineOptionWhenEquationDefined
      null
    } else {
      val e = new Element[env.Option]()
      e.level = lev
      e.values = null
      e.option = opt
      e.res = env.ok

      c.opts.put(opt.name, e)

      opt
    }
  }

  private def findCreate(c: Config, name: XString): Element[env.Option] = {
    var e = c.opts(name)

    if (e == null) {
      // Treat unknown options as Unchecked.
      // How can they affect compilation? If they can, they must be defined in the compiler
      if (registerOpt(c, env.newOption(name, Unchecked), c.level) == null) {
        return null
      }
      e = c.opts(name)
      assert(e != null)
      c.res = env.unknownOption
    } else {
      c.res = env.ok
    }

    e.option match {
      case synonym: Synonym => synonym.ref
      case _ => e
    }
  }

  @tailrec
  private def checkConsistency[T <: env.AbstractOption](n: Element[T]): Unit = {
    n.option match {
      case synonym: Synonym => checkConsistency(synonym.ref)
      case opt =>
        if (n.values != null) {
          assert(opt.getValue != null)
          assert(n.values.value eq opt.getValue)
        } else {
          assert(opt.getValue == null)
        }
    }
  }

  private def setOptions(c: Context): Unit = {
    for (l <- c.opts.values) {
      env.config.setOptionJS(l.name, l.getBooleanValue)
    }
  }

  private def setEquations(c: Context): Unit = {
    for (l <- c.equs.values) {
      env.config.setEquationJS(l.name, l.getStringValue)
    }
  }

  /*----------------------------------------------------------------*/
  private def skipSpaces(s: XString, posPar: Int): Int = {
    var pos = posPar

    while (pos < s.length && CharClass.isWhiteSpace(s.charAt(pos))) {
      pos += 1
    }
    pos
  }

  private def getName(s: XString, pPar: Int, /*VAR*/ name: js.StringBuffer): Int = {
    var p = pPar

    name.trunc(0)
    p = skipSpaces(s, p)
    val len = s.length
    loop {
      if (p >= len) {
        break()
      }
      val ch = s.charAt(p)
      if (CharClass.isLetter(ch) || CharClass.isNumeric(ch) || ch == '_') {
        name.appendChar(ch)
      } else {
        break()
      }
      p += 1
    }
    p
  }
}
