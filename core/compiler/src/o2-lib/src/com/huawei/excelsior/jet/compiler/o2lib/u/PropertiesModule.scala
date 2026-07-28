/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env, xiFilesModule as xfs}

object PropertiesModule {

  /**
     Persistent properties class. This class is basically a hashtable 
     that can be saved/loaded from a stream. If a property is not found,
     a property list containing defaults is searched. This allows
     arbitrary nesting.
  */
  class Properties extends Hashtable {

    private[PropertiesModule] var def0: Properties = _

    /**
       Save properties to an OutputStream. Use the header as
       a comment at the top of the file.
    */
    def save(out: xfs.TextFile, header: XString): Unit = {
      if (header != null) {
        out.print("#%S\\n", header)
      }
      val iter = this.keys
      while (iter.hasNext) {
        val key = iter.next()
        out.print("%S=%S\\n", key, this.get(key))
      }
    }

    /**
       Loads properties from an InputStream.
       Parameters:
         in - the input stream
    */
    def load(in: xfs.TextFile): Unit = {
      var line = in.readLine()
      while (line != null) {
        line = line.trim()
        if (line.nonEmpty && line.charAt(0) != '#' && line.charAt(0) != '!') { // skip empty lines
          // skip comments 
          while (line.charAt(line.length - 1) == '\\') {
            line = line.substring(0, line.length - 1)
            // concat next line 
            val nextLine = in.readLine()
            if (nextLine != null) {
              line = line.concat(nextLine.trim())
            }
          }
          val eqPos = line.indexOf('=')
          if (eqPos > 0) {
            val key = line.substring(0, eqPos).trim()
            val value = line.substring(eqPos + 1).trim()
            this.put(key, value)
          } else {
            this.put(line, js.jstrEmpty)
          }
        }
        line = in.readLine()
      }
    }

    def getProperty(key: String): XString = this.getProperty0(js.newJString(key))

    /**
       Gets a property with the specified key. If the key is not 
       found in this property list, tries the defaults. This method 
       returns default if the property is not found.
       Parameters:
         key - the hashtable key
    */
    def getProperty0(key: XString, defaultVal: XString = null): XString = {
      var value = this.get(key)
      if (value == null && this.def0 != null) {
        value = this.def0.getProperty0(key)
      }
      if (value != null) {
        value.asInstanceOf[XString]
      } else {
        defaultVal
      }
    }

    override def put(key: Object, value: Object): Object = {
      assert(key.isInstanceOf[XString] && value.isInstanceOf[XString])
      super.put(key, value)
    }

  }

  private var jcProps: Properties = _

  /**
     Creates an empty property list with specified defaults.
     Parameters:
       defaults - the defaults
  */
  def newProperties(defaults: Properties): Properties = {
    val p = new Properties()
    p.def0 = defaults
    p
  }

  def initJCProperties(): Unit = {
    jcProps = newProperties(null)
    val fn = xfs.sys.sysLookup("properties")
    val f = xfs.text.openToRead(fn)
    if (f == null) {
      env.info.forcePrint("Configuration file \"jc.properties\" is not opened: %S\\n", xfs.text.errmsg)
      throw new Error()
    }
    jcProps.load(f)
    f.close()
  }

  def getJCProperty(key: String): XString = {
    assert(jcProps != null)
    jcProps.getProperty(key)
  }

  def getJCPropertyS(key: XString): XString = {
    assert(jcProps != null)
    jcProps.getProperty0(key)
  }
}
