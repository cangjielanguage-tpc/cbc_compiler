/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiFilesModule as xfs, xmZipModule as xmZip}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule
import com.huawei.excelsior.o2j.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

object ManifestModule {

  /**
   * The Attributes.Name class represents an attribute name stored in
   * this Map. Valid attribute names are case-insensitive, are restricted
   * to the ASCII characters in the set [0-9a-zA-Z_-], and cannot exceed
   * 70 characters in length. Attribute values can contain any characters
   * and will be UTF8-encoded when written to the output stream.  See the
   * <a href="../../../../technotes/guides/jar/jar.html">JAR File Specification</a>
   * for more information about valid attribute names and values.
   */
  class Name extends Object {

    /*RO*/ var name: XString = _
    private[ManifestModule] var upper: XString = _
    private[ManifestModule] var hash: Int = _

    /**
     * Computes the hash value for this attribute name.
     */
    override def hashCode: Int = {
      if (this.hash == -1) {
        this.hash = this.upper.hashCode
      }
      this.hash
    }

    /**
      * Compares this attribute name to another for equality.
      * o -- the object to compare
      * Returns TRUE if this attribute name is equal to the
      * specified attribute object
     */
    override def equals(oPar: Any): Boolean = {
      val o = oPar.asInstanceOf[AnyRef]

      if (o.isInstanceOf[Name]) {
        return o.asInstanceOf[Name].upper.equals(this.upper)
      }
      false
    }

  }


  class Attributes extends Object {

    private[ManifestModule] var entries: Hashtable = _

    def filter(f: ManifestFilter): Unit = {
      val it = this.entries.keys
      while (it.hasNext) {
        val name = it.next().asInstanceOf[Name]
        if (f.filterAttribute(name, this.entries.get(name).asInstanceOf[XString])) {
          it.remove()
        }
      }
    }

    def writeMain(os: xfs.TextFile): Unit = {
      var vername = MANIFEST_VERSION
      var version = this.getValue(vername)
      if (version == null) {
        vername = SIGNATURE_VERSION
        version = this.getValue(vername)
      }


      if (version != null) {
        os.print("%S: %S\\r\\n", vername.name, version)
      }

      val it = this.entries.keys
      while (it.hasNext) {
        val name = it.next().asInstanceOf[Name]
        if (version == null || !name.equals(vername)) {
          val buffer = new js.StringBuffer()
          buffer.appendString(name.name)
          buffer.append(": ")
          buffer.appendString(this.entries.get(name).asInstanceOf[XString])
          buffer.appendString(newLine)
          make72Safe(buffer)
          os.print("%S", buffer.toJString)
        }
      }
      os.print("\\r\\n")
    }

    def write(os: xfs.TextFile): Unit = {

      val it = this.entries.keys
      while (it.hasNext) {
        val name = it.next().asInstanceOf[Name]
        val buffer = new js.StringBuffer()
        buffer.appendString(name.name)
        buffer.append(": ")
        buffer.appendString(this.entries.get(name).asInstanceOf[XString])
        buffer.appendString(newLine)
        make72Safe(buffer)
        os.print("%S", buffer.toJString)
      }
      os.print("\\r\\n")
    }

    def put(name: XString, value: XString): Unit = {
      if (name != null) {
        assert(value != null)
        this.entries.put(newNameByJString(js.intern(name)), value)
      }
    }

    def getValue(name: Name): XString = this.entries.get(name).asInstanceOf[XString]

  }


  class Manifest extends Object {

    private[ManifestModule] var mainattr: Attributes = _
    private[ManifestModule] var entries: Hashtable = _

    def filter(f: ManifestFilter): Unit = {
      this.mainattr.filter(f)
      val it = this.entries.keys
      while (it.hasNext) {
        val name = it.next().asInstanceOf[Name]
        val attrs = this.entries.get(name).asInstanceOf[Attributes]
        attrs.filter(f)
      }
    }

    def write(out: xfs.TextFile): Unit = {
      this.mainattr.writeMain(out)
      val it = this.entries.keys
      while (it.hasNext) {
        val name = it.next().asInstanceOf[Name]
        val attrs = this.entries.get(name).asInstanceOf[Attributes]
        if (attrs.entries.size > 0) {
          val buffer = new js.StringBuffer()
          buffer.append("Name: ")
          buffer.appendString(name.name)
          buffer.appendString(newLine)
          make72Safe(buffer)
          out.print("%S", buffer.toJString)

          attrs.write(out)
        }
      }
    }

    def put(name: XString, attr: Attributes): Unit = {
      this.entries.put(newNameByJString(name), attr)
    }

    def getAttributes(name: XString): Attributes = this.entries.get(newNameByJString(name)).asInstanceOf[Attributes]

    def getMainAttributes: Attributes = this.mainattr

  }


  class ManifestFilter {

    def filterAttribute(name: Name, value: XString): Boolean = {
      throw new AssertionError
    }

  }

  private val MANIFEST_VERSION = newName("Manifest-Version")
  private val SIGNATURE_VERSION = newName("Signature-Version")
  val MAIN_CLASS = newName("Main-Class")
  val SPLASHSCREEN_IMAGE = newName("SplashScreen-Image")

  val IMPLEMENTATION_TITLE = newName("Implementation-Title")
  val IMPLEMENTATION_VERSION = newName("Implementation-Version")
  val IMPLEMENTATION_VENDOR = newName("Implementation-Vendor")
  val SPECIFICATION_TITLE = newName("Specification-Title")
  val SPECIFICATION_VERSION = newName("Specification-Version")
  val SPECIFICATION_VENDOR = newName("Specification-Vendor")
  val SEALED = newName("Sealed")
  val SHA1_DIGEST = newName("SHA1-Digest")
  val SHA_DIGEST = newName("SHA-Digest")
  val MD5_DIGEST = newName("MD5-Digest")
  val JAVAFX_APP_CLASS = newName("JavaFX-Application-Class")
  val JAVAFX_PRELOADER_CLASS = newName("JavaFX-Preloader-Class")
  private val META_INF_MANIFEST_MF = js.internJString("META-INF/MANIFEST.MF")
  private val newLine = js.newJString("\r\n")
  private val newLineAndSpace = js.newJString("\r\n ")
  private var manifests: Hashtable = _
  private var lastjar: XString = _
  private var lastman: Manifest = _

  /**
   * Constructs a new attribute name using the given string name.
   *
   * name -- the attribute string name
   */
  private def newNameByJString(name: XString): Name = {
    val this0 = new Name()
    this0.hash = -1
    this0.name = name
    this0.upper = js.intern(this0.name.toUpperCase)
    this0
  }

  private def isAlpha(c: Char): Boolean = O2JSupport.convCharToInt(c).toShort >= 97.toShort && O2JSupport.convCharToInt(c).toShort <= 122.toShort || O2JSupport.convCharToInt(c).toShort >= 65.toShort && O2JSupport.convCharToInt(c).toShort <= 90.toShort

  private def isDigit(c: Char): Boolean = O2JSupport.convCharToInt(c).toShort >= 48.toShort && O2JSupport.convCharToInt(c).toShort <= 57.toShort

  private def isValidChar(c: Char): Boolean = isAlpha(c) || isDigit(c) || c == '_' || c == '-'

  private def isValid(name: String): Boolean = {
    val len = name.length
    if (len > 70 || len == 0) {
      return false
    }

    for (i <- 0 until len) {
      if (!isValidChar(name(i))) {
        return false
      }
    }
    true
  }

  private def newName(name: String): Name = {
    assert(isValid(name))
    newNameByJString(js.internJString(name))
  }

  private def make72Safe(/*VAR*/ line: js.StringBuffer): Unit = {
    var length = line.length
    if (length > 72) {
      var index = 70
      while (index < length - 2) {
        line.insert(index, newLineAndSpace)
        index += 72
        length += 3
      }
    }
  }

  private def colonIndex(s: XString): Int = {
    val pos = s.indexOf(':')

    if (pos == -1 || s.charAt(pos + 1) != ' ') {
      return -1
    }

    pos
  }

  private def readAttr(f: xfs.TextFile, initstr: XString): Attributes = {
    var s: XString = null

    val a = new Attributes()
    a.entries = new Hashtable()

    var name: XString = null
    var value: XString = null
    if (initstr == null) {
      s = f.readLine()
    } else {
      s = initstr
    }

    infiniteLoop {
      if (s == null || s.length == 0) {
        a.put(name, value)
        return a
      }

      if (s.charAt(0) == ' ') {
        if (value == null) {
          return null
        } else {
          value = value.concat(s.substring(1))
        }
      } else {
        a.put(name, value)
        val pos = colonIndex(s)
        if (pos < 0) {
          return null
        }
        name = s.substring(0, pos)
        value = s.substring(pos + 2)
      }
      s = f.readLine()
    }
  }

  def readManifest(f: xfs.TextFile): Manifest = {
    val m = new Manifest()
    m.entries = new Hashtable()
    var attr = readAttr(f, null)
    if (attr == null) {
      return null
    }
    m.mainattr = attr
    var name: XString = null
    var value: XString = null
    infiniteLoop {
      val s = f.readLine()
      if (s == null) {
        return m
      }
      if (s.length != 0) {
        if (s.charAt(0) == ' ') {
          if (name == null) {
            return null
          } else {
            value = value.concat(s.substring(1))
          }
        } else if (name == null) {
          val pos = colonIndex(s)
          if (pos < 0) {
            return null
          }
          name = s.substring(0, pos).toUpperCase
          value = s.substring(pos + 2)
          if (!name.equals2("NAME")) {
            return null
          }
        } else {
          attr = readAttr(f, s)
          if (attr == null) {
            return m
          }
          m.put(value, attr)
          name = null
        }
      }
    }
  }

  def getManifest(jar: XString): Manifest = {
    if (lastjar != null && lastjar.equals(jar)) {
      return lastman
    }
    if (manifests == null) {
      manifests = new Hashtable()
    } else {
      val m = manifests.get(jar).asInstanceOf[Manifest]
      if (m != null) {
        return m
      }
    }
    val fd = xmZip.createFileDescriptorNoCase(jar, META_INF_MANIFEST_MF)
    if (!fd.exists) {
      return null
    }
    val file = fd.openTextFile()
    val m = try readManifest(file) finally file.close()
    lastjar = jar
    lastman = m
    if (m != null) {
      assert(manifests.put(jar, m) == null)
    }
    m
  }

  def getManifestFD(jar: xfs.FileDescriptor): xfs.FileDescriptor = xmZip.createFileDescriptorNoCase(jar.getName, META_INF_MANIFEST_MF)
}
