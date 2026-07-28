/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.JavaClassParserModule as jcp
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.{JBCPreprocessor, JavaClassParserModule as jcp}
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as CharClass
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.AbstractProject
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.CPEntryModes.{CPEntryMode, cpe_app, cpe_appclassloader, cpe_webapp}
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule.PDBKind
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xArchivePDBModule as xArchivePDB, xDirectoryPDBModule as xDirectoryPDB, xLookupModule as xLookup, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.o2lib.u.{DirsModule as Dirs, JStringsModule as JStrings, ManifestModule as Manifest, PropertiesModule as Properties, StdLibCompilerModule as StdLibCompiler, xcModesModule as xcModes, xiEnvModule as env, xiFilesModule as xfs, xmErrorsModule as xmErrors, xmZipModule as xmZip}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.JZip.ZipFileModule as ZipFile
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, PortableProgExecModule as ProgExec}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{DoNotCallImportResolver, OldSpringBoot}
import com.huawei.excelsior.jet.compiler.options.StrOption.{DynLibs, STDLib, TomcatClasspath, UseLibrary}
import com.huawei.excelsior.jet.compiler.smart.ImportResolutionType
import com.huawei.excelsior.jet.compiler.smart.ImportResolutionType.{ABSENT, EXTERNAL, NORMAL}
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.{Set32, UByte, UShort}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Make facility */
object xcMakeModule { /* Ned 24-Feb-94. */
                /* Sem 10-Oct-95. */

  /** Sketch of usage:

    VAR p: Project; n: Node;

    p.SetEquations;
    LOOP p.Append(name) END;
    p.Regulate(n);
    WHILE n#NIL DO
      IF n.mod.IsCompilable(all) THEN
        Compile; n.mod.SetResult()
      END;
    END;
    p.SetFileNames;
  */
  /** modes */
  type Mode = UByte
  val md_obj: Mode = UByte(0)        /* object file */
  val md_sym: Mode = UByte(1)        /* sym-file */
  val md_jbc: Mode = UByte(2)        /* Java class file */
  val md_lib: Mode = UByte(3)        /* library file */
  val md_jar: Mode = UByte(4)        /* jar file */
  val md_war: Mode = UByte(5)        /* war file */
  val md_dll: Mode = UByte(6)        /* dll file */
  val md_usg: Mode = UByte(7)
  val md_fus: Mode = UByte(8)
  val md_ico: Mode = UByte(9)
  val md_zip: Mode = UByte(10)        /* zip file */
  val md_res: Mode = UByte(11)        /* resource file */
  val md_jitcache: Mode = UByte(13)
  val md_efs: Mode = UByte(14)
  val md_unknown: Mode = UByte(17)
  val md_dir: Mode = UByte(18)        /* directory */
  val md_bundle: Mode = UByte(19)

  val SetOfModes = Set32
  type SetOfModes = Set32

  class TM {

    var undef: Boolean = _
    var exist: Boolean = _
    var time: Int = _

    def _copyTo(that: TM): TM = {
      that.undef = this.undef
      that.exist = this.exist
      that.time = this.time
      that
    }

    /*---------------------------------------------------------------*/
    def getTime(mod: File): Unit = {
      assert(mod.fd != null)
      if (mod.time.undef) {
        this.undef = false
        this.exist = mod.fd.exists
        this.time = mod.fd.modifyTime()
        if (!(mod.tags contains tag_out)) {
          this._copyTo(mod.time)
        }
      } else {
        mod.time._copyTo(this)
      }
    }
  }


  class Node {

    var mod: File = _
    var next: Node = _

  }


  class CUnit extends Object {

    var name: pcNames.NAME = _
    /*RO*/ var s: CUnit = _ /* Synonyms */
    /*RO*/ var u: CUnit = _ /* First synonym */

    def append(tPar: Hashtable): Hashtable = {
      var t = tPar

      if (t == null) {
        t = new Hashtable()
      }
      val e = t.get(this.name)
      if (e == null) {
        this.u = this
        assert(t.put(this.name, this) == null)
      } else {
        var l = e.asInstanceOf[CUnit]
        while (l.s != null) {
          l = l.s
        }
        l.s = this
        this.u = l.u
      }
      t
    }

  }


  class File extends CUnit {

    var mode: Mode = _
    var fd: xfs.FileDescriptor = _ /* file descriptor   */
    /*RO*/ var out: Node = _      /** out#NIL not only for XDS compiled modules! */
    var next: File = _      /** in the module list */
    var from: File = _      /** depend on this file */
    var tags: Set32 = _
    var clazz: pcO.Class = _
    var context: env.Context = _
    var host: File = _
    /*RO*/ var cpeMode: CPEntryMode = _
    private[xcMakeModule] var time: TM = new TM()        /** not valid for output files */
    private[xcMakeModule] var pro: Project = _

    def getFileName: XString = {
      if (fd != null) {
        fd.getName
      } else {
        name.getMangledName
      }
    }

    def addOut(md: Mode): Unit = {
      val m = include(this.pro, this.name, md, Set32.of(tag_out.toUByte))
      val n = new Node()
      n.mod = m
      n.next = this.out
      this.out = n
      n.mod.from = this
    }

    def getStringID: XString = {
      if (this.mode == md_jbc) {
        JStrings.format("0%S", this.name.toStringID)
      } else if (this.mode == md_sym) {
        JStrings.format("1%S", this.name.toStringID)
      } else {
        throw new AssertionError
      }
    }

    def getProject: Project = this.pro
  }


  class Ext {

    private[xcMakeModule] var mode: Mode = _
    private[xcMakeModule] var compilable: Boolean = _
    private[xcMakeModule] var name: XString = _
    private[xcMakeModule] var next: Ext = _

  }


  abstract class Project extends AbstractProject[File] {

    /*RO*/ var list: File = _        /** list of all project files */
    /*RO*/ var hashtable: Hashtable = _
    /*RO*/ var nodes: Node = _        /** ordered module list (after Regulate) */
    var errs: Int = _     /** number of errors */
    var fileName: XString = _  /** project name or NIL */
    private[xcMakeModule] var exts: Ext = _
    /*RO*/ var jbcExt: XString = _
    private[xcMakeModule] var last: File = _
    var tail: Node = _
    private[xcMakeModule] var tm: TM = new TM()
    var usgTm: TM = new TM()
    private[xcMakeModule] var iconFlag: Boolean = _  // means that this project has icon in its modules

    def createFD(n: Node): Unit = {
      // abstract
      throw new AssertionError
    }

    def addImport(cls: pcO.Class, fromsym: Boolean): Unit = {
      // abstract
      throw new AssertionError
    }

    def compile(): Unit = {
      // abstract
      throw new AssertionError
    }

    def init(): Unit = {
      this.list = null
      this.last = null
      this.errs = 0
      this.fileName = null
      this.tm.undef = true
      this.usgTm.undef = true
      this.setEquations()
      this.iconFlag = false
    }

    def hasIcon: Boolean = this.iconFlag

    def destroy(): Unit = {
    }

    def setEquations(): Unit = {
      val cxt = env.context
      env.context = null

      this.setExt("JAVABC", "class", md_jbc, compilable = true)
      this.setExt("JAR", "jar", md_jar, compilable = true)
      this.setExt("WAR", "war", md_war, compilable = true)
      this.setExt("ZIP", "zip", md_zip, compilable = true)
      this.setExt("SYM", "sym", md_sym, compilable = false)
      this.setExt("OBJEXT", "obj", md_obj, compilable = false)
      this.setExt("LIB", "lib", md_lib, compilable = false)
      this.setExt("DLL", FS.TARGET.dllExtension, md_dll, compilable = false)
      this.setExt("USG", "usg", md_usg, compilable = false)
      this.setExt("FUS", "fus", md_fus, compilable = false)
      this.setExt("ICOEXT", "ico", md_ico, compilable = false)
      this.setExt("RESEXT", "res", md_res, compilable = false)
      this.setExt("EFSEXT", "efs", md_efs, compilable = false)
      this.setExt("JITCACHEEXT", "cache", md_jitcache, compilable = false)

      this.jbcExt = this.modeToExt(md_jbc)

      if (this.fileName != null) {
        env.config.setEquation2("PRJ", this.fileName)
        env.config.setEquation2("PROJECT", FS.getBaseName(this.fileName))
      } else {
        env.config.setEquation("PROJECT", "")
      }
      env.context = cxt
      jcp.needVerify = !env.config.option("VerifyNone")
      jcp.relaxVerify = !env.config.option("VerifyAll")
    }

    override def setErr(): Unit = {
      errs += 1
      env.errDetected = true
    }

    def setExt(name: String, def0: String, mode: Mode, compilable: Boolean): Unit = {
      var s = env.config.equation(name) //TODO kill it
      if (s == null) {
        s = JStrings.internJString(def0)
      }
      val x = new Ext()
      x.mode = mode
      x.compilable = compilable
      x.name = s
      x.next = this.exts
      this.exts = x
    }

    def regulate(): Unit = {
      var place: xPDB.Placeholder = null

      if (this.errs > 0) {
        return
      }

      this.checkBundleDesc()

      var mainstr = env.config.equation("MAIN")
      if (mainstr != null && mainstr.nonEmpty) {
        val appendedmod = this.appendFile(mainstr)
        if (appendedmod == null) {
          this.errModNotFound(mainstr)
        }
      }

      if (this.errs > 0) {
        return
      }
      var l = this.list
      var head = new Node()
      head.next = null
      var tail = head
      /* tie and create file descriptors */
      while (l != null) {
        var n = l.out
        while (n != null) {
          assert(n.mod.fd == null)
          assert(n.mod.tags contains tag_out)
          n = n.next
        }
        if (l.fd == null) {
          //TODO: remove this old evil. It seems nobody needs now file descriptors for project files.
          if (l.tags contains tag_out) {
            place = xPDB.findPlaceToWriteTo(l.name.getMangledName, modeToPDBType(l.mode))
          } else {
            place = xPDB.findPlaceToReadFrom(l.name.name, modeToPDBType(l.mode))
          }
          l.fd = place.getFileDescriptor
          assert(l.fd != null)
        }
        if (!(l.tags contains tied)) {
          tail = tie(l, tail)
        }
        l = l.next
      }
      head = head.next
      this.nodes = head
      this.tail = tail
      if (head == null) {
        env.errors.envError(msg_no_modules)
        setErr()
      } else {
        while (head != null && head.mod.mode != md_jbc) {
          head = head.next
        }
        if (head == null) {
          env.errors.envError(msg_no_classfiles)
          setErr()
        }
      }
      if (this.errs != 0) {
        env.errDetected = true
      }
    }
    /** Builts module list in compilation order */

    def checkBundleDesc(): Unit = {
      checkAppDir()

      if (!pcO.isCustomClassloaders) {
        return
      }

      if (pcO.isSpringBoot) {
        val springBootArchive = env.config.equation("springbootarchive")
        assert(springBootArchive != null)

        // check that we have the only jar in the classpath
        var m = this.list
        while (m != null) {
          if (CPEs contains m.mode) {
            if (!springBootArchive.equals(m.name.getReadableName)) {
              env.errors.fault(ErrMsg653, m.name.getReadableName)
            }
          }
          m = m.next
        }
      }

      if (!O2Env.env.enabled(DoNotCallImportResolver)) {
        invokeImportResolver(env.config.equation("APPTYPE"))
      }
      importresolvercalled = true
      val desc = FS.addPath(env.config.equation("PDBNAME"), JStrings.newJString("bundledesc.imports"))
      val fd = xfs.sys.createFileDescriptor(desc)
      if (!fd.exists) {
        env.errors.fault(ErrMsg648, fd.getName)
      }
      this.readBundleDescriptor(fd)
      env.config.setOption("writeclassloadersIDtable", value = true)
    }

    def getProjectFileByStringID(id: XString): File = {
      var md: SetOfModes = SetOfModes.empty

      if (id.charAt(0) == '0') {
        md = SetOfModes.of(md_jbc)
      } else {
        assert(id.charAt(0) == '1')
        md = SetOfModes.of(md_sym)
      }
      val name = pcNames.fromStringID(id.substring(1))
      this.getProjectFile(name, md)
    }

    def getProjectFile(name: pcNames.NAME, md: SetOfModes): File = {
      var l = search(this.hashtable, name)
      while (l != null) {
        if (md contains l.asInstanceOf[File].mode) {
          return l.asInstanceOf[File]
        }
        l = l.s
      }
      null
    }

    def appendLambdaClass(classname: pcNames.NAME, fPar: xfs.FileDescriptor, fromsym: Boolean): File = {
      var f = fPar

      if (!fromsym) {
        include(this, classname, md_jbc, Set32.empty)
      } else {
        assert(f == null)
        f = lookupSym(this, classname)
        assert(f != null)
        scan(this, f, classname, md_sym)
      }
    }

    def appendBundleClass(bundle: File, classname: pcNames.NAME, f: xfs.FileDescriptor): File = {
      val mod = include(this, classname, md_jbc, Set32.empty)
      mod.fd = f
      mod.context = bundle.context
      mod.host = bundle
      mod
    }

    def appendClassloaderEntry(bidPar: XString, cpeMode: CPEntryMode, bidInInternalForm: Boolean, userDef: Boolean = false): File = {
      var bid = bidPar

      assert(cpeMode != cpe_app) // cpe_app may come from classpathentry directive only

                         // and it has another semantic for directive argument
                         // in compare with cpe_appclassloader that comes
                         // with classloaderentry
      var cxt = env.context
      env.context = null // we should change config to global context
      checkAppDir()
      env.context = cxt

      if (!pcO.isCustomClassloaders) {
        env.errors.envError(ErrMsg511)
        sys.exit(511)
      }

      if (!bidInInternalForm) {
        bid = this.convertBidToInternalForm(bid, cpeMode)
      }
      var bundleFile = this.createBundleFileDescriptor0(bid, cpeMode)

      if (bundleFile == null) {
        setErr()
        return null
      }

      if (cpeMode == cpe_appclassloader) {
        val cpe = bundleFile.getName

        if (!bidInInternalForm) {
          // BID is not in internal form when we read project not imports file.
          // When we read project we should update additionalclasspath equation
          // with new classpath entry.
          cxt = env.context
          env.context = null // we should change equation in global context
          var additionalclasspath = env.config.equation("additionalclasspath")
          if (additionalclasspath == null || additionalclasspath.isEmpty) {
            additionalclasspath = cpe
          } else {
            additionalclasspath = JStrings.format("%S;%S", additionalclasspath, cpe)
          }
          env.config.setEquation2("additionalclasspath", additionalclasspath)
          env.context = cxt
        }

        return this.appendClasspathEntry(cpe)
      }

      if (bundleFile.isInstanceOf[xmZip.FileDescriptor] && !bundleFile.isDirectory) {
        bundleFile = extractNestedJar(bundleFile)
      }

      assert(bundleFile != null)

      val mod = scan(this, bundleFile, pcNames.newBundleName(bid), md_bundle)

      env.config.setContext(mod.context)
      env.config.setOption("visibleresource", value = true) // all bundles must be visible through EFS
      env.config.removeContext(mod.context)

      mod.cpeMode = cpeMode
      if (userDef) {
        mod.tags += userdef.toUByte
      }
      mod
    }

    /*
      Creates FileDescriptor for specified BID
      (Bundle Id -- for historical reason).
    */
    def createBundleFileDescriptor0(bid: XString, cpeMode: CPEntryMode): xfs.FileDescriptor = {
      var pbid: pcNames.ParsedBundleID = null

      val appDir = env.config.equation("APPDIR")
      assert(appDir != null)

      if (cpeMode == cpe_appclassloader) {   // for application classloader entry format is:
                                           //   1) relative on application root or
                                           //   2) absolute path
                                           // in both situations container is equal with bid and entry is NIL
        pbid = pcNames.newParsedBundleID(bid, null)
      } else {
        pbid = pcNames.parseBundleID(bid)
      }

      val relToAppContainer = FS.addPath(appDir, pbid.container)

      var bundleFile = xfs.sys.createFileDescriptor(relToAppContainer)

      if (!bundleFile.exists) {
        if (cpeMode == cpe_appclassloader) {
          // for app classloader entries may be referenced with absolute path
          bundleFile = xfs.sys.createFileDescriptor(pbid.container)
        }
      } else {
        pbid.container = relToAppContainer
      }

      if (!bundleFile.exists) {
        env.errors.envError(err_bundle_not_found, bid, pbid.container)
        return null
      }

      val isDir = !FS.isJar(pbid.container) // we assume that something is dir if it does not end with .jar,

                                       // because XDS lacks proper isDirectory predicate
      if (pbid.entry != null) {
        assert(cpeMode != cpe_appclassloader)
        if (isDir) {
          if (!FS.isJar(pbid.entry)) {
            bundleFile = bundleFile.getDir(pbid.entry)
          } else {
            pbid.container = FS.addPath(pbid.container, pbid.entry)
            bundleFile = xmZip.createFileDescriptor(pbid.container, JStrings.jstrDot)
          }
        } else {
          bundleFile = xmZip.createFileDescriptor(pbid.container, pbid.entry)
        }
      } else if (!isDir && cpeMode != cpe_appclassloader) {
        bundleFile = xmZip.createFileDescriptor(pbid.container, JStrings.jstrDot)
      }

      if (!bundleFile.exists) {
        env.errors.envError(err_bundle_entry_not_found, bid, pbid.entry, pbid.container)
        return null
      }

      bundleFile
    }

    /*
      Translates bid to internal form.

      BID syntax: <Containter>:/<Entry>

      BID internal form symantic:
         <Container> is file (dir or jar/war) related to appdir,
           except appclassloader where container maybe absolute file also.

      BID external form semantic (used in projects):
         For webapp classloader <Container> is file without .war extension,
         else the semantic is the same as for internal form

      <Entry> is item inside container for both forms.
    */
    def convertBidToInternalForm(bidPar: XString, cpeMode: CPEntryMode): XString = {
      var bid = bidPar

      if (cpeMode == cpe_webapp) {
        val appDir = env.config.equation("APPDIR")
        assert(appDir != null)
        val pbid = pcNames.parseBundleID(bid)
        var relToAppContainer = FS.makeFileName(appDir, pbid.container)
        var bundleFile = xfs.sys.createFileDescriptor(relToAppContainer)
        if (!bundleFile.exists) {
          // add .war extension for webapp entries
          relToAppContainer = FS.makeFileName(appDir, pbid.container, this.modeToExt(md_war))
          bundleFile = xfs.sys.createFileDescriptor(relToAppContainer)
          if (bundleFile.exists) {
            // convert bid to internal form
            if (pbid.entry != null) {
              bid = JStrings.format("%S.%S:/%S", pbid.container, this.modeToExt(md_war), pbid.entry)
            } else {
              bid = JStrings.format("%S.%S", pbid.container, this.modeToExt(md_war))
            }
          }
        }
      }
      bid
    }

    def appendClasspathEntry(fname: XString): File = {
      var f: xfs.FileDescriptor = null

      var tname = FS.HOST.fromPlatform(fname)
      val ext = FS.getExt(tname)
      if (fname.equals(JStrings.jstrDot) || fname.equals(JStrings.jstrTwoDots)) {
        f = xfs.sys.createFileDescriptor(fname)
        if (!f.exists) {
          f = null
        }
      } else {
        tname = FS.cutExt(tname)
        f = lookupFile(tname, ext, lookInCurrentDir = true)
      }
      if (f == null) {
        env.errors.envError(err_classpathentry_not_found, fname)
        setErr()
        return null
      }
      var mode = this.extToMode(ext)
      if (!(SetOfModes.of(md_jar, md_war, md_zip) contains mode)) {
        mode = md_dir
      }
      val mod = scan(this, f, pcNames.newFileNameByFD(f), mode)
      assert(mod != null)
      mod.tags += userdef.toUByte
      mod
    }

    def getModuleClassFD(module: File, clazz: XString): xfs.FileDescriptor = {
      assert(MODs contains module.mode)
      if (module.mode != md_bundle) {
        // check that given class inside the module
        val res = resolveImport(module, clazz)
        assert(res.host eq module)
        lookupFile(clazz, this.jbcExt, lookInCurrentDir = false)
      } else {
        // Implemented in xcMain
        throw new AssertionError
      }
    }

    /** Resolves import in classpath only.
      * Resolution for custom classloaders is implemented in xcMain via overriding this method.
    */
    def resolveImport(module: File, clazz: XString): ImportResult = {
      assert(MODs contains module.mode)
      val cname = pcNames.newClassName(clazz)
      val f = lookupSym(this, cname)
      if (f != null) {
        if (!(isFromProfilePDB(f) && isFromExcludedPackage(cname)) && !isFromMainPDB(f)) {
          return ImportResult(EXTERNAL)
        }
      }
      if (isExcluded(clazz)) {
        return ImportResult(ABSENT)
      }
      var mod = findFile(this, cname, md_jbc)
      if (mod != null) {
        assert(mod.host != null)
        ImportResult(NORMAL, mod.host)
      } else {
        val fd = lookupFile(clazz, this.jbcExt, lookInCurrentDir = false)
        if (fd != null) {
          val hostFD = getHostFDByClassFD(fd, cname)
          mod = findFile(this, getFileNameByFD(hostFD), this.getModeByFD(hostFD))
          ImportResult(NORMAL, mod)
        } else {
          ImportResult(ABSENT)
        }
      }
    }

    def checkFName(fname: XString, checkname: Boolean): Boolean = {
      if (!checkname) {
        return true
      }
      var dots = false
      for (i <- 0 until fname.length) {
        if (fname.charAt(i) == '\\') {
          env.errors.envError(err_back_slash, fname)
          setErr()
          return false
        } else if (fname.charAt(i) == '.') {
          if (dots) {
            env.errors.envError(err_dots, fname)
            setErr()
            return false
          } else {
            dots = true
          }
        } else if (fname.charAt(i) == ' ') {
          env.errors.envError(err_spaces, fname)
          setErr()
          return false
        }
      }
      true
    }

    def errModNotFound(fname: XString): Unit = {
      if (!hasSpaces(fname)) {
        env.errors.envError(err_module_not_found, fname)
      } else {
        env.errors.envError(err_module_not_found2, fname)
      }
      setErr()
    }

    def appendJava(oname: XString, symImport: Boolean = false, doError: Boolean = true): File = {
      var f: xfs.FileDescriptor = null
      var mode: Mode = 0
      val checkNameRes = new CheckClassNameResult()

      val cname = pcNames.newClassName(oname)
      val cnameOrig = pcNames.newClassName(JBCPreprocessor.originalScalaClassName(oname))
      if (isExcluded(oname)) {
        return null
      }
      if (env.stage != env.BACK || xPDB.isProfileBuild) {
        f = searchAnyJava(this, cnameOrig)
      } else {
        // do not try to search and append classes at backend stage
        f = null
      }
      if (f == null) {
        f = lookupSym(this, cname)
        if (f != null) {
          if (isFromMainPDB(f)) {
            // found sym but not class -- it seems to be from previous build
            // we should not reuse sym-file for class
            // which import does not exist in class form
            // except for profile compiling
            if (!xPDB.isProfileBuild) {
              assert(!symImport)
              return null
            }
          }
          if (!symImport && isFromProfilePDB(f) && isFromExcludedPackage(cname)) {
            return null
          }
          mode = md_sym
        } else {
          if (pcO.classAbsenceErr && doError) {
            env.errors.envError(err_module_not_found, oname)
            setErr()
          }
          return null
        }
      } else {
        mode = this.extToMode(FS.getExt(f.getName))
        val mod = findFile(this, cnameOrig, mode)
        if (mod != null || env.stage != env.CHECKING && (env.config.tags contains env.superimportonly)) {
          checkClassName(this, f, cnameOrig, checkNameRes)
          if (checkNameRes.code != symexist) { // not duplicated
            return mod
          }
        }
      }
      scan(this, f, cnameOrig, mode)
    }

    def getModeByFD(fd: xfs.FileDescriptor): Mode = {
      if (fd.isDirectory) {
        md_dir
      } else {
        this.extToMode(FS.getExt(fd.getName))
      }
    }

    def readBundleDescriptor(f: xfs.FileDescriptor): Unit = {
      // abstract
      throw new AssertionError
    }

    override def appendFile(fileName: XString): File = {
      append(fileName, checkname = true, explicitAppend = true)
    }

    def append(fname: XString, checkname: Boolean = true, explicitAppend: Boolean = true): File = {
      /** Tries to find module,
        if extension is omitted.
      */
      var f: xfs.FileDescriptor = null
      var mode: Mode = 0

      val tname = FS.cutExt(fname)
      val ext = FS.getExt(fname)
      val cname = pcNames.newClassName(tname)
      if (isExcluded(tname)) {
        return null
      }
      if (ext.isEmpty) {
        if (!this.checkFName(fname, checkname)) {
          return null
        }
        f = searchAnyJava(this, cname)
        if (f == null) {
          this.errModNotFound(fname)
          return null
        }
        mode = this.extToMode(FS.getExt(f.getName))
      } else {
        mode = this.extToMode(ext)
        if (mode == md_unknown) {
          if (ext.equals2("java")) {
            env.errors.envError(err_java_extension, tname)
          } else if (ext.equals2("prj")) {
            env.errors.envError(ErrMsg474, tname)
          } else {
            env.errors.envError(err_undefined_extension, ext)
          }
          setErr()
          return null
        }
        if (mode == md_jbc && !this.checkFName(fname, checkname)) {
          return null
        }
        f = lookupFile(tname, ext, lookInCurrentDir = true)
        if (f == null) {
          this.errModNotFound(fname)
          return null
        }
        if (mode == md_ico) {
          this.iconFlag = true
        }
      }
      val mod = scan(this, f, getNameByMode(tname, f, mode), mode)
      if (mod != null) {
        mod.tags += userdef.toUByte
        if (mode == md_jbc && explicitAppend) {
          // JET-11890: mark all explicitly added classes to be compiled.
          mod.tags += compileall.toUByte
        }
      }
      mod
    }

    def getTime: Unit = {
      if (this.tm.undef) {
        if (this.fileName == null) {
          this.tm.exist = false
        } else {
          this.tm.time = xfs.sys.modifyTime(this.fileName)
          this.tm.exist = xfs.sys.exists(this.fileName)
        }
        if (!this.tm.exist) {
          this.tm.time = Int.MinValue
        }
        this.tm.undef = false
      }
    }

    def extToMode(s: XString): Mode = {
      /* returns -1, if not found */
      var x = this.exts
      while (x != null) {
        if (xfs.sys.compareExtSys(x.name, s)) {
          return x.mode
        }
        x = x.next
      }
      md_unknown
    }

    def modeToExt(mode: Mode): XString = {
      assert(mode != md_unknown)
      var x = this.exts
      while (x != null) {
        if (x.mode == mode) {
          return x.name
        }
        x = x.next
      }
      null
    }
  }

  type CState = UByte
  val none: CState = UByte(0)
  val compilable: CState = UByte(1)

  /*---------------------------------------------------------------*/


  private class DirIterator extends xfs.DirIterator {

    private[xcMakeModule] var p: Project = _
    private[xcMakeModule] var dir: xfs.FileDescriptor = _
    private[xcMakeModule] var name: XString = _
    private[xcMakeModule] var host: File = _

    override def entry(name: XString, dir: Boolean): Boolean = {
      var i: DirIterator = new DirIterator()

      if (name.equals(JStrings.jstrDot) || name.equals(JStrings.jstrTwoDots)) {
        return false
      }
      i.name = FS.addPath(this.name, name)
      if (dir) {
        i.p = this.p
        i.dir = this.dir.getDir(name)
        i.host = this.host
        if (i.dir.iterateDir(i)) {
        }
      } else {
        val ext = FS.getExt(name)
        if (ext.equals(this.p.jbcExt)) {
          dirclassamount += 1
          val file = this.p.append(i.name, checkname = false, explicitAppend = false)
          if (file != null) {
            file.host = this.host
          }
        }
      }
      false
    }

  }

  type CheckClassNameResCode = UByte
  private val correct: CheckClassNameResCode = UByte(0)
  private val noncorrect: CheckClassNameResCode = UByte(1)
  private val symexist: CheckClassNameResCode = UByte(2)
  private val badlettercase: CheckClassNameResCode = UByte(3)


  private class CheckClassNameResult {
    private[xcMakeModule] var code: CheckClassNameResCode = _
    private[xcMakeModule] var correctName: pcNames.ClassName = _ // name in correct case, when name is given in bad case
  }

  // `host` is host module (containing class files, if importType = NORMAL)
  case class ImportResult(importType: ImportResolutionType, host: File = null)

  /** tags */
  val userdef: Int = 1 /** file name is user defined */
  val isCompilable: Int = 2
  private val tied: Int = 9
  val tag_out: Int = 10
  val objs: Int = 11
  val redundant: Int = 12
  val importnotscanned: Int = 15
  val compileall: Int = 16 // bundle should compiled as a whole
  val autodef: Int = 17  // bundle is defined automatically (via importresolver)
  val bootstrapjar: Int = 18 // this bundle is bootstrap jar actually
  val springbootarchive: Int = 19
  private val err_undefined_extension = ErrMsg406
  private val err_java_extension = ErrMsg419
  private val err_module_not_found = ErrMsg407
  private val err_module_not_found2 = ErrMsg405
  private val err_bundle_not_found = ErrMsg478
  private val err_bundle_entry_not_found = ErrMsg479
  private val err_classpathentry_not_found = ErrMsg480
  val err_back_slash = ErrMsg455
  val err_dots = ErrMsg456
  private val err_spaces = ErrMsg466
  private val msg_no_modules = ErrMsg413 /* */
  private val msg_no_classfiles = ErrMsg417
  private var curDirectoryAdded: Boolean = false
  val MODs: SetOfModes = SetOfModes.of(md_jar, md_zip, md_dir, md_bundle, md_war)
  private val CPEs: SetOfModes = SetOfModes.of(md_jar, md_zip, md_dir, md_war)
  /*----------------------------------------------------------------*/
  private var rtjarcompiling: Boolean = false
  private var inclasspathentryreading: Boolean = false
  private var duplicateClassAmount: Int = _
  private var dirclassamount: Int = _
  private val excludelist = mutable.HashSet.empty[XString]
  private var importresolvercalled: Boolean = false
  private var defaultRedirectionsAdded: Boolean = false
  private var lastNameLen: Int = 20
  var javaseExcludedPackages: mutable.HashSet[XString] = _
  var javaseIncludedPackages: mutable.HashSet[XString] = _
  var javaseExcludeTypes: mutable.HashSet[XString] = _
  /*RO*/ var compact1Packages: mutable.HashSet[XString] = _
  /*RO*/ var compact2Packages: mutable.HashSet[XString] = _
  /*RO*/ var compact3Packages: mutable.HashSet[XString] = _
  /*RO*/ var fulljrePackages: mutable.HashSet[XString] = _
  /*RO*/ var fullJreIncludeTypes: mutable.HashSet[XString] = _
  private var appDirChecked: Boolean = false

  /*---------------------------------------------------------------*/
  def search(t: Hashtable, nm0: pcNames.NAME): CUnit = {
    if (t == null) {
      return null
    }
    val nm = if (pcNames.isClassName(nm0)) {
      pcNames.newClassName(JBCPreprocessor.originalScalaClassName(nm0.name))
    } else {
      nm0
    }
    val e = t.get(nm)
    if (e == null) {
      null
    } else {
      e.asInstanceOf[CUnit]
    }
  }

  /*---------------------------------------------------------------*/
  private def findFile(p: Project, name: pcNames.NAME, mode: Mode): File = {
    var l = search(p.hashtable, name)
    while (l != null) {
      if (l.asInstanceOf[File].mode == mode) {
        return l.asInstanceOf[File]
      }
      l = l.s
    }
    null
  }

  private def include(p: Project, name: pcNames.NAME, mode: Mode, tags: Set32): File = {
    var x = findFile(p, name, mode)
    if (x != null) {
      return x
    }
    x = new File()
    x.name = name
    x.fd = null
    x.mode = mode
    x.out = null
    x.next = null
    x.tags = tags
    x.time.undef = true
    x.pro = p
    if (p.list == null) {
      p.list = x
    } else {
      p.last.next = x
    }
    p.last = x
    p.hashtable = x.append(p.hashtable)
    mode match {
      case `md_jbc` =>
        x.addOut(md_sym)
        x.addOut(md_obj)
      case `md_usg` =>
      case `md_sym` =>
      case _ =>
    }
    assert(x.name eq name)
    x
  }

  private def appendAllFromJar(p: Project, str: XString, host: File, isSystemJar: Boolean): Int = {
    var jarclassamount = 0
    duplicateClassAmount = 0

    val zf = ZipFile.newZipFile(str)
    if (zf != null) {
      val entries = zf.entries
      while (entries.hasNext) {
        val ze = entries.next()
        val ename = ze.getName
        val ext = FS.getExt(ename)
        val cname = FS.cutExt(ename)
        if (ext.nonEmpty) {
          val mode = p.extToMode(ext)
          if (mode == md_jbc) {
            if (!isSystemJar || !isFromExcludedPackage(pcNames.newClassName(cname))) {
              jarclassamount += 1
              val file = p.append(ename, checkname = false, explicitAppend = false)
              if (file != null) {
                file.host = host
              }
            }
          }
        }
      }
      zf.close()
    }

    jarclassamount - duplicateClassAmount
  }

  private def appendAllFromDir(p: Project, str: XString, host: File): Int = {
    var d: DirIterator = new DirIterator()

    dirclassamount = 0
    duplicateClassAmount = 0
    d.p = p
    d.dir = xfs.sys.createFileDescriptor(str)
    d.name = JStrings.jstrEmpty
    d.host = host
    if (d.dir.iterateDir(d)) {
    }
    dirclassamount - duplicateClassAmount
  }

  private def compileAll(mod: File): Boolean = {
    env.config.setContext(mod.context)
    val optimize = env.config.equation("optimize")
    val protect = env.config.equation("protect")
    env.config.removeContext(mod.context)
    optimize == null || optimize.equals2("ALL") || protect != null && protect.equals2("ALL")
  }

  private def isSystemJar(mod: File): Boolean = {
    if (xPDB.isProfileBuild) {
      env.config.setContext(mod.context)
      val res = env.config.option("systemclasses") && !env.config.option("localecomponent")
      env.config.removeContext(mod.context)
      res
    } else {
      false
    }
  }

  private def putJavaFXPreloaderIntoVMProp(preloader: XString): Unit = {
    val JET_VM_PROP: String = "jetvmprop"
    val JAVAFX_PRELOADER_PROPERTY: String = "-Djavafx.preloader"

    val jetvmprop = env.config.equation(JET_VM_PROP)
    if (jetvmprop == null || jetvmprop.equals(JStrings.jstrEmpty)) {
      env.config.setEquation2(JET_VM_PROP, JStrings.format("-Djavafx.preloader=%S", preloader))
    } else if (jetvmprop.indexOf(JStrings.newJString(JAVAFX_PRELOADER_PROPERTY)) < 0) {
      env.config.setEquation2(JET_VM_PROP, JStrings.format("%S -Djavafx.preloader=%S", jetvmprop, preloader))
    }
  }

  def parseExcludeList(value: XString): Unit = excludelist ++= value.split(';')

  def isExcluded(className: XString): Boolean = excludelist.contains(className)

  private def checkJavaFXManifestAttrs(attr: Manifest.Attributes, main: XString): XString = {
    val javafxapp = attr.getValue(Manifest.JAVAFX_APP_CLASS)
    if (javafxapp != null && (main == null || !main.equals2("com/javafx/main/Main"))) {
      // Do not compile com/javafx/main classes: they are added by old
      // JavaFX packager for checking that JavaFX is present in JRE
      // and if it is not present shows dialog using Swing.
      // So if we compile them, JavaFX application will depend on Full JRE.
      excludelist += XString("com/javafx/main/Main")
      excludelist += XString("com/javafx/main/Main$1")
      excludelist += XString("com/javafx/main/Main$2")
      excludelist += XString("com/javafx/main/NoJavaFXFallback")
    }
    if (main == null || main.isEmpty || main.equals(javafxapp)) {
      val preloader = attr.getValue(Manifest.JAVAFX_PRELOADER_CLASS)
      if (preloader != null) {
        putJavaFXPreloaderIntoVMProp(preloader)
      }
    }
    javafxapp
  }

  private def readClasspathEntry(p: Project, cpEntry: XString, mod: File): Unit = {
    var classamount: Int = 0

    val md = mod.mode
    val cxt = env.context
    env.context = null
    inclasspathentryreading = true
    var silent = false
    var springbootdetected = false

    if (md == md_jar || md == md_war) {
      var attr: Manifest.Attributes = null
      val m = Manifest.getManifest(cpEntry)
      if (m != null) {
        attr = m.getMainAttributes
      }
      if (attr != null) {
        var main = env.config.equation("MAIN")
        val javafxapp = checkJavaFXManifestAttrs(attr, main)
        if (main == null || main.isEmpty) {
          if (javafxapp == null) {
            main = attr.getValue(Manifest.MAIN_CLASS)
          } else {
            main = javafxapp
          }
          if (main != null) {
            if (!importresolvercalled && (main.equals2("org.springframework.boot.loader.JarLauncher") || main.equals2("org.springframework.boot.loader.WarLauncher"))) {
              env.config.setEquation2("APPTYPE", JStrings.newJString("SPRINGBOOT"))
              env.config.setEquation2("SPRINGBOOTARCHIVE", cpEntry)
              springbootdetected = true
            }
            env.config.setEquation2("MAIN", main.replace('.', '/'))
            if (p.fileName == null) {
              env.config.setEquation2("PROJECT", FS.getBaseName(cpEntry))
            }
          }
        }
      }
    }

    if (!springbootdetected) {
      // spring boot jar is added to lookups after ImportResolver call
      // to allow to prepend patched jar file with JarFile class.
      val eq0 = JStrings.format("*.class=%S", cpEntry)
      env.config.setEquation2("LOOKUP", eq0)
      xfs.sys.parseRed(eq0)
    }

    silent = springbootdetected || pcO.isTomcat
    if (!silent) {
      env.info.print("Reading %S ... ", cpEntry)
    }
    if (!springbootdetected) {
      if (compileAll(mod)) {
        mod.tags += compileall.toUByte
      }
      env.context = cxt
      if (md == md_dir) {
        classamount = appendAllFromDir(p, cpEntry, mod)
      } else {
        classamount = appendAllFromJar(p, cpEntry, mod, isSystemJar(mod))
      }
      if (!silent) {
        env.info.print(" %d classes", classamount)
      }
      xmErrors.jarsClassAmount += classamount
      env.context = null
    }
    if (!silent) {
      env.info.print("\\n")
    }
    env.context = cxt
    inclasspathentryreading = false
  }

  private def getHostFDByClassFD(fPar: xfs.FileDescriptor, classname: pcNames.NAME): xfs.FileDescriptor = {
    var f = fPar

    if (f.isInstanceOf[xmZip.FileDescriptor]) {
      val fname = f.asInstanceOf[xmZip.FileDescriptor].zname
      f = xfs.sys.createFileDescriptor(fname)
    } else {
      var tname = FS.cutExt(f.getName)
      val cname = classname.name
      assert(FS.HOST.fromPlatform(FS.HOST.caseToPlatform(tname)).endsWith(FS.HOST.caseToPlatform(cname)))
      tname = tname.substring(0, tname.length - cname.length)
      if (tname.isEmpty) {
        tname = JStrings.jstrDot
      } else if (!tname.equals2("/")) {
        assert(tname.charAt(tname.length - 1) == '/')
        tname = tname.substring(0, tname.length - 1)
      }
      f = xfs.sys.createFileDescriptor(tname)
    }
    assert(f.exists)
    f
  }

  private def getFileNameByFD(fd: xfs.FileDescriptor): pcNames.NAME = {
    if (fd.isDirectory) {
      pcNames.newFileName(fd.getName, JStrings.jstrEmpty)
    } else {
      val fname = fd.getName
      val tname = FS.cutExt(fname)
      val ext = FS.getExt(fname)
      pcNames.newFileName(tname, ext)
    }
  }

  /*
    Tries to add to classpath a location (jar/zip or dir)
    where we found a class after we add it to the project.
    There is no need to do this if the class is added to the project as result
    of classpath entry adding (its location is already added) and
    if we compile our RT DLLs.
  */
  private def autoCompleteClasspathForClass(p: Project, f: xfs.FileDescriptor, classname: pcNames.NAME): File = {
    if (rtjarcompiling || inclasspathentryreading) {
      return null
    }
    val hostFD = getHostFDByClassFD(f, classname)
    if (!curDirectoryAdded && hostFD.getName.equals(JStrings.jstrDot)) {
      // add cur directory to lookups explicitly
      xfs.sys.parseRed(JStrings.newJString("*.class=."))
      curDirectoryAdded = true
    }
    val mod = include(p, getFileNameByFD(hostFD), p.getModeByFD(hostFD), Set32.empty)
    assert(MODs contains mod.mode)
    if (mod.fd == null) {
      mod.fd = hostFD
    }
    mod
  }

  def setDefaultLookups(): Unit = {
    if (!defaultRedirectionsAdded) {
      defaultRedirectionsAdded = true

      if (O2Env.env.valueOfOrNull(STDLib) != null) {
        if (!StdLibCompiler.checkOrCompileStdLib()) {
          env.exit(526)
        }
      }

      val dynLibs: Set[String] = {
        val dynLibsVal = O2Env.env.valueOfOrNull(DynLibs)
        if (dynLibsVal != null) {
          dynLibsVal.split(',').toSet
        } else {
          Set.empty
        }
      }

      val uselibraryVal = O2Env.env.valueOfOrNull(UseLibrary)
      if (uselibraryVal != null) {
        for {
          l <- uselibraryVal.split(',')
          if l.nonEmpty && !dynLibs.contains(l)
        } {
          val libraryPath = if (!pcO.isCangjie || (targetArch == CBC)) {
            env.findLibraryPath(l)
          } else {
            env.findLibraryPath(l) / "static"
          }
          if (libraryPath == null) {
            env.errors.fault(ErrMsg526, XString(l))
            return
          }
          val libraryPDB = xArchivePDB.ctor.open(PDBKind.Library, XString(libraryPath.toString))
          if (libraryPDB == null) {
            env.errors.fault(ErrMsg526, XString(l))
            return
          }

          xPDB.manager.registerAuxPDB(libraryPDB)
        }
      }

      if (!env.config.option("nodefaultlookups") && !isStandalone) {
        val develop_dir = env.getDevelopDir

        if (env.config.option("nodefaultsymbodlookup")) {
          rtjarcompiling = true
        } else {
          assert(xPDB.manager != null)
          val profilePDB = xArchivePDB.openProfilePDB()
          if (profilePDB == null) {
            env.errors.fault(ErrMsg524, env.getProfileDir)
          }
          xPDB.manager.registerAuxPDB(profilePDB)
        }

        import xPDB.ContentType as CT

        val lib_redir = JStrings.format("%S/lib/lres", develop_dir)
        val ctypes = CT.Set(CT.LIB, CT.RES, CT.EFS, CT.EFSDATA, CT.LI)
        val lresPDB = xDirectoryPDB.openDirectoryPDB(PDBKind.LibResources, lib_redir, ctypes) // TODO: was `isProfile = true`
        xPDB.manager.registerAuxPDB(lresPDB)
      }
    }
  }

  def checkConflict(proj: Project, oname: pcNames.NAME, fd_javaPar: xfs.FileDescriptor): Boolean = {
    var fd_java = fd_javaPar

    setDefaultLookups()

    if (fd_java == null) {
      fd_java = searchAnyJava(proj, oname)
      if (fd_java == null) {
        return false
      }
    }

    val fname = oname.getMangledName
    if (fname == null) {
      return false
    }

    val symName = FS.cutExt(fname)
    val writePlace = xPDB.findPlaceToWriteTo(symName, xPDB.ContentType.SYM)
    val readPlace = xPDB.findPlaceToReadFrom(symName, xPDB.ContentType.SYM)

    if (readPlace != null && readPlace.exists) {
      val rnm = readPlace.fullName
      if (!rnm.equals(writePlace.fullName)) {
        val className = fd_java.getName
        if (className.startsWith(XString("com/huawei/excelsior/"))) {
          proj.errs += 1
        } else if (!env.config.option("IGNORECLASSDUPLICATION") && env.stage == env.CHECKING) {
          proj.errs += 1
        } else if (env.stage == env.CHECKING) {
          if (!env.config.option("SUPRESSCLASSDUPLICATIONMSG")) {
            env.errors.message(ErrMsg957, className, rnm)
          }
          duplicateClassAmount += 1
        } else {
          xmErrors.jarsClassAmount -= 1
        }
        return true
      }
    }
    false
  }

  private def decorWriteChecking(nm: XString): Unit = {
    var name_len = nm.length
    if (name_len > 70) {
      name_len = 70
    }
    lastNameLen = name_len
  }

  private def checkIndexAndAdd(p: Project, C: jcp.PtrClassInfo, i: Int): Unit = {
    val pool = C.constantPool
    if (i > 0 && i < C.constantPoolCount.toInt && pool(i).constantType == jcp.TagClass.toByte) {
      p.appendJava(pool(pool(i).indexName.toInt).bufferPtr)
    }
  }

  private def addSuperImport(p: Project, C: jcp.PtrClassInfo): Unit = {
    if (!(env.config.tags contains env.superimportonly) || C.constantPool == null) {
      return
    }
    checkIndexAndAdd(p, C, C.superClass.toInt)
    if (C.interfaceCount > UShort(0)) {
      for (i <- 0 until C.interfaceCount.toInt) {
        checkIndexAndAdd(p, C, C.interface(i).toInt)
      }
    }
  }

  private def isMain(name: pcNames.NAME): Boolean = {
    pcNames.isClassName(name) && name.name == env.config.equation("main")
  }

  private def checkClassName(p: Project, fd: xfs.FileDescriptor, name: pcNames.ClassName, res: CheckClassNameResult): Unit = {
    var C: jcp.PtrClassInfo = null
    var cname: pcNames.ClassName = null

    if (!fd.exists) {
      res.code = correct
      return
    }
    val fnm = fd.getName
    if (env.stage == env.CHECKING) {
      decorWriteChecking(fnm)
    }
    val file = fd.openSymFile()
    val loaded = try jcp.loadHead(file) finally file.close()
    if (loaded) {
      C = jcp.c
      val cnm = C.constantPool(C.constantPool(C.thisClass.toInt).indexName.toInt).bufferPtr
      val nameEquals = cnm == name.name
      val badLetterCase = !nameEquals && !FS.HOST.isCaseSensitive && !fd.isInstanceOf[xmZip.FileDescriptor] &&
        cnm.toUpperCase == name.name.toUpperCase
      if (jcp.relaxVerify && !nameEquals && !badLetterCase) {
        val pack = FS.getPath(name.name)
        val cpack = FS.getPath(cnm)
        if (cpack == pack) {
          env.errors.message(ErrMsg379, fnm, cnm)
        }
        res.code = noncorrect
      } else {
        if (badLetterCase) {
          cname = pcNames.newClassName(cnm)
        } else {
          cname = name
        }
        if (!checkConflict(p, cname, fd)) {
          addSuperImport(p, C)
          if (badLetterCase) {
            if (isMain(name)) {
              env.errors.message(ErrMsg379, fnm, cnm)
              res.code = noncorrect
            } else {
              // JET-7787 fix:
              // If the file system is case insensitive then it means
              // that if we ask whether a file exists providing
              // different cases of the same file name, it returns true,
              // if file exists with some arbitrary case.
              // It means that if a class file was added during directory
              // classpathentry scanning and its real name inside classfile differs
              // from name in the file system we should not reject it as unloadable
              // because during class loading with correct case it will be found
              // by the file system and correctly loaded.
              // So for this case, we need to correct project name of the class
              // with the class name in the class file.
              res.code = badlettercase
              res.correctName = cname
            }
          } else {
            res.code = correct
          }
        } else {
          res.code = symexist
        }
      }
    } else {
      // catch this error later (jbcFront)
      if (!checkConflict(p, name, fd)) {
        addSuperImport(p, C)
        res.code = correct
      } else {
        res.code = symexist
      }
    }
  }

  private def scan(p: Project, f: xfs.FileDescriptor, namePar: pcNames.NAME, modePar: Mode): File = {
    var name = namePar
    var mode = modePar
    val checkNameRes = new CheckClassNameResult()

    var tags = Set32.empty
    if (mode == md_jbc) {
      checkClassName(p, f, name.asInstanceOf[pcNames.ClassName], checkNameRes)
      if (checkNameRes.code != correct) {
        if (checkNameRes.code == symexist) {
          // duplicated
          if (isFromExcludedPackage(name)) {
            return null
          }
          mode = md_sym
          tags = Set32.of(importnotscanned.toUByte)
        } else if (checkNameRes.code == badlettercase) {
          name = checkNameRes.correctName
        } else {
          return null
        }
      }
    }
    val mod = include(p, name, mode, tags)
    assert(mod.mode == mode)
    mod.fd = f
    if (mod.context == null) {
      mod.context = env.context
    }
    if (SetOfModes.of(md_jar, md_war, md_zip) contains mode) {
      env.config.setContext(mod.context)
      val packMode = env.getPackMode
      env.config.removeContext(mod.context)
      if (packMode == env.PM_ALL) {
        pcO.addCodeSource(f.getName)
      }
      if (packMode != env.PM_RESOURCES) {
        readClasspathEntry(p, f.getName, mod)
      }
    } else if (mode == md_dir) {
      readClasspathEntry(p, f.getName, mod)
    } else if (mode == md_jitcache) {
      env.errors.fault(ErrMsg636)
    } else if (mode == md_jbc) {
      val cpe = autoCompleteClasspathForClass(p, f, name)
      if (cpe != null) {
        mod.host = cpe
      } else {
        assert(inclasspathentryreading || rtjarcompiling)
      }
      if (cpe != null && mod.context == null && cpe.context != null) {
        // JET-2448 fix: inherit context options and equations
        // from classpath entry this class belongs
        mod.context = cpe.context
      }
    }
    mod
  }

  /*---------------------------------------------------------------*/
  def lookupFile(name: XString, ext: XString, lookInCurrentDir: Boolean): xfs.FileDescriptor = {
    /* RETURN NIL, if not exist */
    val fname = FS.addExt(name, ext)
    val f = xLookup.lookup(fname, lookInCurrentDir)
    if (f == null || !f.exists) {
      return null
    }
    f
  }

  def lookupSym(p: Project, oname: pcNames.NAME): xfs.FileDescriptor = lookupFile(oname.getMangledName, p.modeToExt(md_sym), lookInCurrentDir = false)

  def searchAnyJava(p: Project, name: pcNames.NAME): xfs.FileDescriptor = {
    assert(!pcNames.isLambdaClassName(name))
    lookupFile(FS.cutExt(name.name), p.jbcExt, !(pcO.isTomcat || pcO.isSpringBoot) && !curDirectoryAdded)
    // JET-4810: do not look in current dir for Tomcat, SpringBoot
    // and after project initialization
  }

  private def getNameByMode(name: XString, f: xfs.FileDescriptor, mode: Mode): pcNames.NAME = {
    mode match {
      case `md_obj` |
           `md_sym` |
           `md_jbc` =>
        pcNames.newClassName(name)
      case _ =>
        pcNames.newFileNameByFD(f)
    }
  }

  private def hasSpaces(s: XString): Boolean = {
    for (i <- 0 until s.length) {
      if (CharClass.isWhiteSpace(s.charAt(i))) {
        return true
      }
    }
    false
  }

  private def isFromProfilePDB(f: xfs.FileDescriptor): Boolean = {
    assert(f.isInstanceOf[xPDB.FileDescriptor])
    f.asInstanceOf[xPDB.FileDescriptor].place.pdb.isProfile
  }

  private def isFromMainPDB(f: xfs.FileDescriptor): Boolean = f.asInstanceOf[xPDB.FileDescriptor].place.pdb.isMain

  def initCompactProfilesPackages(): Unit = {
    compact1Packages = env.convValueToSet(Properties.getJCProperty("compact1Packages"))
    compact2Packages = env.convValueToSet(Properties.getJCProperty("compact2Packages"))
    compact3Packages = env.convValueToSet(Properties.getJCProperty("compact3Packages"))
    fulljrePackages = env.convValueToSet(Properties.getJCProperty("fullPackages"))
    fullJreIncludeTypes = env.convValueToSet(Properties.getJCProperty("fullIncludeTypes"))
  }

  def getCompactProfileForClass(classname: XString): Int = {
    if (fullJreIncludeTypes.contains(classname)) {
      return 4
    }

    var p = classname
    var lastslash = p.lastIndexOf('/')
    while (lastslash != -1) {
      p = p.substring(0, lastslash)
      if (fulljrePackages.contains(p)) {
        return 4
      } else if (compact3Packages.contains(p)) {
        return 3
      } else if (compact2Packages.contains(p)) {
        return 2
      } else if (compact1Packages.contains(p)) {
        return 1
      }
      lastslash = p.lastIndexOf('/')
    }
    // extension or locale class;
    0
  }

  private def addPackagesTo(packagesSetPar: mutable.HashSet[XString], packages: mutable.HashSet[XString]): mutable.HashSet[XString] = {
    var packagesSet = packagesSetPar
    if (packagesSet == null) packagesSet = new mutable.HashSet[XString]
    packagesSet ++= packages
    packagesSet
  }

  def addJavaSEExcludedPackages(packages: mutable.HashSet[XString]): Unit = {
    javaseExcludedPackages = addPackagesTo(javaseExcludedPackages, packages)
  }

  def addJavaSEIncludedPackages(packages: mutable.HashSet[XString]): Unit = {
    javaseIncludedPackages = addPackagesTo(javaseIncludedPackages, packages)
  }

  def isFromExcludedPackage(classname: pcNames.NAME): Boolean = {
    if (javaseExcludedPackages == null) {
      return false
    }
    if (javaseIncludedPackages == null) {
      javaseIncludedPackages = new mutable.HashSet[XString]
    }

    if (javaseExcludeTypes != null && javaseExcludeTypes.contains(classname.name)) {
      return true
    }

    val visitPackages = new ArrayBuffer[XString]
    var p = classname.name
    infiniteLoop {
      val lastslash = p.lastIndexOf('/')
      if (lastslash == -1) {
        javaseIncludedPackages ++= visitPackages
        return false
      }
      p = p.substring(0, lastslash)
      if (javaseIncludedPackages.contains(p)) {
        javaseIncludedPackages ++= visitPackages
        return false
      }
      if (javaseExcludedPackages.contains(p)) {
        javaseExcludedPackages ++ visitPackages
        return true
      }
      visitPackages += p
    }
  }

  def getHostName(fd: xfs.FileDescriptor): XString = fd match {
    case descriptor: xmZipModule.FileDescriptor => descriptor.zname
    case _ => fd.getName
  }

  private def extractNestedJar(file: xfs.FileDescriptor): xfs.FileDescriptor = {
    // extract to tmpres/<jarname>/<path in jar>
    val tempres = xPDB.getTempResourcesDir
    var jarName = FS.getBaseName(FS.HOST.fromPlatform(file.asInstanceOf[xmZip.FileDescriptor].zname))
    if (pcO.isSpringBoot) {
      // for Spring Boot we have the only jar, no need to put path
      // for it (as it can be absolute).
      jarName = FS.cutPath(jarName)
    }
    var placeDir = FS.addPath(tempres.getName, jarName) // placeDir = tmpres/<jarname>
    val placeName = FS.addPath(placeDir, file.asInstanceOf[xmZip.FileDescriptor].ename)

    placeDir = FS.getPath(placeName)

    assert(Dirs.mkdirs(placeDir))

    if (!xcModes.workerMode) {
      val out = xfs.raw.openToWrite(placeName)
      if (out == null) {
        env.errors.fault(xfs.MSG_FILE_CREATE_ERROR, xfs.raw.errmsg)
      }

      val in = file.openRawFile()
      out.writeFile(in)
      in.close()
      out.closeNew()
    } else {
      // JET-12855: nested jars should be already extracted by driver for workers
      assert(xfs.sys.exists(placeName))
    }

    xmZip.createFileDescriptor(placeName, JStrings.jstrDot)
  }

  private def checkAppDir(): Unit = {
    if (!appDirChecked) {
      appDirChecked = true
      pcO.isTomcat = false
      pcO.isIdea = false
      pcO.isSpringBoot = false
      pcO.isCustomClassloaders = false
      val apptype = env.config.equation("APPTYPE")
      if (apptype == null) {
        return
      } else if (apptype.equals2("TOMCAT")) {
        pcO.isTomcat = true
      } else if (apptype.equals2("IDEA")) {
        pcO.isIdea = true
      } else if (apptype.equals2("SPRINGBOOT")) {
        pcO.isSpringBoot = true
        env.config.tags += env.springboot
      } else {
        return
      }

      pcO.isCustomClassloaders = true

      // check or init appdir
      var appdir = env.config.equation("appdir")
      if (pcO.isSpringBoot) {
        val springBootArchive = env.config.equation("springbootarchive")
        if (springBootArchive == null) {
          env.errors.fault(ErrMsg652)
        }

        // always set appdir to the directory of the Spring Boot jar location
        // to allow import resolver to write entries relative to the jar
        // even if it is specified with full path
        appdir = FS.getPath(FS.HOST.fromPlatform(springBootArchive))
        if (appdir.isEmpty) {
          appdir = JStrings.jstrDot
        }
        env.config.setEquation2("appdir", appdir)

        // On the other hand keep semantic of outputdir the same as for plain applications.
        val outputdir = env.config.equation("outputdir")
        if (outputdir == null || outputdir.isEmpty) {
          env.config.setEquation2("outputdir", JStrings.jstrDot)
        }
      }
      if (appdir == null || appdir.isEmpty) {
        env.errors.fault(ErrMsg646)
      }
    }
  }

  /*----------------------------------------------------------------*/
  def chkFile(p: Project, name: pcNames.NAME, md: SetOfModes): Boolean = {
    var l = search(p.hashtable, name)
    while (l != null) {
      if (md contains l.asInstanceOf[File].mode) {
        return true
      }
      l = l.s
    }
    false
  }

  /*----------------------------------------------------------------*/
  private def invokeImportResolver(apptype: XString): Unit = {
    // importresolver exit codes, should match constants in com.huawei.excelsior.importresolvers.Main.java
    val EXIT_OK: Int = 0
    val cmdSb = ArrayBuffer.empty[String]

    cmdSb ++= Seq(
      s"-app-type=$apptype",
      s"-profile-dir=${env.getProfileDir}",
      s"-jre-version=${env.config.equation("jre_version")}",
    )

    var appdir = env.config.equation("appdir")
    assert(appdir != null)
    if (targetOS.isWindows) {
      if (appdir.charAt(appdir.length - 1) == '\\') {
        // JET-4503 fix: backslash is used for shielding symbols by shell
        // so the arguments  will come in wrong way if appdir ends with it
        appdir = appdir.substring(0, appdir.length - 1)
      }
    }
    cmdSb += s"-root-dir=$appdir"

    val outputname = env.config.equation("outputname")
    if (outputname != null) {
      cmdSb += s"-output-name=$outputname"
    }


    val pdb = env.config.equation("pdbname")
    cmdSb += s"-output-dir=$pdb"

    val jetHome = env.config.equation("jet_home")

    if (pcO.isTomcat) {
      val tomcatclasspath = O2Env.env.valueOfOrNull(TomcatClasspath)
      if (tomcatclasspath != null) {
        cmdSb += s"-tomcat-classpath=$tomcatclasspath"
      }
      val additionalclasspath = env.config.equation("additionalclasspath")
      if (additionalclasspath != null) {
        cmdSb += s"-additional-classpath=$additionalclasspath"
      }
    } else if (pcO.isIdea) {
    } else if (pcO.isSpringBoot) {
      val springBootArchive = env.config.equation("springbootarchive")
      assert(springBootArchive != null)
      cmdSb += s"-spring-boot-archive=$springBootArchive"
      if (O2Env.env.enabled(OldSpringBoot)) {
        cmdSb += "-old-spring-boot=true"
      }
    } else {
      throw new AssertionError
    }

    var importresolver = FS.makeFileName(jetHome, JStrings.newJString("bin/ImportResolver"))
    importresolver = FS.addExt2(importresolver, FS.HOST.exeLikeExtension)
    importresolver = FS.HOST.toPlatform(importresolver)

    if (isWorkMode) {
      env.info.print("\\nInvoking import resolver: %S %S\\n", importresolver, XString(cmdSb.mkString(" ")))
    }

    val exit = ProgExec.execute(importresolver.toString, cmdSb)
    if (exit != EXIT_OK) {
      sys.exit(exit)
    }
  }

  def modeToPDBType(mode: Mode): xPDB.ContentType = {
    import xPDB.ContentType as CT
    mode match {
      case `md_sym` => CT.SYM
      case `md_efs` => CT.EFS
      case `md_obj` => CT.OBJ
      case `md_lib` => CT.LIB
      case `md_fus` => CT.FUS
      case `md_res` => CT.RES
    }
  }

  private def tie(x: File, tailPar: Node): Node = {
    var tail = tailPar

    if (x.tags contains tied) {
      return tail
    }
    val n = new Node()
    n.mod = x
    n.next = null
    tail.next = n
    tail = n
    x.tags += tied.toUByte
    tail
  }
}
