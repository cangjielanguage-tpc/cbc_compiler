/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.RTConst
import com.huawei.excelsior.jet.compiler.o2lib.fe.pcOModule.ClassloaderIDGetter
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.o2.CharClassModule as CharClass
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.CPEntryModes
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.CPEntryModes.cpe_error
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, StringTokenizerModule as strtok, xcMakeModule as mk, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.BoolOption.HideConfiguration
import com.huawei.excelsior.jet.compiler.smart.ImportResolutionType.{ABSENT, EXTERNAL, NORMAL}
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

import scala.collection.mutable
import scala.util.chaining.scalaUtilChainingOps

object BundleImportResolverModule {

  type ImportType = Int
  val IMP_SYSTEM    = -2  // used for searching classes loaded by system and application classloaders
  val IMP_NONIMPORT = -1  // there is no such import at all in the class
  val IMP_BUNDLE    = 0   // one should search the class inside of current context
  val IMP_EXTERNAL  = 1   // one should search the class outside of current context
  val IMP_ABSENT    = 3   // the import should not be resolved

  class ImportResult private(val importType: ImportType, bcls: BundleClass) {
    assert(isBundle == (bcls != null))
    assert(!isBundle || className != null)

    def isBundle = importType == IMP_BUNDLE
    def className = { assert(isBundle); bcls.name }
    def classFile = { assert(isBundle); bcls.mod }
  }

  object ImportResult {
    def apply(importType: ImportType) = new ImportResult(importType, null)
    private[BundleImportResolverModule] def bundle(bcls: BundleClass) = new ImportResult(IMP_BUNDLE, bcls)
  }

  private class Bundle(val bid: XString, val bidnum: Int, val clid: Int, val stringCLID: XString) {
    val imports = mutable.LinkedHashMap.empty[XString, ImportEntry]
  }

  private class BundleClass(val name: pcNames.NAME, val bundle: Bundle) {
    var mod: mk.File = _

    private[BundleImportResolverModule] def addImport(classname: XString, type0: Int, bidnum: Int): Unit = {
      val imp = new ImportEntry(type0, classname, bidnum)

      bundle.imports.get(classname) match {
        case None =>
          bundle.imports(classname) = imp
        case Some(prev) =>
          if (!(imp.type0 == prev.type0 && imp.bidnum == prev.bidnum)) {
            // JET-10177: import resolver could decide to stub some import that can
            // lead to classloader constraints violation for one class,
            // while another class of the same jar can safely resolve the same import.
            // To satisfy smart mode constraint that all classes of one module (jar)
            // see a class reference the same, let's conservatively stub import
            // of classes that can potentially lead to class loader constraints violation
            // for any class of the jar.
            assert(imp.type0 == IMP_ABSENT && prev.type0 == IMP_BUNDLE || imp.type0 == IMP_BUNDLE && prev.type0 == IMP_ABSENT)
            if (prev.type0 == IMP_BUNDLE) {
              // keep only absent type in the imports
              assert(imp.type0 == IMP_ABSENT)
              bundle.imports(classname) = imp
          }
        }
      }
    }
  }

  private class ImportEntry(var type0: ImportType, val name: XString, val bidnum: Int) {
    var clazz: BundleClass = _
  }


  class BundleDescriptorReader(in: xfs.TextFile) {
    private var line: XString = _
    private var pos: Int = _

    def read(p: mk.Project): Unit = {
      // Read parameters block:
      while (nextLine() && line.charAt(0) == '-') {
        env.config.parse(line)
        assert(env.config.res != env.wrongSyntax)
      }

      val packeq = js.newJString("-pack")

      var lastClassloaderID = ClassloaderIDGetter.LAST_STD_CLID
      var hasMore = true
      while (hasMore) {
        val word = readWord(intern = false)
        assert(word.equals2("!classloaderentry"))

        val clType = readWord(intern = false)
        val entryType = CPEntryModes.toCpeMode(clType)
        assert(entryType != cpe_error)

        val bid = readWord()
        val bidnum = readInt()

        bidnums.put(bidnum, bid) ensuring (_.isEmpty)

        val classloaderIDAsString = readWord()

        val clid = classloadersIDTable.getOrElseUpdate(classloaderIDAsString, {
          lastClassloaderID += 1
          lastClassloaderID
        })

        val bundle = new Bundle(bid, bidnum, clid, classloaderIDAsString)
        bundles(bid) = bundle

        val bundleFile = createBundleFile(p, bid, entryType)
        assert(bundleFile != null) // import resolver should always add only existing entries

        env.info.print("Reading %S classloader entry %S ...\\n", clType, bid)

        val libapp = readWord(intern = false)
        val app = libapp.equals2("app")

        val packmode = readWord()
        assert(packmode.startsWith(packeq, 0))

        var readMoreClasses = true
        while (readMoreClasses && nextLine()) {
          val classname = readWord()
          if (classname.equals2("!end")) {
            readMoreClasses = false

          } else {
            val bcname = pcNames.newBundleClassName(classname, classloaderIDAsString)
            val clazz = new BundleClass(bcname, bundle)

            // JET-8731: respect excludelist compiler equation to be able
            //           to exclude classes from compilation that are dynamically
            //           substituted by frameworks like Javassist
            var excluded = mk.isExcluded(classname)

            if (!excluded) {
              clazz.addImport(classname, IMP_BUNDLE, bidnum) // add self to import
              var hasMoreImport = true
              while (hasMoreImport) {
                val impClassname = readWord()
                if (impClassname == null) {
                  hasMoreImport = false
                } else if (impClassname.equals2("@excluded")) {
                  // import resolver decided to exclude this class from compilation
                  // all references to this class should be ignored and class should be treated as absent
                  excluded = true
                  hasMoreImport = false
                } else {
                  val impType = readInt()
                  val impBIDNum = if (impType == IMP_BUNDLE) readInt() else -1
                  clazz.addImport(impClassname, impType, impBIDNum)
                }
              }
            }

            val map = if (excluded) excludedClasses else classes
            val old = map.put(clazz.name, clazz)
            assert(old.isEmpty)
          }
        }

        setDefaultProtectOptimizePack(bundleFile, app, packmode.substring(6))
        env.config.setContext(bundleFile.context)
        val optimize = env.config.equation("optimize")
        val protect = env.config.equation("protect")
        if (optimize.equals2("ALL") || protect.equals2("ALL")) {
          bundleFile.tags += mk.compileall.toUByte
        }
        env.config.removeContext(bundleFile.context)

        hasMore = nextLine()
      }

      in.close()

      addBootstrapJars(p)

      if (pcO.isTomcat && O2Env.env.enabled(HideConfiguration)) {
        addConfiguration(p)
      }

      checkUserDefBundles(p)

      resolveImports(p)

      // add icon
      val iconEq = env.config.equation("icon")
      if (iconEq != null && !p.hasIcon) {
        assert(p.appendFile(iconEq) != null)
      }

      setDefaultOptionsAndEquations()
    }

    def readInt(): Int = {
      val n = readWord(intern = false)
      js.parseIntOrElse(n, -1)
    }

    def readWord(intern: Boolean = true): XString = {
      if (!skip()) {
        return null
      }
      val len = line.length
      val i = pos
      if (line.charAt(pos) == '\"') {
        pos += 1
        while (pos < len && line.charAt(pos) != '\"') {
          pos += 1
        }
        pos += 1
        if (pos == i + 2) {
          return null
        }
        if (intern) {
          js.internSubstring(line, i + 1, pos - 1)
        } else {
          line.substring(i + 1, pos - 1)
        }
      } else {
        while (pos < len && !CharClass.isWhiteSpace(line.charAt(pos))) {
          pos += 1
        }
        if (i == pos) {
          return null
        }
        if (intern) {
          js.internSubstring(line, i, pos)
        } else {
          line.substring(i, pos)
        }
      }
    }

    def nextLine(): Boolean = {
      line = in.readLine()
      pos = 0

      line != null && (skip() || nextLine())
    }

    def skip(): Boolean = {
      val len = line.length
      while (pos < len && CharClass.isWhiteSpace(line.charAt(pos))) {
        pos += 1
      }
      pos < len
    }
  }

  private var bundleDescriptorLoaded = false
  private val classes = mutable.LinkedHashMap.empty[pcNames.NAME, BundleClass]
  private val excludedClasses = mutable.LinkedHashMap.empty[pcNames.NAME, BundleClass]
  private val classloadersIDTable = mutable.LinkedHashMap.empty[XString, Int]
  private val bidnums = mutable.HashMap.empty[Int, XString]
  private val bundles = mutable.LinkedHashMap.empty[/*bid:*/ XString, Bundle]

  def getClassloaderStringIDbyBID(bid: XString): XString = {
    assert(bundleDescriptorLoaded)
    bundles(bid).stringCLID
  }

  private def getBundleClass(name: pcNames.NAME): BundleClass = {
    assert(bundleDescriptorLoaded)
    classes.get(name).orNull
  }

  private def getBundleFileForClass(p: mk.Project, clazz: BundleClass): mk.File = {
    val bname = pcNames.newBundleName(clazz.bundle.bid)
    p.getProjectFile(bname, mk.SetOfModes.of(mk.md_bundle))
  }

  private def appendClass(p: mk.Project, clazz: BundleClass): mk.File = {
    val bundleFile = getBundleFileForClass(p, clazz)

    if (bundleFile != null) { // bundle = NIL, if it is removed from the compilation
      val fd = bundleFile.fd.getEntry(clazz.name.name, js.newJString("class"))
      assert(fd.exists)

      clazz.mod = p.appendBundleClass(bundleFile, clazz.name, fd)
      return clazz.mod
    }
    null
  }

  def appendBundleClass(p: mk.Project, classname: pcNames.NAME): mk.File = {
    if (bundleDescriptorLoaded) {
      classes.get(classname).map(c => appendClass(p, c)).orNull
    }
    null
  }

  private def resolveImports(p: mk.Project): Unit = {
    env.info.print("Resolving import dependencies ...\\n")
    for (bundle <- bundles.values; entry <- bundle.imports.values if entry.type0 == IMP_BUNDLE) {
      val bid = bidnums(entry.bidnum)
      val classname = pcNames.newBundleClassName(entry.name, bundles(bid).stringCLID)
      entry.clazz = classes.get(classname).orNull
      if (entry.clazz == null) {
        assert(excludedClasses contains classname)
        entry.type0 = IMP_ABSENT
      }
    }

    classes.values foreach { appendClass(p, _) }
  }

  private def createBundleFile(p: mk.Project, bid: XString, entryType: CPEntryModes.CPEntryMode): mk.File = {
    var bundle = p.getProjectFile(pcNames.newBundleName(bid), mk.SetOfModes.of(mk.md_bundle))
    if (bundle == null) {
      env.config.push()
      bundle = p.appendClassloaderEntry(bid, entryType, bidInInternalForm = true)
      env.config.pop()
    }
    if (bundle != null) {
      bundle.tags += mk.autodef.toUByte
    }
    bundle
  }

  private def setDefaultProtectOptimizePack(bundle: mk.File, app: Boolean, packval: XString): Unit = {
    env.config.setContext(bundle.context)
    val optimize = env.config.equation("optimize")
    val protect = env.config.equation("protect")
    val pack = env.config.equation("pack")
    env.config.removeContext(bundle.context)

    if (optimize == null) {
      if (!app) {
        bundle.context.setEquation("optimize", "autodetect")
      } else {
        bundle.context.setEquation("optimize", "all")
      }
    }
    if (protect == null) {
      if (!app) {
        bundle.context.setEquation("protect", "nomatter")
      } else {
        bundle.context.setEquation("protect", "all")
      }
    }
    if (pack == null) {
      bundle.context.setEquation2("pack", packval)
    }
  }

  /*
     Scans bootstrapjars equation that is come from ImportResolver
     and adds all jars from the equation to the classpath of the application.
     If the same bundleentry is specified in the project
     (by !bundleentry directive), we copy context equations and options
     from the bundleentry to the just created classpathentry.
     If a jar is is specified in absolute form (such as tools.jar) then
     it is packed into exe (we ignore another -pack setting for it)
  */
  private def addBootstrapJars(p: mk.Project): Unit = {
    var bundle: mk.File = null
    var absolute: Boolean = false

    val bootstrapJarList = env.config.equation("bootstrapJars")
    assert(bootstrapJarList != null)

    val st = strtok.newStringTokenizer(bootstrapJarList, ";")
    while (st.hasMoreTokens) {
      val jar = st.nextToken()

      val appdir = env.config.equation("APPDIR")
      assert(appdir != null)
      var path = FS.addPath(appdir, jar)

      var toolsJar = false
      if (!xfs.sys.exists(path)) {
        path = jar
        bundle = null // the jar is outside from appdir,
                      // there is no bundleentry for it
        absolute = true
        if (path.endsWith(js.newJString("tools.jar"))) {
          // tools.jar contain multiple duplicated with rt.jar classes
          // disable according message for this because they do not bring
          // any useful information
          env.config.setOption("SUPRESSCLASSDUPLICATIONMSG", value = true)
          toolsJar = true
        }
      } else {
        // look for the same bundleentry declaration
        bundle = p.getProjectFile(pcNames.newBundleName(FS.HOST.fromPlatform(jar)), mk.SetOfModes.of(mk.md_bundle))
        absolute = false
        if (path.endsWith(js.newJString("jmx.jar"))) {
          // jmx.jar also contain multiple duplicated classes as well as tools.jar
          env.config.setOption("SUPRESSCLASSDUPLICATIONMSG", value = true)
        }
      }

      env.config.push() // create context for the classpath entry
      val cpentry = p.appendClasspathEntry(path) // create classpathentry
      env.config.pop() // remove context
      assert(cpentry != null)
      if (pcO.isSpringBoot && path.equals(env.config.equation("SPRINGBOOTARCHIVE"))) {
        cpentry.tags += mk.springbootarchive.toUByte
      }

      if (bundle != null) {
        bundle.tags += mk.autodef.toUByte
        bundle.tags += mk.bootstrapjar.toUByte
        setDefaultProtectOptimizePack(bundle, app = false, js.newJString("NONCOMPILED"))
        // copy context from the bundle to the classpathentry
        // TODO: now jetcp does not allow to set any  settings for bootstrap jars,
        //       so if we will not support it in the future, this should be removed
        cpentry.context = bundle.context
      }
      if (absolute || pcO.isTomcat) {
        // jar's with absolute name and Tomcat jars should always be packed
        // else they will not be found by JetPackII
        cpentry.context.setEquation("pack", "noncompiled")
        if (toolsJar) {
          cpentry.context.setEquation("optimize", "autodetect")
        }
      }
    }

    // JET-4335: append main to the project
    val main = env.config.equation("main")
    assert(main != null && main.length != 0) // one of bootstrap jars must have MAIN-CLASS manifest attribute
    assert(p.appendFile(main) != null)
  }

  private def addConfiguration(p: mk.Project): Unit = {
    // Add pseudo-bundle for packing purposes only.
    // Note, that conf/ directory must exist, we read conf/catalina.properties
    // in importresolver from there at least
    env.config.push()
    val bundle = p.appendClassloaderEntry(js.newJString("conf"), CPEntryModes.cpe_tomcat, bidInInternalForm = true)
    env.config.pop()

    bundle.context.setEquation("pack", "all")
  }

  private def checkUserDefBundles(p: mk.Project): Unit = {
    var m = p.list
    while (m != null) {
      if (m.mode == mk.md_bundle && (m.tags contains mk.userdef) && !(m.tags contains mk.autodef)) {
        // we found user defined bundle that is not detected by importresolver
        env.errors.fault(ErrMsg477, m.name.name)
      }
      m = m.next
    }
  }

  private def setDefaultOptionsAndEquations(): Unit = {
    var clidProvider: XString = null

    val apptype = env.config.equation("APPTYPE")
    if (apptype.equals2("TOMCAT")) {
      clidProvider = js.newJString("com/huawei/excelsior/jet/runtime/classload/customclassloaders/tomcat/TomcatCLIDProvider")
    } else if (apptype.equals2("IDEA")) {
      clidProvider = js.newJString("com/huawei/excelsior/jet/runtime/classload/customclassloaders/idea/IdeaCLIDProvider")
    } else if (apptype.equals2("SPRINGBOOT")) {
      clidProvider = js.newJString("com/huawei/excelsior/jet/runtime/classload/customclassloaders/springboot/SpringBootCLIDProvider")
    } else {
      throw new AssertionError
    }

    var jetvmprop = env.config.equation("jetvmprop")
    if (jetvmprop == null || jetvmprop.equals(js.jstrEmpty)) {
      env.config.setEquation2("jetvmprop", js.format("-Djet.stack.trace -Djet.classloader.id.provider=%S", clidProvider))
    } else if (jetvmprop.indexOf(js.newJString("-Djet.classloader.id.provider")) < 0) {
      // if jetvmprop is set then all above properties should be set also
      // either by JET Control Panel or by user manually,  except classloaderidprovider
      env.config.setEquation2("jetvmprop", js.format("%S -Djet.classloader.id.provider=%S", jetvmprop, clidProvider))
    }

    jetvmprop = env.config.equation("jetvmprop")
    if (apptype.equals2("IDEA")) {
      env.config.setEquation2("jetvmprop", js.format("%S -Duser.dir=*{exe.dir} -Djet.cd.to.user.dir", jetvmprop))
      // enable eager activation since semistubs are disabled
    } else if (apptype.equals2("TOMCAT")) {
      env.config.setEquation2("jetvmprop", js.format("-Djava.io.tmpdir=../temp %S -Duser.dir=*{exe.dir} -Djet.cd.to.user.dir -Dcatalina.home=.. -Dcatalina.base=..", jetvmprop))

    } else if (apptype.equals2("SPRINGBOOT")) {
      env.config.setEquation2("jetvmprop", js.format("%S -Djet.jit.always.interpret=true", jetvmprop))
      // enable eager activation since semistubs are disabled
      // JET-11913: remove -Djet.jit.always.interpret=true when the issue is resolved
    }

    val inlinetolimit = env.config.equation("inlinetolimit")
    if (inlinetolimit == null || inlinetolimit.equals(js.jstrEmpty)) {
      env.config.setEquation("inlinetolimit", "500")
    }
  }

  def readBundleDescriptor(fd: xfs.FileDescriptor, prj: mk.Project): Unit = {
    assert(!bundleDescriptorLoaded)
    new BundleDescriptorReader(fd.openTextFile()).read(prj)
    bundleDescriptorLoaded = true
  }

  def resolveImportFor(p: mk.Project, classname: pcNames.NAME, importStr: XString): ImportResult = {
    assert(bundleDescriptorLoaded)
    val class0 = classes(classname)
    val name = classname.name
    if (name == importStr) {
      assert(class0.mod != null)
      assert(class0.name == classname)
      return ImportResult.bundle(class0)
    }

    class0.bundle.imports.get(importStr) match {
      case None =>
        ImportResult(IMP_NONIMPORT)
      case Some(i) if i.type0 != IMP_BUNDLE =>
        ImportResult(i.type0)
      case Some(i) =>
        if (i.clazz.mod == null && getBundleFileForClass(p, i.clazz) == null) {
          // bundle not loaded
          ImportResult(IMP_ABSENT)
        } else {
          // i.clazz.mod could be `null` here too, if it was not already
          // added to the project (-optimize=autodetect)
          ImportResult.bundle(i.clazz)
        }
    }
  }

  def resolveImportForBundle(p: mk.Project, bid: XString, importStr: XString): mk.ImportResult = {
    assert(bundleDescriptorLoaded)
    bundles.get(bid) flatMap (_.imports.get(importStr)) match {
      case None =>
        // Consider the following case: a module had a class that was not imported by other classes of the module,
        // and new module appears that overrides the class,
        // but it is still not imported, so now we unable to find it within imported
        // classes of the module. Return absent to recompile the module.
        mk.ImportResult(ABSENT)

      case Some(i) => i.type0 match {
        case IMP_ABSENT => mk.ImportResult(ABSENT)
        case IMP_EXTERNAL => mk.ImportResult(EXTERNAL)
        case IMP_BUNDLE =>
          if (i.clazz.mod == null) {
            // has not been added to the project yet (-optimize=autodetect)
            val bundleFile = getBundleFileForClass(p, i.clazz)
            assert(bundleFile != null)
            mk.ImportResult(NORMAL, bundleFile)
          } else {
            assert(i.clazz.mod.host != null)
            mk.ImportResult(NORMAL, i.clazz.mod.host)
          }
      }
    }
  }

  def getSourceClass(p: mk.Project, name: pcNames.NAME): xfs.FileDescriptor = {
    assert(name != null)
    val clazz = getBundleClass(name)

    if (clazz == null) { // no class -> no source
      return null
    }
    if (clazz.mod != null) {  // have class and source already found
      return clazz.mod.fd
    }
    // source not yet found. let's found it
    val bundleFile = getBundleFileForClass(p, clazz)

    if (bundleFile != null) {
      val fd = bundleFile.fd.getEntry(clazz.name.name, js.newJString("class"))
      assert(fd.exists)
      return fd
    }
    null
  }

  def getClassloaderIDByName(name: pcNames.NAME): Int = {
    assert(name != null)
    getBundleClass(name).bundle.clid
  }

  def getClassloaderID(bid: XString): Int = {
    assert(bundleDescriptorLoaded)
    bundles(bid).clid
  }

  def writeClassloadersIDTable(out: xfs.TextFile): Unit = {
    for ((id, num) <- classloadersIDTable) {
      out.print("%S = %d\\n", id, num)
    }
  }
}
