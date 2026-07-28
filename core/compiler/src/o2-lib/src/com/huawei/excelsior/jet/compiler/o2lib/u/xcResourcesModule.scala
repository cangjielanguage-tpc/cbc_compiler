/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

import com.huawei.excelsior.common.Language.JAVA
import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.Env.*
import com.huawei.excelsior.jet.compiler.o2lib.fe.{pc, ObjNamesModule as ObjNames, pcNamesModule as pcNames, pcOModule as pcO}
import com.huawei.excelsior.jet.compiler.o2lib.fe_jbc.jbcFrontModule as jbcFront
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.projectsystem.CPEntryModes
import com.huawei.excelsior.jet.compiler.o2lib.tools.NamesCommon.*
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule as xPDB
import com.huawei.excelsior.jet.compiler.o2lib.u.{BundleImportResolverModule as BundleImportResolver, DirsModule as Dirs, JStringsModule as js, ManifestModule as Manifest, StringTokenizerModule as strtok, TextFileModule as tf, xRamFileModule as xRamFile, xcCompModule as xcComp, xcMakeModule as mk, xcModesModule as xcModes, xiEnvModule as env, xiFilesModule as xfs, xmZipModule as xmZip}
import com.huawei.excelsior.jet.compiler.o2lib.xjRTSModule as xjRTS
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.JZip.ZipFileModule as ZipFile
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.{FSModule as FS, PortableProgExecModule as PortableProgExec}
import com.huawei.excelsior.jet.compiler.options.BoolOption.{GenMegaObj, GenTomcatScripts, SplashGetFromManifest}
import com.huawei.excelsior.jet.compiler.xminizip.Minizip
import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.io.Path
import xscala.util.{UByte, UShort}

import java.io.IOException
import scala.collection.*
import scala.collection.mutable.ArrayBuffer
import scala.util.Using

/*
  Preparation of resources.
*/
object xcResourcesModule {

  class RawData {

    private[xcResourcesModule] var data: Array[Byte] = _
    private[xcResourcesModule] var len: Int = _

  }

  class EFSNode extends Object {

    private[xcResourcesModule] var name: XString = _
    private[xcResourcesModule] var hostname: XString = _
    private[xcResourcesModule] var visible: Boolean = _
    private[xcResourcesModule] var extra: RawData = _
    private[xcResourcesModule] var entries: Hashtable = _ // <js.JString, EFSNode>

    def writeEntriesTo(out: xfs.TextFile, indent: Int): Unit = {
      if (this.isDir) {
        indentLine(out, indent)
        out.print("%d\\n", this.entries.size)

        var it = this.entries.values
        while (it.hasNext) {
          val child = it.next().asInstanceOf[EFSNode]
          if (child.isDir) {
            child.writeTo(out, indent + 1)
          }
        }

        it = this.entries.values
        while (it.hasNext) {
          val child = it.next().asInstanceOf[EFSNode]
          if (!child.isDir) {
            child.writeTo(out, indent + 1)
          }
        }
      }
    }

    def writeTo(out: xfs.TextFile, indent: Int): Unit = {
      indentLine(out, indent)

      if (this.isDir) {
        out.print("DIR ")
      } else {
        out.print("FILE ")
      }

      out.print("\"%S\" \"%S\"", this.name, this.hostname)

      if (!this.visible) {
        out.print(" HIDDEN")
      }

      if (this.extra != null) {
        out.print(" EXTRA %x", this.extra.len)
        for (i <- 0 until this.extra.len) {
          // printing out EXTRA info as bytes (see JET-14497)
          out.print(" %x", this.extra.data(i))
        }
      }

      out.print("\\n")
      this.writeEntriesTo(out, indent)
    }

    def addPath(namePar: XString, hostname: XString, source: XString, visible: Boolean, isdir: Boolean, extra: RawData): EFSNode = {
      var name = namePar
      var parent: EFSNode = null

      if (name.charAt(name.length - 1) == '/') {
        // JET-4872: strip last slash,
        // else getPath will return empty string
        name = name.substring(0, name.length - 1)
      }

      val nameDir = FS.getPath(name)

      if (nameDir.nonEmpty) {
        var hostnameDir = FS.normalizeFileName(FS.HOST.fromPlatform(source))
        hostnameDir = FS.getPath(hostnameDir)
        assert(hostnameDir != null)
        if (hostnameDir.isEmpty) {
          // JET-6504: current dir -- use absolute path for hostnameDir
          hostnameDir = FS.HOST.fullPath(js.jstrDot)
        }

        hostnameDir = FS.HOST.toPlatform(hostnameDir)

        parent = this.addPath(nameDir, hostnameDir, hostnameDir, visible, isdir = true, null)

        name = FS.cutPath(name)
      } else {
        parent = this
      }

      assert(name.indexOf('/') < 0)
      var node = parent.getEntry(name)
      if (node == null) {
        node = newEFSNode(name, hostname, visible, isdir, extra)
        parent.addEntry(node)
      }

      node
    }

    def pathExists(name: XString): Boolean = this.getExistingPath(name) != null

    def getExistingPath(namePar: XString): EFSNode = {
      var name = namePar
      var parent: EFSNode = null

      val nameDir = FS.getPath(name)

      if (nameDir.nonEmpty) {
        parent = this.getExistingPath(nameDir)

        if (parent == null) {
          return null
        }

        name = FS.cutPath(name)
      } else {
        parent = this
      }

      parent.getEntry(name)
    }

    def getEntry(name: XString): EFSNode = {
      assert(this.isDir)
      this.entries.get(name).asInstanceOf[EFSNode]
    }

    def addEntry(e: EFSNode): Unit = {
      assert(this.isDir)
      assert(this.entries.put(e.name, e) == null)
    }

    def empty(): Boolean = {
      assert(this.isDir)
      this.entries.size == 0
    }

    def isDir: Boolean = this.entries != null

    def init(name: XString, hostname: XString, visible: Boolean, isdir: Boolean, extra: RawData): Unit = {
      this.name = name
      this.hostname = hostname
      this.visible = visible
      this.extra = extra

      if (isdir) {
        this.entries = new Hashtable()
      }
    }

  }


  class Mountpoint extends EFSNode {

    def writeMountpointTo(out: xfs.TextFile): Unit = {
      out.print("\"%S\"\\n", this.name)
      this.writeEntriesTo(out, 0)
    }

    /*
      Adds resource to mountpoint.
      Parameters:
        resPath:  relative path of the resource assotiated in EFS for the executable
        fromPath: path to processed resource (located in tmpres) that will be
                  actually packed
        source:   path to original resource (not processed)
    */
    def addResource(resPath: XString, fromPath: XString, source: XString): Unit = {
      assert(this.addPath(resPath, fromPath, source, env.config.option("visibleresource"), isdir = false, null) != null)
    }

  }


  private class EFS {
    private val mountpoints = new ArrayBuffer[Mountpoint]

    def isEmpty: Boolean = mountpoints forall {_.empty()}

    def writeTo(out: xfs.TextFile): Unit = {
      val num = mountpoints count {!_.empty()}
      if (num != 0) {
        out.print("%d\\n", num)
        mountpoints foreach { m => if (!m.empty()) m.writeMountpointTo(out) }
      }
    }

    def newMountpoint(name: XString): Mountpoint = {
      val mountpoint = new Mountpoint()
      mountpoint.init(name, js.jstrDot, visible = true, isdir = true, null)
      this.mountpoints += mountpoint
      mountpoint
    }

  }


  class EFSDataWriter {
    def close(): Unit = throw new AssertionError
    def writeEntry(name: XString, source: xfs.FileDescriptor, data: RawData, extra: RawData): Unit = throw new AssertionError
  }


  private class JarEFSDataWriter(val name: String) extends EFSDataWriter {

    private[xcResourcesModule] var zw: Minizip.Writer = _

    override def close(): Unit = this.zw.close()

    override def writeEntry(name: XString, source: xfs.FileDescriptor, data: RawData, extra: RawData): Unit = {
      var buf: Array[Byte] = null
      var len: Int = 0
      var extraData: Array[Byte] = null
      var extraLen: Int = 0

      if (data == null) {
        if (source.isDirectory) {
          len = 0
          buf = new Array[Byte](len)
        } else {
          buf = source.getFileContents
          assert(buf != null)
          len = buf.length
        }
      } else {
        buf = data.data.slice(0, data.len)
        len = data.len
      }

      val time = source.modifyTime()

      if (extra == null) {
        extraLen = 0
        extraData = null
      } else {
        extraLen = extra.len
        extraData = extra.data
      }

      try {
        this.zw.putBytesAndExtraToArchive(buf, extraData, Path.rel(name.utf8ToString), time, source.isDirectory)
      } catch {
        case _: IOException => env.errors.fault(xPDB.MSG_CANNOT_WRITE_ZIPENTRY, name, this.name)
      }
    }

  }


  private class DirEFSDataWriter extends EFSDataWriter {

    private[xcResourcesModule] var dir: xfs.FileDescriptor = _
    private[xcResourcesModule] var dirNode: EFSNode = _
    private[xcResourcesModule] var visible: Boolean = _

    override def close(): Unit = {
    }

    override def writeEntry(name: XString, source: xfs.FileDescriptor, data: RawData, extra: RawData): Unit = {
      var toFD: xfs.FileDescriptor = null

      val sourceHostName = FS.HOST.toPlatform(source.getName)
      if (source.isDirectory) {
        assert(this.dirNode.addPath(name, sourceHostName, sourceHostName, this.visible, isdir = true, extra) != null)
        return
      }
      if (data != null) {
        toFD = handleLongName(this.dir.getEntry(name, js.jstrEmpty))
        Dirs.mkdirs(FS.getPath(toFD.getName))

        val file = xfs.raw.openToWrite(toFD.getName)
        if (file == null) {
          env.errors.fault(xfs.MSG_FILE_CREATE_ERROR, xfs.raw.errmsg)
        }

        file.writeBlock(data.data, 0, data.len) // TODO: do not copy nested jars here
        file.closeNew()
      } else {
        toFD = source
      }

      val toFDHostName = FS.HOST.toPlatform(toFD.getName)

      assert(this.dirNode.addPath(name, toFDHostName, sourceHostName, this.visible, isdir = false, extra) != null)
    }

    /* constructor */
    def init(dir: XString, dirNode: EFSNode, visible: Boolean): Unit = {
      this.dir = xfs.sys.createFileDescriptor(dir)
      this.dirNode = dirNode
      this.visible = visible
    }

  }


  private class FromJarToDirEFSDataWriter extends DirEFSDataWriter {

    override def writeEntry(namePar: XString, source: xfs.FileDescriptor, dataPar: RawData, extra: RawData): Unit = {
      var name = namePar
      var data = dataPar

      if (source.isDirectory) {
        if (name.charAt(name.length - 1) == '/') {
          // JET-4852: in zip, directory entries ends with "/"
          name = name.substring(0, name.length - 1)
        }
        val toFD = handleLongName(this.dir.getEntry(name, js.jstrEmpty))
        Dirs.mkdirs(toFD.getName)
        val toFDHostName = FS.HOST.toPlatform(toFD.getName)
        assert(this.dirNode.addPath(name, toFDHostName, toFDHostName, this.visible, isdir = true, extra) != null)
        return
      }
      if (data == null) {
        // read file from jar
        val buf = source.getFileContents
        data = newRawData(buf, buf.length)
      }
      super.writeEntry(name, this.dir.getEntry(name, js.jstrEmpty), data, extra)
    }

  }


  private class ManifestFilter extends Manifest.ManifestFilter {
    override def filterAttribute(name: Manifest.Name, value: XString): Boolean = name.equals(Manifest.SHA1_DIGEST) || name.equals(Manifest.SHA_DIGEST) || name.equals(Manifest.MD5_DIGEST)
  }

  //---------------------------------------------------------------------------

  private type PathBuffer = mutable.LinkedHashSet[XString]

  private def pathBufferToString(buffer: PathBuffer): XString = {
    if (buffer.isEmpty) {
      return js.jstrEmpty
    }

    val str = new js.StringBuffer()
    for (path <- buffer) {
      if (str.length > 0) {
        str.appendChar(FS.TARGET.pathEnvVarSeparator)
      }
      str.appendString(path)
    }
    str.toJString
  }

  private def newPathBufferByPath(path: XString): PathBuffer = {
    val buffer = new PathBuffer

    def unquoteAndAppend(pathElement: XString): Unit = {
      var x = pathElement
      if (x.isEmpty) return
      if (x.length >= 2 && x.charAt(0) == '\"' && x.charAt(x.length - 1) == '\"') {
        x = x.substring(1, x.length - 1)
      }
      buffer += x
    }

    var start = 0
    var end = path.indexOf(FS.TARGET.pathEnvVarSeparator)
    while (end >= 0) {
      unquoteAndAppend(path.substring(start, end))
      start = end + 1
      end = path.indexOf(FS.TARGET.pathEnvVarSeparator, start)
    }
    unquoteAndAppend(path.substring(start))

    buffer
  }

  //---------------------------------------------------------------------------

  class ClassPathManager {

    private[xcResourcesModule] var classpath: PathBuffer = _
    private[xcResourcesModule] var standalone: PathBuffer = _

    def updateClasspathWithStandalone(p: mk.Project, m: mk.File): Unit = {
      if (xPDB.isProfileBuild) {
        // JET-9950: there should be no standalone resources for XKRN.
        return
      }

      var rn = getRelativeResourceName(p, m, nullOnFail = true)
      val relative = rn != null

      if (!relative) {
        rn = getFullResourceName(p, m)
      } else {
        rn = transformRelativePath(rn)
        rn = FS.addPath(js.newJString("*{comp.dir}"), rn)
      }

      rn = FS.TARGET.toPlatform(rn)
      this.classpath += rn
      this.standalone += rn
    }

    def updateClasspath(rnPar: XString): Unit = {
      var rn = rnPar

      rn = transformRelativePath(rn)

      // Packed classpath elements are mounted relatively to exe directory
      rn = FS.addPath(js.newJString("*{exe.dir}"), rn)
      rn = FS.TARGET.toPlatform(rn)
      this.classpath += rn
    }

    def constructClassPath(): Unit = {
      val componentClasspathVal = env.config.equation("componentclasspath")
      if (componentClasspathVal != null && componentClasspathVal.nonEmpty) {
        val componentClasspath = newPathBufferByPath(componentClasspathVal)

        val detectedClasspath = this.classpath
        val detectedStandalone = this.standalone

        this.classpath = new PathBuffer
        this.standalone = new PathBuffer

        // prepend component classpath to the classpath auto-detected by compiler.
        this.classpath ++= componentClasspath
        this.classpath ++= detectedClasspath

        // Add component classpath elements into standalone resources
        // if they are not in the detected classpath.
        this.standalone ++= (componentClasspath diff detectedClasspath)
        this.standalone ++= detectedStandalone
      }

      if (this.classpath.nonEmpty) {
        env.config.setEquation2("componentclasspath", pathBufferToString(this.classpath))

        if (this.standalone.nonEmpty) {
          env.config.setEquation2("StandAloneResources", pathBufferToString(this.standalone))
        }
      }
    }

    def init(): Unit = {
      this.classpath = new PathBuffer
      this.standalone = new PathBuffer
    }
  }

  //-------------------- ResourceProcessor ------------------------

  class ResourceProcessor {

    private[xcResourcesModule] var p: mk.Project = _
    private[xcResourcesModule] var createefs: Boolean = _
    private[xcResourcesModule] var compiledclassesset: Hashtable = _
         // hashtable of nested dirs for all bundles:
    // the key is bundle name, value is list of bids
    // of nested classpathentries that are directories
    private[xcResourcesModule] var allNestedDirs: Hashtable = _ // <JString, Vectors.Vector<JString<BID>>>
    private[xcResourcesModule] var filterall: Boolean = _
    private[xcResourcesModule] var filterresources: Boolean = _
    private[xcResourcesModule] var nestedJars: Hashtable = _ // <JString, Hashtable.Hashtable>
    private[xcResourcesModule] var pendingBundles: mutable.HashSet[XString] = _
    private[xcResourcesModule] var mountpoint: Mountpoint = _
    private[xcResourcesModule] var packedBundles: js.StringBuffer = new js.StringBuffer()
    private[xcResourcesModule] var hidedClassesBundles: js.StringBuffer = new js.StringBuffer()
    private[xcResourcesModule] var classPathManager: ClassPathManager = _
    private[xcResourcesModule] var data: RawData = _ // data for current processed entry
    private[xcResourcesModule] var extra: RawData = _  // extra data for current processed entry

    def processResources(): Unit = {
      var out: xfs.TextFile = null

      if (this.p.errs != 0) {
        return
      }

      tempResourceNum = 0

      if (this.createefs) {
        val efsfile = getEFSPlace(xPDB.ContentType.EFS)

        out = efsfile.openAsTextForWrite()

        if (out == null) {
          env.errors.envError(xfs.MSG_FILE_CREATE_ERROR, xfs.text.errmsg)
          this.p.errs += 1
          return
        }
      } else {
        // TODO: do we need to do anything if not createefs?
        out = tf.newTextFile(xRamFile.newRamFile(js.jstrEmpty), read = false)
      }

      val efs = new EFS()
      this.mountpoint = efs.newMountpoint(getRelativeAppDir)
      var jremountpoint: Mountpoint = null

      uncompressEFS = env.config.option("UncompressEFS")
      genvcf = !env.config.option("novcf")

      env.info.print("Preparing resources for packing ...\\n")

      var m = this.p.list
      while (m != null) {
        this.process(m)
        m = m.next
      }

      this.processPendingBundles()

      env.info.print("Resources were successfully prepared for packing.\\n")

      if (this.hidedClassesBundles.length != 0) {
        env.config.setEquation2("tohideclassesbundles", this.hidedClassesBundles.toJString)
      }

      val ignoredBundles = env.config.equation("ignoredBundles")
      if (ignoredBundles != null && ignoredBundles.nonEmpty) {
        if (this.packedBundles.length != 0) {
          this.packedBundles.appendChar(';')
        }
        this.packedBundles.appendString(ignoredBundles)
      }

      if (this.packedBundles.length != 0) {
        env.config.setEquation2("packedbundles", this.packedBundles.toJString)
      }

      if (env.config.option("writeclassloadersIDtable")) {
        val classloadersidtable = xPDB.findPlaceToWriteTo(js.newJString("classloadersid"), xPDB.ContentType.CLIDTABLE)
        val table_out = classloadersidtable.openAsTextForWrite()
        if (table_out == null) {
          env.errors.envError(xfs.MSG_FILE_CREATE_ERROR, xfs.text.errmsg)
          this.p.errs += 1
          return
        }
        BundleImportResolver.writeClassloadersIDTable(table_out)
        table_out.closeNew()

        jremountpoint = efs.newMountpoint(js.newJString("$jre"))
        val citablename = classloadersidtable.fullName
        jremountpoint.addResource(js.newJString("lib/classloadersid.table"), citablename, citablename)
      }

      if (!env.config.option("nortvcf")) {
        genVCFZip(this.p)

        if (env.config.tags contains env.regularbuild) {
          if (jremountpoint == null) {
            jremountpoint = efs.newMountpoint(js.newJString("$jre"))
          }
          val vcfZipName = genVCFZipForSinglecomp(this.p)
          if (vcfZipName != null) {
            jremountpoint.addResource(js.newJString("lib/rt-vcf.zip"), vcfZipName, vcfZipName)
          }
        }
      }

      efs.writeTo(out)

      if (targetOS.isWindows) {
        processVersionInfo(this.p)
      }

      this.classPathManager.constructClassPath()

      if (efs.isEmpty) {
        out.closeNew()
      } else {
        out.closeNew()
        if (this.p.errs == 0) {
          env.config.setEquation2("EmbeddedFileSys", out.getName)
          createdEfsName = out.getName
          if (this.createefs) {
            env.info.print("New \"%S\" is generated\\n", out.getName)
          }
        }
      }

      if (pcO.isTomcat && O2Env.env.enabled(GenTomcatScripts)) {
        this.copyTomcatScripts()
      }

      if ((env.config.tags contains env.regularbuild) && languagePack.supports(JAVA) && !(pcO.isTomcat || pcO.isIdea)) {
        this.createJETJreHomeFile()
      }

      pcO.symCache_gc_EndProcessResources()
    }

    def process(m: mk.File): Unit = {
      if (ZIPs contains m.mode) {
        if (!env.config.option("GENDLL") && !O2Env.env.enabled(GenMegaObj)) {
          if (m.mode == mk.md_jar) {
            var attr: Manifest.Attributes = null
            val man = Manifest.getManifest(m.fd.getName)
            if (man != null) {
              attr = man.getMainAttributes
            }
            var main: XString = null
            if (attr != null) {
              main = attr.getValue(Manifest.MAIN_CLASS)
            }
            if (main != null) {
              main = main.replace('.', '/')
              val name = env.config.equation("MAIN")
              assert(name != null && name.nonEmpty)
              if (name.equals(main)) {
                this.processSplash(m, attr)
              }
            }
          }
        }

        env.config.setContext(m.context)
        val packMode = env.getPackMode
        if (env.isPackedIntoExe) {
          var fn: XString = null
          if (packMode != env.PM_ALL && !noClasses(this.p, m)) {
            if (!pcO.isSpringBoot) {
              fn = this.processResource(m, null, null)
            } else {
              // we will process spring boot jar later as a bundle host
            }
          } else {
            fn = m.fd.getName
          }
          val rn = getRelativeResourceName(this.p, m, nullOnFail = false)
          if (!this.mountpoint.pathExists(rn)) {
            if (!pcO.isSpringBoot || (m.tags contains mk.springbootarchive)) {
              // for spring boot we should have the only jar in the classpath
              this.classPathManager.updateClasspath(rn)
              if (pcO.isTomcat) {
                if (this.packedBundles.length != 0) {
                  this.packedBundles.appendChar(';')
                }
                this.packedBundles.appendString(rn)
              }
            }
            if (fn != null) {
              this.mountpoint.addResource(rn, fn, mk.getHostName(m.fd))
            } else {
              assert(pcO.isSpringBoot)
            }
          }
        } else if (!pcO.isSpringBoot || (m.tags contains mk.springbootarchive)) {
          this.classPathManager.updateClasspathWithStandalone(this.p, m)
        }
        env.config.removeContext(m.context)
      } else if (m.mode == mk.md_dir) {
        this.classPathManager.updateClasspathWithStandalone(this.p, m)
      } else if (m.mode == mk.md_bundle && !(m.tags contains mk.bootstrapjar)) {
        env.config.setContext(m.context)

        val pbid = pcNames.parseBundleID(m.name.name)

        val packMode = env.getPackMode
        val bundlePackMode = getBundlePackMode(this.p, pbid.container)
        if (pbid.entry == null) {
          assert(packMode == bundlePackMode)
          if (packMode != env.PM_NONE) {
            if (packMode == env.PM_ALL && FS.isJar(pbid.container)) {
              // add jar bundles in original form
              this.addBundleResource(m, pbid.container, mk.getHostName(m.fd))
            } else if (packMode == env.PM_NONE_AND_OMIT_CLASSES) {
              if (FS.isJar(pbid.container)) {
                env.errors.silentMessage(ErrMsg381, m.name.name)
                this.pendingBundles.add(pbid.container) // add all bundles for packing
              } else {
                this.addBundleResourceToHideClasses(pbid.container, this.processResource(m, pbid.container, null), m.fd, BundleImportResolver.getClassloaderID(pbid.container))
              }
            } else {
              this.pendingBundles.add(pbid.container) // add all bundles for packing
            }
          }
        } else if (bundlePackMode == env.PM_NONE || bundlePackMode == env.PM_NONE_AND_OMIT_CLASSES) {
          if (!FS.isJar(pbid.container)) {
            // handle entries for directory bundles
            // (leave intact jar bundles that are not for packing)
            packMode match {
              case env.PM_NONCOMPILED |
                   env.PM_AS_DIR_NONCOMPILED |
                   env.PM_RESOURCES =>
                this.processBundleResource(m, pbid.container, pbid.entry, m.name.name, null)
              case env.PM_ALL =>
                val fn = FS.addPath(pbid.container, pbid.entry)
                this.addBundleResource(m, fn, mk.getHostName(m.fd))
              case env.PM_NONE_AND_OMIT_CLASSES =>
                val bid = m.name.name
                this.addBundleResourceToHideClasses(FS.addPath(pbid.container, pbid.entry), this.processResource(m, bid, null), m.fd, BundleImportResolver.getClassloaderID(bid))
              case _ =>
            }
          }
        } else if (packMode == env.PM_NONCOMPILED || packMode == env.PM_AS_DIR_NONCOMPILED || packMode == env.PM_RESOURCES && !(bundlePackMode == env.PM_ALL && FS.isJar(pbid.container))) {
          if (FS.isJar(pbid.entry)) {
            this.processNestedJar(m)
          } else {
            if (this.allNestedDirs == null) {
              this.allNestedDirs = new Hashtable()
            }
            var nestedDirs = this.allNestedDirs.get(pbid.container).asInstanceOf[ArrayBuffer[XString]]
            if (nestedDirs == null) {
              nestedDirs = new ArrayBuffer[XString]
              nestedDirs += m.name.name
              this.allNestedDirs.put(pbid.container, nestedDirs)
            } else {
              nestedDirs += m.name.name
            }
          }
        }
        env.config.removeContext(m.context)
      }
    }

    def processSplash(m: mk.File, attr: Manifest.Attributes): Unit = {
      val splashOpt = env.config.equation("SPLASH")
      if (splashOpt == null && O2Env.env.enabled(SplashGetFromManifest)) {
        // splash option has higher precedence over the manifest setting
        if (attr != null) {
          // look for the splash specified in the manifest of the jar file
          val splash = attr.getValue(Manifest.SPLASHSCREEN_IMAGE)
          if (splash != null) {
            val fd = xPDB.getTempResourcesDir
            if (fd == null) {
              env.errors.fault(xfs.MSG_FILE_CREATE_ERROR, js.newJString("temp resource directory"))
            }
            val splashPath = FS.addPath(fd.getName, splash)
            if (Minizip.unzipEntry(m.fd.getName.toString, splash.toString, splashPath.toString)) {
              env.config.setEquation2("SPLASH", splashPath)
            }
          }
        }
      }
    }

    def processPendingBundles(): Unit = {
      val itBundles = this.pendingBundles.iterator
      while (itBundles.hasNext) {
        val bundle = itBundles.next()
        val m = getHostBundle(this.p, bundle)
        assert(m != null) // importresolver should always add host (even if there is no class-files in it);
        env.config.setContext(m.context)
        this.processBundleResource(m, bundle, null, bundle, this.getNestedFilteredJars(bundle))
        env.config.removeContext(m.context)
      }
    }

    /*
       Filters specified bundle entry with stumps
       and add processed resource to EFS.
       Paramaters:
         bundle, entry, bid: bundle's description
         jars: already filtered nested jars (if any)
    */
    def processBundleResource(m: mk.File, bundle: XString, entry: XString, bid: XString, jars: Hashtable): Unit = {
      var fn: XString = null

      if (entry == null) {
        fn = bundle
      } else {
        fn = FS.addPath(bundle, entry)
      }
      this.addBundleResource(m, fn, this.processResource(m, bid, jars))
    }

    /*
      Adds bundle resource to EFS and saves this fact in "tohideclassesbundles" property.
      Parameters:
        resPath:  relative path of the resource assotiated in EFS for the executable
        fromPath: path to processed resource (located in tmpres) that will be
                  actually packed
        source:   path to original resource (not processed)
    */
    def addBundleResourceToHideClasses(resPath: XString, fromPath: XString, source: xfs.FileDescriptor, bundleID: Int): Unit = {
      this.mountpoint.addResource(resPath, fromPath, mk.getHostName(source))
      if (this.hidedClassesBundles.length != 0) {
        this.hidedClassesBundles.appendChar(';')
      }
      this.hidedClassesBundles.appendString(resPath)
      this.hidedClassesBundles.appendChar('%')
      this.hidedClassesBundles.appendInt(bundleID)
    }

    /*
      Adds bundle resource to EFS and saves this fact in "packedbundles" property.
      Parameters:
        source:   original resource
        resPath:  relative path of the resource assotiated in EFS for the executable
        fromPath: path to processed resource (located in tmpres) that will be
                  actually packed
    */
    def addBundleResource(source: mk.File, resPathPar: XString, fromPath: XString): Unit = {
      var resPath = resPathPar

      // JET-4778 fix: if bundle is packed to EFS as dir then
      // it should not be added as file here to EFS
      if (!isPackAsDir(source)) {
        if (pcO.isSpringBoot) {
          // for Spring Boot unlike other custom classloaders apptypes,
          // Spring Boot jar can be specified in not relative form.
          resPath = getRelativeResourceName(this.p, source, nullOnFail = false)
        }
        this.mountpoint.addResource(resPath, fromPath, mk.getHostName(source.fd))
      }
      if (this.packedBundles.length != 0) {
        this.packedBundles.appendChar(';')
      }
      this.packedBundles.appendString(resPath)
    }

    def processNestedJar(m: mk.File): Unit = {
      val pbid = pcNames.parseBundleID(m.name.name)


      // filter nested jar
      val filteredName = this.processResource(m, m.name.name, null)

      // register filtered
      if (this.nestedJars == null) {
        this.nestedJars = new Hashtable()
      }

      var jar = this.nestedJars.get(pbid.container).asInstanceOf[Hashtable]
      if (jar == null) {
        jar = new Hashtable()
        assert(this.nestedJars.put(pbid.container, jar) == null)
      }
      assert(jar.put(pbid.entry, filteredName) == null)

      this.pendingBundles.add(pbid.container)
    }

    def processResource(m: mk.File, bid: XString, jars: Hashtable): XString = {
      var efsWriter: EFSDataWriter = null
      var resHostName: XString = null
      var path: XString = null
      var res: xfs.FileDescriptor = null

      if (ZIPs contains m.mode) {
        // for app classloader classpath entries that are zips, convert bid to xmZip.FileDescriptor
        res = xmZip.createFileDescriptor(m.fd.getName, js.jstrDot)
      } else {
        res = m.fd
      }
      val packMode = env.getPackMode
      this.filterresources = packMode == env.PM_NONE_AND_OMIT_CLASSES
      this.filterall = packMode == env.PM_RESOURCES
      if (!this.filterall && this.compiledclassesset == null) {
        this.compiledclassesset = fillCompiledClassesSet(this.p)
      }

      var outputname = createTempResourceName(res)

      var asdir = isPackAsDir(m)
      if (res.isInstanceOf[xmZip.FileDescriptor] && !asdir) {
        efsWriter = newJarEFSDataWriter(outputname)
      } else {
        // treat ASDIRNONCOMPILED as NONCOMPILED for directories
        asdir = res.isInstanceOf[xmZip.FileDescriptor]

        val visible = env.config.option("visibleresource")


        val pbid = pcNames.parseBundleID(bid)
        if (pbid.entry == null) {
          path = bid
        } else {
          path = FS.addPath(pbid.container, pbid.entry)
        }
        if (asdir) {
          path = FS.cutExt(path)
          outputname = FS.cutExt(outputname)
          resHostName = outputname
        } else {
          resHostName = FS.HOST.toPlatform(mk.getHostName(res))
        }
        val toNode = this.mountpoint.addPath(path, resHostName, resHostName, visible, isdir = true, null)
        if (asdir) {
          efsWriter = newFromJarToDirEFSDataWriter(outputname, toNode, visible)
        } else {
          efsWriter = newDirEFSDataWriter(outputname, toNode, visible)
        }
      }

      if (this.createefs) {
        this.packResources(res, bid, jars, efsWriter)
      }

      outputname
    }

    def packResources(hostFD: xfs.FileDescriptor, bid: XString, jars: Hashtable, writer: EFSDataWriter): Unit = {
      env.info.print("Processing %S ...\\n", mk.getHostName(hostFD))
      val entriesIt = hostFD.getIterator
      while (entriesIt.next()) {
        val entryName = entriesIt.getRelativeName
        val entryFD = entriesIt.getFileDescriptor

        if (this.processEntry(hostFD, bid, jars, entryName, entryFD)) {
          writer.writeEntry(entryName, entryFD, this.data, this.extra)
        }
      }
      writer.close()
    }

    def getNestedFilteredJars(bundle: XString): Hashtable = {
      if (bundle == null || this.nestedJars == null) {
        return null
      }

      this.nestedJars.get(bundle).asInstanceOf[Hashtable]
    }

    def processEntry(hostFD: xfs.FileDescriptor, bid: XString, jars: Hashtable, entry: XString, entryFD: xfs.FileDescriptor): Boolean = {
      this.data = null
      this.extra = null

      val shouldFilterRes = this.shouldFilterJarEntry(hostFD, bid, entry)
      if (shouldFilterRes == FILTER) {
        return false
      }

      if (isManifest(entry)) {
        this.data = filterManifest(entryFD.getFileContents)
      } else if (jars != null && FS.isJar(entry)) {
        val filteredNestedJar = jars.get(entry).asInstanceOf[XString]
        if (filteredNestedJar != null) {
          // current entry is in fact a nested jar we filtered on first pass
          val in = xfs.raw.openToRead(filteredNestedJar)
          if (in == null) {
            env.errors.fault(xfs.MSG_FILE_OPEN_ERROR, xfs.raw.errmsg)
          }

          val buf = in.readFully()
          in.close()
          this.data = newRawData(buf, buf.length)
        }
      } else if (shouldFilterRes == GEN_VCF) {
        val m = this.findCompiledClass(bid, entry)
        this.extra = formVCFData(m.u.asInstanceOf[mk.File].clazz)
        this.data = newRawData(this.extra.data, 1)
      }

      true
    }

    def shouldFilterJarEntry(hostFD: xfs.FileDescriptor, bid: XString, entry: XString): ShouldFilterResult = {
      if (env.getPackMode == env.PM_ALL) {
        return NOT_FILTER
      }
      if (this.p.extToMode(FS.getExt(entry)) != mk.md_jbc) {
        if (isDigest(entry)) {
          return FILTER
        }

        if (this.filterresources) {
          return FILTER
        } else {
          return NOT_FILTER
        }
      }

      val jname = FS.cutExt(entry)

      if (this.filterall) {
        // filterall mode is secret mode used in
        // JET profile compilation for filtering rt.jar
        // We should not filter classes that are marked for VCF excluding
        // to make sure they will be available at runtime.
        if (!jbcFront.isInVCFExcluded(jname)) {
          return FILTER
        } else {
          return NOT_FILTER
        }
      }

      assert(this.compiledclassesset != null)

      val m = this.findCompiledClass(bid, entry)
      if (m != null) {
        assert(m.u != null && m.u.asInstanceOf[mk.File].mode == mk.md_jbc)

        // Check that compiled class was taken from the same place we are
        // currently processing.
        //
        // Otherwise there is a class file duplication, and we should
        // filter out the class file (see JET-4892). As an option, we could
        // leave it as is, but we should not replace it with VCF/stump in any case.
        //
        if (isSameEntry(hostFD, entry, m.u.asInstanceOf[mk.File].fd)) {
          if (genvcf) {
            val genvcf4class = m.u.asInstanceOf[mk.File].clazz

            if (!genvcf4class.hasManagedMetaInformation) {
              return FILTER
            }

            env.config.setContext(m.u.asInstanceOf[mk.File].context)
            assert(env.shouldGenerateVCF())
            env.config.removeContext(m.u.asInstanceOf[mk.File].context)

            if (genvcf4class.isVCFExcluded) {
              // do not filter VCF excluded classes as they may be required at runtime
              return NOT_FILTER
            }

            if (genvcf4class.isVerifiable) {
              return GEN_VCF
            } else if (env.config.option("filterbadclasses")) {
              return FILTER
            } else {
              return NOT_FILTER
            }
          }

          if (bid != null) {
            // for custom classloaders we need to place in jars some classes
            // to let underlying framework to resolve dependencies
            val genvcf4class = m.u.asInstanceOf[mk.File].clazz
            if (genvcf4class.isVCFExcluded) {
              return NOT_FILTER
            }
            return GEN_VCF
          }
        } else if (pcO.isSpringBoot && genvcf && entry.equals2("org/springframework/boot/loader/jar/JarFile.class")) {
          // special case: we patch above class in import resolver and place it to
          // an additional jar file. Let's put VCF for it.
          return GEN_VCF
        } else if (isWorkMode) {
          env.info.print("=========================================\\n")
          env.info.print("WARNING: Duplicated class files detected:\\n")
          env.info.print("Filtered class file %S from %S, as it was compiled from %S\\n", entry, mk.getHostName(hostFD), mk.getHostName(m.u.asInstanceOf[mk.File].fd))
        }

        return FILTER
      }

      if (bid == null) {
        // JET-2491 fix: filter also duplicated classes (from sym.pdb -- platform)
        // TODO: Filter platform classes from bundles too
        val cname = pcNames.newClassName(jname)
        val tname = FS.cutExt(cname.getMangledName)
        val writePlace = xPDB.findPlaceToWriteTo(tname, xPDB.ContentType.SYM)
        val readPlace = xPDB.findPlaceToReadFrom(tname, xPDB.ContentType.SYM)
        if (readPlace != null && readPlace.exists && readPlace.fullName != writePlace.fullName) {
          FILTER
        } else {
          NOT_FILTER
        }
      } else if (this.filterresources) {
        FILTER
      } else {
        NOT_FILTER
      }
    }

    def findCompiledClass(bid: XString, entry: XString): mk.File = {
      if (this.p.extToMode(FS.getExt(entry)) != mk.md_jbc) {
        return null
      }

      val jname = FS.cutExt(entry)

      assert(this.compiledclassesset != null)

      if (bid != null) {
        var pbid = pcNames.parseBundleID(bid)
        val cname = if (pcO.isSpringBoot && pbid.entry == null) {
          pcNames.newClassName(jname)
        } else {
          pcNames.newBundleClassName(jname, BundleImportResolver.getClassloaderStringIDbyBID(bid))
        }
        var m = this.compiledclassesset.get(cname).asInstanceOf[mk.File]
        if (m != null) {
          return m
        }
        if (this.allNestedDirs == null) { // no nested dirs, nothing to search further
          return null
        }
        if (pbid.entry != null) {
          // we are processing bundle entry, not bundle itself
          return null
        }
        val nestedDirs = this.allNestedDirs.get(pbid.container).asInstanceOf[ArrayBuffer[XString]]
        if (nestedDirs == null) {
          return null
        }
        // if there are nested dirs (directories classpath entries)
        // for this entry, then we look for the class in them
        for (bid <- nestedDirs) {
          pbid = pcNames.parseBundleID(bid)
          pbid.entry = FS.HOST.fromPlatform(pbid.entry)
          if (!pbid.entry.endsWith(jstrSlash)) {
            pbid.entry = pbid.entry.concat(jstrSlash)
          }
          if (jname.startsWith(pbid.entry, 0)) {
            val cname = pcNames.newBundleClassName(jname.substring(pbid.entry.length), BundleImportResolver.getClassloaderStringIDbyBID(bid))
            m = this.compiledclassesset.get(cname).asInstanceOf[mk.File]
            if (m != null) {
              return m
            }
          }
        }
        null
      } else {
        this.compiledclassesset.get(pcNames.newClassName(jname)).asInstanceOf[mk.File]
      }
    }

    def createJETJreHomeFile(): Unit = {
      val outputDir = env.config.equation("outputdir")
      val outputName = env.config.equation("outputname")
      val jetjrehomefile = FS.makeFileName(outputDir, outputName, js.newJString("jetjrehome"))
      val out = xfs.text.openToWrite(jetjrehomefile)
      if (out == null) {
        env.errors.envError(xfs.MSG_FILE_CREATE_ERROR, xfs.text.errmsg)
        this.p.errs += 1
        return
      }
      out.print("%S", env.getProfileJREDir)
      out.closeNew()
    }

    def copyTomcatScripts(): Unit = {
      val scriptsIter: TomcatScriptsIter = new TomcatScriptsIter()

      val tomcatVersion = env.config.equation("tomcatversion")
      if (tomcatVersion == null || tomcatVersion.isEmpty) {
        throw new AssertionError
      }

      val tmp = FS.addPath(env.config.equation("jet_home"), js.newJString("lib/tomcat"))
      val srcScriptsDir = FS.addPath(tmp, tomcatVersion)
      val outputDir = env.config.equation("outputdir")
      val extBat = js.format("%s%s", ".", FS.TARGET.batchExtension)

      val srcScriptsDirFD = xfs.sys.createFileDescriptor(srcScriptsDir)
      if (srcScriptsDirFD.exists) {
        scriptsIter.p = this.p
        scriptsIter.srcDir = srcScriptsDir
        // output dir already initialized in xcMain.InitOutputDir
        scriptsIter.dstDir = outputDir
        scriptsIter.extBat = extBat

        if (srcScriptsDirFD.iterateDir(scriptsIter)) {
        }
      }

      val setclasspath = FS.makeFileName(outputDir, js.newJString("setclasspath"), extBat)
      val out = xfs.text.openToWrite(setclasspath)
      if (out == null) {
        env.errors.envError(xfs.MSG_FILE_CREATE_ERROR, xfs.text.errmsg)
        this.p.errs += 1
        return
      }

      val tomcatJuliExists = this.mountpoint.pathExists(js.newJString("bin/tomcat-juli.jar"))
      val loggingPropertiesExists = this.mountpoint.pathExists(js.newJString("conf/logging.properties"))


      if (targetOS.isLinux) {
        out.print("#!/bin/sh\\n")
        out.print("export SCRIPTS_DIR=`dirname \"$0\"`\\n")
        out.print("export MAINCLASS=%S\\n", env.config.equation("Main").replace('/', '.'))
        out.print("export RUNTOMCAT=$SCRIPTS_DIR/%S\\n", pcO.currentComponentName())
        if (tomcatJuliExists) {
          out.print("export TOMCAT_JULI=EXISTS\\n")
        }
        if (loggingPropertiesExists) {
          out.print("export LOGGING_PROPERTIES=EXISTS\\n")
        }
      } else if (targetOS.isWindows) {
        out.print("@echo off\\n")
        out.print("set SCRIPTS_DIR=%%~dp0\\n")
        out.print("set MAINCLASS=%S\\n", env.config.equation("Main").replace('/', '.'))
        out.print("set RUNTOMCAT=\"%%SCRIPTS_DIR%%%S\"\\n", pcO.currentComponentName())
        if (tomcatJuliExists) {
          out.print("set TOMCAT_JULI=EXISTS\\n")
        }
        if (loggingPropertiesExists) {
          out.print("set LOGGING_PROPERTIES=EXISTS\\n")
        }
        // set ERRORLEVEL to 0: see JET-6802.
        out.print("exit /b 0\\n")
      } else {
        throw new AssertionError // Unknown target_platform_os
      }

      out.closeNew()

      xfs.sys.makeExecutable(setclasspath)
    }

  }


  private class TomcatScriptsIter extends xfs.DirIterator {

    private[xcResourcesModule] var p: mk.Project = _
    private[xcResourcesModule] var srcDir: XString = _
    private[xcResourcesModule] var dstDir: XString = _
    private[xcResourcesModule] var extBat: XString = _

    override def entry(name: XString, dir: Boolean): Boolean = {
      if (!dir && name.endsWith(this.extBat)) {
        val srcFile = FS.addPath(this.srcDir, name)
        val dstFile = FS.addPath(this.dstDir, name)
        if (!copyFile(srcFile, dstFile)) {
          this.p.errs += 1
        }
        xfs.sys.makeExecutable(dstFile)
      }
      false
    }

  }

  type ShouldFilterResult = UByte
  val FILTER: ShouldFilterResult = UByte(0)
  val NOT_FILTER: ShouldFilterResult = UByte(1)
  val GEN_VCF: ShouldFilterResult = UByte(2)

  private val ZIPs: mk.SetOfModes = mk.SetOfModes.of(mk.md_jar, mk.md_war, mk.md_zip)
  private val VERSIONINFO_MAX_STR_LEN: Int = 1999 // limitation of XRC. see JET-3523
  var createdEfsName: XString = _
  private var uncompressEFS: Boolean = _
  private var genvcf: Boolean = _
  private val jstrSlash: XString = js.newJString("/")
  private val jstrDotClass: XString = js.newJString(".class")
  private var tempResourceNum: Int = _

  private def getEFSPlace(type0: xPDB.ContentType): xPDB.Placeholder = {
    val outname = env.config.equation("OutputName")
    val name = if (outname != null) FS.getBaseName(outname) else XString("tmp")
    xPDB.findPlaceToWriteTo(name, type0)
  }

  private def newRawData(data: Array[Byte], len: Int): RawData = {
    val res = new RawData()
    res.data = data
    res.len = len
    res
  }

  private def newEFSNode(name: XString, hostname: XString, visible: Boolean, isdir: Boolean, extra: RawData): EFSNode = {
    val node = new EFSNode()
    node.init(name, hostname, visible, isdir, extra)
    node
  }

  private def indentLine(out: xfs.TextFile, indent: Int): Unit = {
    for (_ <- 1 to indent) {
      out.print("  ")
    }
  }

  private def newJarEFSDataWriter(jarFileName: XString): JarEFSDataWriter = {
    val name = jarFileName.toString
    val writer = new JarEFSDataWriter(name)
    writer.zw = try {
      Minizip.openWriter(name)
    } catch {
      case _: IOException => null
    }
    writer
  }

  private def newDirEFSDataWriter(dir: XString, dirNode: EFSNode, visible: Boolean): DirEFSDataWriter = {
    val writer = new DirEFSDataWriter()
    writer.init(dir, dirNode, visible)
    writer
  }

  private def createTempResourceName(resource: xfs.FileDescriptor): XString = {
    val tmpDir = xPDB.getTempResourcesDir
    if (tmpDir == null) {
      env.errors.fault(xfs.MSG_FILE_CREATE_ERROR, js.newJString("temp resource directory"))
    }
    var fname = FS.HOST.fromPlatform(mk.getHostName(resource))
    val bname = js.format("%S__%d", FS.getBaseName(fname), tempResourceNum)
    fname = FS.makeFileName(tmpDir.getName, bname, FS.getExt(fname))

    tempResourceNum += 1

    FS.HOST.toPlatform(fname)
  }

  private def handleLongName(fdPar: xfs.FileDescriptor): xfs.FileDescriptor = {
    var fd = fdPar
    val FILE_NAME_LIMIT: Int = 255

    val curdirfullname = FS.HOST.toPlatform(FS.HOST.fullPath(js.jstrDot))
    if (curdirfullname.length + fd.getName.length + 1 > FILE_NAME_LIMIT) {
      // handle long names (>255) placing temporal file
      // closer to PDB root
      fd = xfs.sys.createFileDescriptor(createTempResourceName(fd))
    }
    fd
  }

  private def newFromJarToDirEFSDataWriter(dir: XString, dirNode: EFSNode, visible: Boolean): FromJarToDirEFSDataWriter = {
    val writer = new FromJarToDirEFSDataWriter()
    writer.init(dir, dirNode, visible)
    writer
  }

  private def copyFile(from: XString, to0: XString): Boolean = {
    val in = xfs.raw.openToRead(from)
    if (in == null) {
      env.errors.envError(xfs.MSG_FILE_OPEN_ERROR, xfs.raw.errmsg)
      return false
    }
    val out = xfs.raw.openToWrite(to0)
    if (out == null) {
      env.errors.envError(xfs.MSG_FILE_OPEN_ERROR, xfs.raw.errmsg)
      return false
    }
    out.writeFile(in)
    in.close()
    out.closeNew()
    true
  }

  // Returns appdir path relative to outputdir or "." if they are equals
  private def getRelativeAppDir: XString = {
    val appdir = env.config.equation("appdir")
    val outputdir = env.config.equation("outputdir")
    if (pcO.isSpringBoot || appdir == null || outputdir == null || appdir.equals(outputdir)) {
      js.jstrDot
    } else if (pcO.isTomcat || pcO.isIdea) {
      js.jstrTwoDots
    } else {
      throw new AssertionError
    }
  }

  def execute(command: String, args: Seq[String]): Boolean = {
    val res = PortableProgExec.executeCommand(command, args)
    val cmdstr = XString(command + " " + args.mkString(" "))
    if (res > 0) {
      env.errors.envError(ErrMsg439, res, cmdstr) // INC(p.errs);
      return false
    } else if (res != 0) {
      env.errors.envError(ErrMsg447, cmdstr) // INC(p.errs);
      return false
    }
    true
  }

  private def fillCompiledClassesSet(p: mk.Project): Hashtable = {
    val compiledclassesset = new Hashtable()
    var m = p.list
    while (m != null) {
      if (m.mode == mk.md_obj && !(m.tags contains mk.redundant)) {
        if (pcO.isClassShouldNotBeFiltered(m.name.name)) {
          env.config.setContext(m.u.asInstanceOf[mk.File].context)
          val value = env.config.equation("PROTECT")
          if (value != null && value.equals2("ALL")) {
            compiledclassesset.put(m.name, m)
          }
          env.config.removeContext(m.u.asInstanceOf[mk.File].context)
        } else {
          compiledclassesset.put(m.name, m)
        }
      }
      m = m.next
    }
    compiledclassesset
  }

  private def isManifest(entryPar: XString): Boolean = {
    var entry = entryPar

    entry = entry.toUpperCase
    entry.equals2("META-INF/MANIFEST.MF")
  }

  private def filterManifest(buf: Array[Byte]): RawData = {
    val man = Manifest.readManifest(tf.newTextFile(xRamFile.newRamFile(js.jstrEmpty, buf, buf.length), read = true))
    if (man == null) {
      // JET-5475: bad manifest format. Do not filter it.
      return newRawData(buf, buf.length)
    }
    val f = new ManifestFilter()
    man.filter(f)
    val ramfile = xRamFile.newRamFile(js.jstrEmpty)
    man.write(tf.newTextFile(ramfile, read = false))
    newRawData(ramfile.getBytes, ramfile.lengthAsInt)
  }

  private def isDigest(entry: XString): Boolean = {
    val name = FS.getBaseName(entry)

    var dir = FS.getPath(entry)
    dir = dir.toUpperCase

    var ext = FS.getExt(entry)
    ext = ext.toUpperCase

    dir.equals2("META-INF") && (ext.equals2("SF") || ext.equals2("RSA") || ext.equals2("DSA") || name.startsWithIgnoreCase(js.newJString("SIG-"), 0))
  }

  private def getHostBundle(p: mk.Project, bundle: XString): mk.File = {
    if (!pcO.isSpringBoot) {
      p.getProjectFile(pcNames.newBundleName(bundle), mk.SetOfModes.of(mk.md_bundle))
    } else {
      // spring boot jar serves as bundle host
      val springbootarchive = env.config.equation("SPRINGBOOTARCHIVE")
      val ext = FS.getExt(springbootarchive)
      p.getProjectFile(pcNames.newFileName(FS.cutExt(springbootarchive), ext), mk.SetOfModes.of(p.extToMode(ext)))
    }
  }

  private def getBundlePackMode(p: mk.Project, bundle: XString): env.PackMode = {
    val m = getHostBundle(p, bundle)
    assert(m != null) // importresolver should always add host (even if there is no class-files in it);
    env.config.setContext(m.context)
    val mode = env.getPackMode
    env.config.removeContext(m.context)
    mode
  }

  private def putData(buf: Array[Byte], pos: Int, data: Int): Unit = {
    val x = data.toUInt
    buf(pos) = (x & 0xFF.toUInt).toByte
    buf(pos + 1) = (x >>> 8 & 0xFF.toUInt).toByte
    buf(pos + 2) = (x >>> 16 & 0xFF.toUInt).toByte
    buf(pos + 3) = (x >>> 24 & 0xFF.toUInt).toByte
  }

  /**
    Class ID contains two parts: first three bytes forms classloader id of the class,
    last byte is package depth (number of subpackages) of the class name to
    retrieve the name from EFS.
  */
  private def getClassId(clazz: pcO.Class): Int = {
    var packageDepth = 0
    val name = clazz.name
    var pos = name.indexOf('/')
    while (pos >= 0) {
      packageDepth += 1
      pos = name.indexOf('/', pos + 1)
    }
    assert(packageDepth < 256)
    (packageDepth << 24) + clazz.getClassloaderID
  }

  private def formVCFData(clazz: pcO.Class): RawData = {
    assert(!clazz.isVCFExcluded)

    val vcfdata = new Array[Byte](16)
    val vcfdataLen = 4 + xjRTS.VCF_DATA_LEN

    val name = ObjNames.getClassName(clazz, CL_slash)
    val nameHash = name.getJavaHashCode

    val classId = getClassId(clazz)
    val vcfLen = clazz.getVCFSizeEstimation

    putData(vcfdata, 0, (xjRTS.VCF_DATA_LEN.toInt << 16) | xjRTS.VCF_MAGIC.toInt)
    putData(vcfdata, 4, vcfLen)
    putData(vcfdata, 8, classId)
    putData(vcfdata, 12, nameHash)
    newRawData(vcfdata, vcfdataLen)
  }

  /*
     Adds VCF descriptor entry for the given class into zip file.
  */
  private def addVCFEntry(m: mk.File, clazz: pcO.Class, vcfZip: Minizip.Writer): Unit = {
    val entryName = clazz.name.concat(jstrDotClass)

    val extra = formVCFData(clazz)
    val extraData = extra.data
    val data = extraData.slice(0, 1) // do we really need this 1 byte data in the entry?
    val mtime = m.u.asInstanceOf[mk.File].fd.modifyTime()
    vcfZip.putBytesAndExtraToArchive(data, extraData, Path.rel(entryName.utf8ToString), mtime)
  }

  /*
    Generates a standalone zip-file with VCF entries for all compiled classes,
    if "genvcf2zip" equation is specified.
  */
  private def genVCFZip(p: mk.Project): Unit = {
    val vcfZipName = env.config.equation("genvcf2zip")
    if (vcfZipName == null) {
      return
    }

    val vcfZip = try {
      Minizip.openWriter(vcfZipName.toString, xfs.sys.exists(vcfZipName))
    } catch {
      case _: IOException => env.errors.fault(xfs.MSG_FILE_OPEN_ERROR, vcfZipName)
    }

    Using.resource(vcfZip) { vcfZip =>
      var m = p.list
      while (m != null) {
        if (m.mode == mk.md_obj && !(m.tags contains mk.redundant)) {
          assert(m.u.asInstanceOf[mk.File].mode == mk.md_jbc)

          if (xcComp.isCompilable(m.u.asInstanceOf[mk.File]) != mk.none) {
            val clazz = m.u.asInstanceOf[mk.File].clazz
            if (!clazz.isLambdaClass) {
              assert(clazz.isPlatformClass)
              if (clazz.hasManagedMetaInformation && clazz.isVerifiable && !clazz.isVCFExcluded) {
                addVCFEntry(m, clazz, vcfZip)
              }
            }
          }
        }
        m = m.next
      }
    }
  }

  /*
    Generates a standalone zip file with VCF entries for all compiled
    platform classes.

    Returns name of the generated zip file.
  */
  private def genVCFZipForSinglecomp(p: mk.Project): XString = {
    val place = getEFSPlace(xPDB.ContentType.VCFZIP)
    val vcfZipName = place.fullName
    assert(vcfZipName != null)

    val vcfZip = try {
      Minizip.openWriter(vcfZipName.toString)
    } catch {
      case _: IOException => env.errors.fault(xfs.MSG_FILE_OPEN_ERROR, vcfZipName)
    }

    Using.resource(vcfZip) { vcfZip =>
      var m = p.list
      while (m != null) {
        if (m.mode == mk.md_sym && (m.u eq m) && !(m.tags contains mk.redundant)) {
          val clazz = pcO.findClassByNameObject(m.name)
          if (!clazz.isLambdaClass) {
            assert(clazz.isCompilable)
            assert(clazz.isPlatformClass)

            if (clazz.hasManagedMetaInformation && clazz.isVerifiable && !clazz.isVCFExcluded) {
              addVCFEntry(m, clazz, vcfZip)
            }
          }
        }
        m = m.next
      }
    }

    vcfZipName
  }

  //--------------------------------------------------------------------------
  // Returns full name of resource jar/dir
  private def getFullResourceName(p: mk.Project, m: mk.File): XString = {
    val rn = FS.HOST.fullPath(m.name.name)
    if (m.mode != mk.md_dir) {
      FS.addExt(rn, p.modeToExt(m.mode))
    } else {
      rn
    }
  }

  /*
     Tries to obtain name of resource jar/dir relative to appdir.
     If it is not relative to appdir then returns null if nullOnFail
     specified else simple name.
  */
  private def getRelativeResourceName(p: mk.Project, m: mk.File, nullOnFail: Boolean): XString = {
    var startsWith: Boolean = false

    val rn = getFullResourceName(p, m)

    var appdir = env.config.equation("appdir")
    if (appdir == null) {
      appdir = js.jstrDot
    }
    appdir = FS.HOST.fullPath(appdir)

    if (m.mode == mk.md_dir && rn.equals(appdir)) {
      return js.jstrDot
    }

    if (appdir.charAt(appdir.length - 1) != '/') {
      appdir = js.format("%S/", appdir)
    }


    if (FS.TARGET.isCaseSensitive) {
      startsWith = rn.startsWith(appdir, 0)
    } else {
      startsWith = rn.startsWithIgnoreCase(appdir, 0)
    }

    if (startsWith) {
      rn.substring(appdir.length)
    } else if (nullOnFail) {
      null
    } else {
      FS.cutPath(rn)
    }
  }

  // Transforms 'Relative to appdir' filenames to 'relative to outputdir' form.
  // Args: rn - filename in 'relative to appdir' form
  private def transformRelativePath(rn: XString): XString = {
    val appdirRelativeToOutputDir = getRelativeAppDir
    if (!appdirRelativeToOutputDir.equals(js.jstrDot)) {
      if (rn.equals(js.jstrDot)) {
        return appdirRelativeToOutputDir
      } else {
        return FS.addPath(appdirRelativeToOutputDir, rn)
      }
    }
    rn
  }

  //--------------------------------------------------------------------------
  private def isHexChar(ch: Char): Boolean = {
    ch match {
      case _ if ('0' <= ch && ch <= '9') ||
                ('a' <= ch && ch <= 'f') ||
                ('A' <= ch && ch <= 'F') =>
        true
      case _ =>
        false
    }
  }

  private def shieldQuotesAndSlashesAndTrim(s: XString): XString = {
    val buf = new js.StringBuffer()
    var i = 0
    var totalSymbolsCount = 0
    while (i < s.length && totalSymbolsCount < VERSIONINFO_MAX_STR_LEN) {
      val c = s.charAt(i)
      if (c == '\"' || c == '\'' || c == '\\' && i + 1 < s.length && s.charAt(i + 1) != 'x') {
        buf.appendChar('\\')
      }
      if (c == '\\' && i + 5 < s.length && s.charAt(i + 1) == 'x' && isHexChar(s.charAtAsChar(i + 2)) && isHexChar(s.charAtAsChar(i + 3)) && isHexChar(s.charAtAsChar(i + 4)) && isHexChar(s.charAtAsChar(i + 5))) {
        totalSymbolsCount -= 5
      }
      buf.appendChar(c)
      i += 1
      totalSymbolsCount += 1
    }
    buf.toJString
  }

  private def getOption(name: String): XString = {
    val value = env.config.equation(name)
    if (value == null) {
      return js.jstrEmpty
    }
    shieldQuotesAndSlashesAndTrim(value)
  }

  private def parseVersion(value: XString): Array[UShort] = {
    val BUFSIZE: Int = 20

    val version = new Array[UShort](4)
    version(0) = UShort(0)
    version(1) = UShort(0)
    version(2) = UShort(0)
    version(3) = UShort(0)
    if (value == null) {
      return version
    }
    val st = strtok.newStringTokenizer(value, ".")
    var i = 0
    while (st.hasMoreTokens && i < 4) {
      val s = st.nextToken()
      if (s.length >= BUFSIZE) {
        return null
      }
      val num = js.parseIntOrElse(s, -1)
      if (num < 0 || num > UShort.MaxValue) {
        return null
      }
      version(i) = num.toUShort
      i += 1
    }
    if (!st.hasMoreTokens && i == 4) {
      version
    } else {
      null
    }
  }

  private def generateRCForVersionInfo(p: mk.Project): xPDB.Placeholder = {
    var filetype: XString = null
    val companyname = getOption("VersionInfoCompanyName")
    val productname = getOption("VersionInfoProductName")
    val filedescription = getOption("VersionInfoFileDescription")
    val legalcopyright = getOption("VersionInfoLegalCopyright")
    val internalname = getOption("outputname")

    val productversion = env.config.equation("VersionInfoProductVersion")
    val version = parseVersion(productversion)
    if (version == null) {
      env.errors.envError(ErrMsg358, productversion)
      p.errs += 1
      return null
    }

    val buf = new js.StringBuffer()
    buf.appendString(internalname)
    if (env.config.option("gendll")) {
      filetype = js.newJString("0x2L")
      buf.append(".dll")
    } else {
      filetype = js.newJString("0x1L")
      buf.append(".exe")
    }
    val originalfilename = buf.toJString

    val versionrc = xPDB.findPlaceToWriteTo(js.newJString("version"), xPDB.ContentType.RC)
    val rc_out = versionrc.openAsTextForWrite()

    rc_out.print("1 VERSIONINFO \\r\\n")
    rc_out.print(" FILEVERSION %d,%d,%d,%d \\r\\n", version(0).toUInt.toInt, version(1).toUInt.toInt, version(2).toUInt.toInt, version(3).toUInt.toInt)
    rc_out.print(" PRODUCTVERSION %d,%d,%d,%d \\r\\n", version(0).toUInt.toInt, version(1).toUInt.toInt, version(2).toUInt.toInt, version(3).toUInt.toInt)
    rc_out.print(" FILEFLAGSMASK 0x3fL \\r\\n")
    rc_out.print(" FILEFLAGS 0x0L \\r\\n")
    rc_out.print(" FILEOS 0x4L \\r\\n")
    rc_out.print(" FILETYPE %S \\r\\n", filetype)
    rc_out.print(" FILESUBTYPE 0x0L \\r\\n")
    rc_out.print("BEGIN \\r\\n")
    rc_out.print("    BLOCK \"StringFileInfo\" \\r\\n")
    rc_out.print("    BEGIN \\r\\n")
    rc_out.print("        BLOCK \"00001200\" \\r\\n")
    rc_out.print("        BEGIN \\r\\n")
    rc_out.print("            VALUE \"Comments\", \"\\\\0\" \\r\\n")
    rc_out.print("            VALUE \"CompanyName\", \"%S\\\\0\" \\r\\n", companyname)
    rc_out.print("            VALUE \"FileDescription\", \"%S\\\\0\" \\r\\n", filedescription)
    rc_out.print("            VALUE \"FileVersion\", \"%d.%d.%d.%d\\\\0\" \\r\\n", version(0).toUInt.toInt, version(1).toUInt.toInt, version(2).toUInt.toInt, version(3).toUInt.toInt)
    rc_out.print("            VALUE \"InternalName\", \"%S\\\\0\" \\r\\n", internalname)
    rc_out.print("            VALUE \"LegalCopyright\", \"%S\\\\0\" \\r\\n", legalcopyright)
    rc_out.print("            VALUE \"LegalTrademarks\", \"\\\\0\" \\r\\n")
    rc_out.print("            VALUE \"OriginalFilename\", \"%S\\\\0\" \\r\\n", originalfilename)
    rc_out.print("            VALUE \"ProductName\", \"%S\\\\0\" \\r\\n", productname)
    rc_out.print("            VALUE \"ProductVersion\", \"%d.%d.%d.%d\\\\0\" \\r\\n", version(0).toUInt.toInt, version(1).toUInt.toInt, version(2).toUInt.toInt, version(3).toUInt.toInt)
    rc_out.print("        END \\r\\n")
    rc_out.print("    END \\r\\n")
    rc_out.print("    BLOCK \"VarFileInfo\" \\r\\n")
    rc_out.print("    BEGIN \\r\\n")
    rc_out.print("        VALUE \"Translation\", 0x0000, 0x1200 \\r\\n")
    rc_out.print("    END \\r\\n")
    rc_out.print("END \\r\\n")

    rc_out.closeNew()

    versionrc
  }

  private def processVersionInfo(p: mk.Project): Unit = {
    if (env.config.option("generateversioninfo")) {
      val rc = generateRCForVersionInfo(p)
      if (rc == null) {
        return
      }
      val res = xPDB.findPlaceToWriteTo(js.newJString("version"), xPDB.ContentType.RES)
      val args = Seq(
        "/c", "65001",
        "/fo", FS.HOST.toPlatform(res.fullName).toString,
        FS.HOST.toPlatform(rc.fullName).toString,
      )
      if (!execute("XRC", args)) {
        p.errs += 1
      }
    }
  }

  //--------------------------------------------------------------------------
  private def noClasses(p: mk.Project, m: mk.File): Boolean = {
    val str = m.fd.getName
    val zf = ZipFile.newZipFile(str)
    var no = true
    if (zf != null) {
      val entries = zf.entries
      while (entries.hasNext) {
        val ze = entries.next()
        val ext = FS.getExt(ze.getName)
        if (ext.nonEmpty) {
          val mode = p.extToMode(ext)
          if (mode == mk.md_jbc) {
            no = false
          }
        }
      }
      zf.close()
    }
    no
  }

  private def isSameEntry(hostFD: xfs.FileDescriptor, entryName: XString, fd: xfs.FileDescriptor): Boolean = {
    if (hostFD.isInstanceOf[xmZip.FileDescriptor]) {
      val hostZipFD = hostFD.asInstanceOf[xmZip.FileDescriptor]

      if (fd.isInstanceOf[xmZip.FileDescriptor]) {
        val zipFD = fd.asInstanceOf[xmZip.FileDescriptor]

        val fname1 = FS.HOST.fromPlatform(hostZipFD.zname)
        val fname2 = FS.HOST.fromPlatform(zipFD.zname)

        if (!fname1.equals(fname2) && !FS.HOST.isSameFile(fname1, fname2)) {
          return false
        }

        return entryName.equals(zipFD.ename)
      } else {
        return false
      }
    }

    val fname1 = FS.addPath(FS.HOST.fromPlatform(hostFD.getName), entryName)
    val fname2 = FS.HOST.fromPlatform(fd.getName)

    fname1.equals(fname2) || FS.HOST.isSameFile(fname1, fname2)
  }

  private def isPackAsDir(m: mk.File): Boolean = {
    val packMode = env.getPackMode
    if (packMode == env.PM_AS_DIR_NONCOMPILED) {
      return true
    }
    if (packMode == env.PM_NONCOMPILED && m.mode == mk.md_bundle && m.cpeMode == CPEntryModes.cpe_webapp) {
      // always treat noncompiled mode for WARs as as_dir_noncompiled
      val pbid = pcNames.parseBundleID(m.name.name)
      return pbid.entry == null && m.fd.isInstanceOf[xmZip.FileDescriptor]
    }
    false
  }

  def newResourceProcessor(p: mk.Project, createefs: Boolean): ResourceProcessor = {
    val this0 = new ResourceProcessor()
    this0.p = p
    this0.createefs = createefs

    this0.classPathManager = new ClassPathManager()
    this0.classPathManager.init()

    this0.compiledclassesset = null
    this0.nestedJars = null
    this0.pendingBundles = new mutable.HashSet[XString]

    this0.packedBundles = new js.StringBuffer()
    this0.hidedClassesBundles = new js.StringBuffer()

    this0
  }
}
