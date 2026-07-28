/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u.template

import com.huawei.excelsior.common.Arch.{AMD64, ARM64}
import com.huawei.excelsior.common.Language
import com.huawei.excelsior.common.LanguagePack.{CANGJIE, CANGJIE_JAVA, JAVA, NONE, SCALA}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.{addressSize, languagePack, targetArch, targetOS}
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.o2lib.opt.O2Env
import com.huawei.excelsior.jet.compiler.o2lib.u.{xcFModule, xcMakeModule, xiEnvModule as env}
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xLookupModule as xLookup, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule.Windows
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xcMainModule as xcMain, xcMakeModule as mk, xcResourcesModule as xcResources, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.BoolOption.{CangjieFusionMode, CompressRTData, GenDebug, GenMegaObj, PGO, PrelinkExe, SplashCloseOnAWTWindow}
import com.huawei.excelsior.jet.compiler.options.StrOption
import xscala.properties.OS
import xscala.properties.OS.LINUX

import scala.collection.mutable

object LinkerTemplate {
  private def fileName(name: XString, ext: XString, out: Boolean): XString = {
    var res = FS.addExt(name, ext)

    if (!FS.validName(res)) {
      return js.jstrEmpty
    }

    if (out) {
      val ct = xPDB.getTypeByExt(res)
      if (ct != xPDB.ContentType.UNSUPPORTED) {
        val place = xPDB.findPlaceToWriteTo(FS.cutExt(res), ct)
        res = place.fullName
      } else {
        res = xfs.sys.useFirst(res)
      }
    } else {
      val fd = xLookup.lookup(res, lookInCurrentDir = true)
      if (!fd.exists) {
        res = xfs.sys.useFirst(res)
      } else {
        res = fd.getName
      }
    }
    FS.HOST.toPlatform(res)
  }

  def writeTemplate(p: xcMakeModule.Project): String = {
    val stringBuffer = StringBuilder()

    def iterator(prefix: String, ext: String, addNewLine: Boolean = true): Unit = {
      var file = p.list
      while (file != null) {
        if (file.getFileName.lastIndexOf('.') != -1) {
          val fileExt = file.getFileName.substring(file.getFileName.lastIndexOf('.'))
          if (fileExt.toString == ext) {
            stringBuffer.addAll(s"$prefix$file")
            if (addNewLine) stringBuffer.addOne('\n')
          }
        }
        file = file.next
      }
    }

    def addLine(x: String, addNewLine: Boolean = true) = stringBuffer.addAll(x + (if (addNewLine) "\n" else ""))

    val equation = { (x: String) => env.config.equation(x) }
    val newEquation = { (x: StrOption) => O2Env.env.valueOfOrNull(x) }
    val equationNonEmpty = { (x: String) => val e = env.config.equation(x); e != null && e.toString != "" }
    val option = { (x: String) => env.config.option(x) }
    
    if (targetOS.isLinux) {
      if (O2Env.env.enabled(GenMegaObj)) {
        addLine("-prelinker")
      }

      if (!O2Env.env.enabled(GenMegaObj)) {
        if (targetArch == AMD64) {
          addLine("-ProgramInterpreter=/lib64/ld-linux-x86-64.so.2")
        } else if (targetArch == ARM64) {
          if (option("musl")) {
            addLine("-ProgramInterpreter=/lib/ld-musl-aarch64.so.1")
          } else if (option("ohos")) {
            addLine("-ProgramInterpreter=/lib/ld-musl-aarch64.so.1")
          } else if (option("bionic")) {
            addLine("-ProgramInterpreter=/system/bin/linker64")
          } else {
            addLine("-ProgramInterpreter=/lib/ld-linux-aarch64.so.1")
          }
        }
      }
      
      addLine("-NoDynStrSizeCheck")
    }
    
    if (targetArch == AMD64) {
      addLine("-arch=amd64")
    } else if (targetArch == ARM64) {
      addLine("-arch=arm64")
    } else {
      addLine("-arch=unknown")
    }
    
    if (targetArch == AMD64) {
      addLine("-PIC")
    }
    if (option("bionic")) {
      addLine("-PIE")
    }
    if (targetArch == ARM64 && option("ohos")) {
      addLine("-PIE")
    }
    if (targetArch == ARM64) {
      addLine("-genveneers")
      addLine("-veneeroffsetlimit=67108864")
    }
    if (equation("minorJETVersion") != null && equation("majorJETVersion") != null) {
      addLine(f"-jetcomponent=${equation("majorJETVersion")}.${equation("minorJETVersion")}")
    } else {
      addLine("-jetcomponent")
    }
    
    if (equation("JETEdition") != null) {
      addLine(f"-jetedition=${equation("JETEdition")}")
    }

    if (languagePack == JAVA) {
      addLine(s"-vcode=${equation("vcode")}")
    }

    if (option("gui")) {
      addLine("-sys=W")
    } else {
      addLine("-sys=C")
    }
    
    if (languagePack.supports(Language.JAVA) || option("gendll")) {
      addLine("-nosmart")
    } else {
      addLine("-smart")
    }
    
    if (targetOS.isLinux) {
      addLine(s"-config=StackLimit:${equation("stacklimit")}")
    } else {
      addLine(s"-stack=${equation("stacklimit")}")
    }
    
    if (!O2Env.env.enabled(GenMegaObj) && equation("imagebase") != null && !equation("imagebase").isEmpty) {
      addLine(s"-base=${equation("imagebase")}")
    }
    
    addLine("-LargeAddressAware")
    if (equation("jetvmprop") != null && !equation("jetvmprop").isEmpty) {
      addLine(s"-config=VMProperty:${equation("jetvmprop")}")
    }

    if (languagePack.supports(Language.JAVA)) {
      if (equationNonEmpty("componentClassPath")) {
        addLine(s"-config=ClassPath:${equation("componentClassPath")}")
      }
      if (equation("jet_jre_home") != null) {
        addLine(s"-config=JREHome:${equation("jet_jre_home")}")
      }
      addLine(s"-config=JETProfile:${equation("profile")}")
      addLine(s"-config=JETProfileName:${equation("profile_name")}")
      if (equationNonEmpty("EmbeddedFileSys")) {
        addLine(s"-EmbeddedFileSys=${equation("EmbeddedFileSys")}")
      }
      if (equation("profmode") != null) {
        addLine(s"-config=GenProfile:${equation("profmode")}")
      }
      if (option("disableStackTrace")) {
        addLine("-config=NoStackTrace:1")
      } else {
        if (option("genStackTrace")) {
          addLine("-EmitStackTraceInfo")
        }
      }
      if (equationNonEmpty("disableUsageList")) {
        addLine(s"-config=NoUsageList:1")
      }
      addLine("; TODO enable PGO for LWRT")
      if (O2Env.env.enabled(PGO)) {
        addLine("-config=CompiledWithPGO:1")
      }
      if (option("disableClassSaving")) {
        addLine("-config=NoSavingClasses:1")
      }
      if (equationNonEmpty("standaloneResources")) {
        addLine(s"-config=StandaloneResources:${equation("standaloneResources")}")
      }
      if (equationNonEmpty("version_info")) {
        addLine(s"-config=VersionInfo:${equation("version_info")}")
      }
      if (equationNonEmpty("compatibility_info")) {
        addLine(s"-config=CompatibilityString:${equation("compatibility_info")}")
      }
    }

    addLine(s"-config=CPURequirements:${equation("CPURequirements")}")
    
    if (option("disableJetProfiler")) {
      addLine("-config=NoJetProfiler:1")
    }
    
    if (equation("Main") != null && !equation("Main").isEmpty) {
      addLine(s"-config=MainClass:${equation("Main")}")
    }
    
    if (equation("MainMethodIndex") != null && !equation("MainMethodIndex").isEmpty) {
      addLine(s"-config=MainMethodIndex:${equation("MainMethodIndex")}")
    }
    
    if (ProjectLogic.ForceMainMethodIndex) {
      addLine(s"-config=ForceMainMethodIndex:1")
    }

    if (option("gendll")) {
      addLine(s"-name=${fileName(equation("outputname"), equation("dllext_target"), true)}")
      addLine("-dll", addNewLine = false)
      iterator("=", "expdef", addNewLine = false)
      addLine("")
      //  ! "-dll"
      //  ! { expdef : "=%s",#}
      //  ! "\n"
      if (languagePack == JAVA) {
        addLine("-exp=JNI_GetDefaultJavaVMInitArgs.2")
        addLine("-exp=JNI_CreateJavaVM.3")
        addLine("-exp=JNI_GetCreatedJavaVMs.4")
        addLine("-exp=JVMI_InitJVMInterface.5")
      } else if (languagePack == NONE) {
        addLine("-exp=LWRT_CreateVM.1")
        addLine("-exp=LWRT_AttachThread.2")
        addLine("-exp=LWRT_AttachDaemonThread.3")
        addLine("-exp=LWRT_DetachThread.4")
        addLine("-exp=LWRT_DestroyVM.5")
      } else if (languagePack == CANGJIE) {
        addLine("-exp=CangjieRT_init.1")
        addLine("-exp=CangjieRT_attachCurrentThread.2")
        addLine("-exp=CangjieRT_attachCurrentThreadAsDaemon.3")
        addLine("-exp=CangjieRT_detachCurrentThread.4")
        addLine("-exp=CangjieRT_exit.5")
      } else if (languagePack == CANGJIE_JAVA) {
        addLine("-exp=JNI_GetDefaultJavaVMInitArgs.2")
        addLine("-exp=JNI_CreateJavaVM.3")
        addLine("-exp=JNI_GetCreatedJavaVMs.4")
        addLine("-exp=JVMI_InitJVMInterface.5")
        addLine("-exp=CangjieRT_init.6")
        addLine("-exp=CangjieRT_attachCurrentThread.7")
        addLine("-exp=CangjieRT_attachCurrentThreadAsDaemon.8")
        addLine("-exp=CangjieRT_detachCurrentThread.9")
        addLine("-exp=CangjieRT_exit.10")
      }
    } else if (O2Env.env.enabled(GenMegaObj)) {
      addLine(s"-name=${fileName(equation("outputname"), equation("mobjext"), true)}")
    } else {
      addLine(s"-name=${fileName(equation("outputname"), equation("exeext_target"), true)}")
    }

    if (option("generateversioninfo")) {
      addLine(fileName(XString("version"), XString("res"), false).toString)
    }
    
    if (targetOS.isLinux) {
      addLine("-constr=_xconstr")
    }

    if (option("gendll")) {
      if (targetOS.isWindows) {
        addLine("-entry=DllEntryPoint")
      }

      addLine(s"-LinkFileAsRData=LINK_SplashPicture:${fileName(XString("dummysplash"), XString("lib"), false)}")
    } else {
      if (!O2Env.env.enabled(GenMegaObj)) {
        addLine("-entry=ExeEntryPoint")
        if (targetOS.isLinux) {
          addLine(fileName(XString("startup-entrypoint"), XString("zip"), false).toString)
        }
      }

      if (languagePack.supports(Language.JAVA)) {
        if (ProjectLogic.multiapp || equationNonEmpty("splash")) {
          if (equationNonEmpty("splash")) {
            addLine("-config=HaveSplash:1")
            if (equationNonEmpty("splashMinTime")) {
              addLine(s"-config=SplashMinTime:${equation("SplashMinTime")}")
            }
            if (equationNonEmpty("splashCloseOnTitle")) {
              addLine(s"-config=SplashCloseOnTitle:${equation("splashCloseOnTitle")}")
            } else {
              addLine("-config=SplashCloseOnTitle:")
            }
            if (O2Env.env.enabled(SplashCloseOnAWTWindow)) {
              addLine("-config=SplashCloseOnAWTWindow:1")
            }
            if (option("splashCloseOnClick")) {
              addLine("-config=splashCloseOnClick:1")
            }
            addLine(s"-LinkFileAsRData=LINK_SplashPicture:${equation("splash")}")
          } else {
            addLine(s"-LinkFileAsRData=LINK_SplashPicture:${fileName(XString("dummysplash"), XString("lib"), false)}")
          }

          if (ProjectLogic.multiapp) {
            addLine("-config=MultiMain:1")
          }
        } else {
          addLine(s"-LinkFileAsRData=LINK_SplashPicture:${fileName(XString("dummysplash"), XString("lib"), false)}")
        }
      }
    }

    if (targetArch == AMD64 && targetOS.isWindows) {
      addLine("-GenExceptionInfo=JR_SystemExceptionHandler")
    }

    if (languagePack.supports(Language.JAVA)) {
      if (option("ignoreenvjetvmprop") || option("ignorejetvmpropenvvar")) {
        addLine("-config=IgnoreJETVMPROP:1")
      }
      if (option("ignorejetvmpropfiles")) {
        addLine("-config=IgnoreJETVMPROPFiles:1")
      }
      if (equationNonEmpty("packaging_options")) {
        addLine(s"-config=PackagingOptions:${equation("packaging_options")}")
      }
    }

    if (languagePack.supports(Language.CANGJIE)) {
      if (equation("stdlibCbcPath") != null) {
        addLine(s"-LinkFileAsRData=LINK_StdlibCbc:${equation("stdlibCbcPath")}")
      } else {
        addLine(s"-LinkFileAsRData=LINK_StdlibCbc:!stdlib.cbc")
      }

      if (equation("metaCbcPath") != null) {
        addLine(s"-LinkFileAsRData=LINK_MetaCbc:${equation("metaCbcPath")}")
      } else {
        addLine("-LinkFileAsRData=LINK_MetaCbc:!meta.cbc")
      }
    }

    if (option("regularbuild")) {
      if (languagePack.supports(Language.JAVA)) {
        addLine("-config=SingleComp:1")
        addLine("-config=DeployedApplication:1")
        addLine(s"-ExternalJExport=${fileName(equation("outputname"), equation("jexpext"), true)}")
      }
      if (O2Env.env.enabled(PrelinkExe)) {
        addLine("-exp=main")
      }
      if (languagePack.supports(Language.CANGJIE)) {
        if (!O2Env.env.enabled(PGO) || option("NoJetRTGlobalOptim")) {
          if (option("useobjlib")) {
            if (newEquation(StrOption.UseLibrary) != null && newEquation(StrOption.UseLibrary) == "CangJieStdLib") {
              addLine(fileName(env.config.equation("jet_home") concat XString("/profile/develop/CangJieStdLib/full"), XString("objlib"), false).toString())
            }
          } else {
            addLine(fileName(env.config.equation("jet_home") concat XString("/profile/develop/obj"), XString("zip"), false).toString())
            if (newEquation(StrOption.UseLibrary) != null && newEquation(StrOption.UseLibrary) == "CangJieStdLib") {
              addLine(fileName(env.config.equation("jet_home") concat XString("/profile/develop/CangJieStdLib/obj"), XString("zip"), false).toString())
            }
          }
        }
      }

      if (equation("OBJECTS") != null) {
        addLine(equation("OBJECTS").toString)
      } else {
        iterator("", "sym")
        //        ! { sym : "%s\n",#>objext }
      }
      if (equation("AUXOBJECTS") != null) {
        addLine(equation("AUXOBJECTS").toString)
      }
      if (languagePack.supports(Language.JAVA)) {
        addLine("-config=JETRuntime:")
        addLine(s"-EmbeddedFileSys=${fileName(XString("rt"), XString("efs"), false)}")
        addLine(s"-EmbeddedFileSys=${fileName(XString("nativelibs"), XString("efs"), false)}")
        addLine(s"-EmbeddedFileSys=${fileName(XString("zi"), XString("efs"), false)}")
        if (targetOS.isLinux) {
          addLine("-config=BootClassPath:*{exe.dir}/xresources.jar")
        } else {
          addLine("-config=BootClassPath:*{exe.dir}\\xresources.jar")
        }
      }

      if (!option("noxomfasm")) {
        addLine(fileName(XString("aj-lowlevel"), XString("zip"), false).toString)
        addLine(fileName(XString(s"aj-$languagePack-base-rt-lowlevel"), XString("zip"), false).toString)
        
        if (languagePack == JAVA) {
          addLine(fileName(XString("aj-java-lp-rt-lowlevel"), XString("zip"), false).toString)
        }
        if (languagePack == CANGJIE) {
          addLine(fileName(XString("aj-cangjie-lp-rt-lowlevel"), XString("zip"), false).toString)
        }
        if (languagePack == CANGJIE_JAVA) {
          addLine(fileName(XString("aj-cangjie-java-lp-rt-lowlevel"), XString("zip"), false).toString)
        }
        if (languagePack == SCALA) {
          addLine(fileName(XString("aj-scala-lp-rt-lowlevel"), XString("zip"), false).toString)
        }
      }
      addLine(s"-edf=${fileName(XString("empty"), XString("expdef"), false)}")
    }
    
    if (equation("add_export") != null) {
      addLine(s"-exp=${equation("add_export")}")
    }
    if (equation("stuff_rsp") != null) {
      addLine(s"@${equation("stuff_rsp")}.rsp")
    }
    iterator("-EmbeddedFileSys=", "efs", addNewLine = true)
    // ! { efs : "-EmbeddedFileSys=%s\n",# }
    if (targetOS.isLinux) {
      iterator("", "so")
      // ! { so  : "%s\n",# }
    } else {
      addLine(fileName(XString("import32"), XString("lib"), false).toString)
      iterator("", "dll")
      iterator("", "iso")
      //  ! { dll : "%s\n",# }
      //  ! { ico : "%s\n",# }
      if (!option("gendll")) {
        // option runAsAdmin is unused
        addLine(fileName(XString("app"), XString("res"), false).toString)
      }
    }
    
    addLine("-StrictLinkToDLL")
    
    if (targetOS.isLinux && !O2Env.env.enabled(GenMegaObj)) {
      if (option("bionic")) {
        addLine(fileName(XString("_bionic_libc"), XString("so"), false).toString)
        addLine(fileName(XString("_bionic_libdl"), XString("so"), false).toString)
        
        if (targetArch == AMD64) {
          addLine(fileName(XString("_bionic_libm"), XString("so"), false).toString)
        }
      } else if (option("musl")) {
        addLine(fileName(XString("_musl_libc"), XString("so"), false).toString)
      } else if (option("ohos")) {
        addLine(fileName(XString("_ohos_libc"), XString("so"), false).toString)
      } else {
        addLine(fileName(XString("_libpthread"), XString("so"), false).toString)
        addLine(fileName(XString("_libc"), XString("so"), false).toString)
        addLine(fileName(XString("_libdl"), XString("so"), false).toString)
        addLine(fileName(XString("_libm"), XString("so"), false).toString)
      }
    }
    
    if (equation("printSectionSizes") != null) {
      if (equation("printSectionSizes").isEmpty) {
        addLine(s"-printSectionSizes=${fileName(equation("outputname"), XString("sz"), true)}")
      } else {
        addLine(s"-printSectionSizes=${equation("printSectionSizes")}")
      }
    }

    iterator("", "lib")
    iterator("", "res")

    //  ! { lib : "%s\n",# }
    //  ! { res : "%s\n",# }
    
    if (!option("CleanCompilation")) {
      addLine("-map")
    }
    addLine("-noconsistencyinfo")
    if (!option("efstimestamps")) {
      addLine("-noefstimestamps")
    }
    
    if (O2Env.env.enabled(GenDebug) || option("reusertdwarf")) {
      addLine("-dwarf")
      if (option("gendebugbylinker")) {
        addLine("-gendwarfbylinker")
      }
      addLine(s"-dwarfparts=abbrev:${equation("DEBUG_ABBREV")}")
      addLine(s"-dwarfparts=frame:${equation("DEBUG_FRAME")}")
      addLine(s"-dwarfparts=frame.fx:${equation("DEBUG_FRAME_FX")}")
      addLine(s"-dwarfparts=info:${equation("DEBUG_INFO")}")
      addLine(s"-dwarfparts=info.fx:${equation("DEBUG_INFO_FX")}")
      addLine(s"-dwarfparts=line:${equation("DEBUG_LINE")}")
      addLine(s"-dwarfparts=line.fx:${equation("DEBUG_LINE_FX")}")
      addLine(s"-dwarfparts=pubnames:${equation("DEBUG_PUBNAMES")}")
      addLine(s"-dwarfparts=pubnames.fx:${equation("DEBUG_PUBNAMES_FX")}")
      addLine(s"-dwarfparts=str:${equation("DEBUG_STR")}")
    }
    
    if (option("gensymbols")) {
      addLine("-symbols")
    }
    
    if (!O2Env.env.enabled(CompressRTData)) {
      addLine("-nocompression")
      addLine("-optstr=0")
    }
    
    if (option("nosegorder")) {
      addLine("-nostrictorder")
    }
    
    if (equation("profiledstrings") != null && equation("profiledstrings").toString != "") {
      addLine(s"-profiledstrings=${equation("profiledstrings")}")
    }
    
    if (equation("fastdatasize") != null && equation("fastdatasize").toString != "") {
      addLine(s"-fastdatasize=${equation("fastdatasize")}")
    }
    
    if (option("gentdtablesbylinker")) {
      addLine("-tdtables")
    }
    
    if (option("nogentdtablesbylinker")) {
      addLine("-notdtables")
    }
    
    if (option("noxomfasm")) {
      addLine("-exp=ExceptionHandling_getHandler")
      addLine("-exp=JR_getPhysicalFrameDescriptorBySpecialGuest")
      addLine("-exp=FrameDescriptor_initFD")
      addLine("-exp=JR_InstantiatePendingHardwareException")
      addLine("-exp=JR_GCPointHandler")
      
      if (languagePack.supports(Language.CANGJIE)) {
        addLine("-exp=MemUtils_copyMemory")
        addLine("-exp=CFuncWrappers_wrapperImpl")
        if (O2Env.env.enabled(CangjieFusionMode)) {
          addLine("-exp=CJVM_INTERPRET_METHOD")
        }
      }
    }

    stringBuffer.toString()
  }

  def writeXKRNTemplate(p: xcMakeModule.Project): String = {
    val stringBuffer = StringBuilder()

    def iterator(prefix: String, ext: String, addNewLine: Boolean = true): Unit = {
      var file = p.list
      while (file != null) {
        if (file.getFileName.lastIndexOf('.') != -1) {
          val fileExt = file.getFileName.substring(file.getFileName.lastIndexOf('.'))
          if (fileExt.toString == ext) {
            stringBuffer.addAll(s"$prefix$file")
            if (addNewLine) stringBuffer.addOne('\n')
          }
        }
        file = file.next
      }
    }

    def addLine(x: String, addNewLine: Boolean = true) = stringBuffer.addAll(x + (if (addNewLine) "\n" else ""))

    val equation = { (x: String) => env.config.equation(x) }
    val equationNonEmpty = { (x: String) => val e = env.config.equation(x); e != null && e.toString != "" }
    val option = { (x: String) => env.config.option(x) }

    if (targetOS.isLinux) {
      addLine("-Image=ELF")

      if (targetArch == AMD64) {
        addLine("-ProgramInterpreter=/lib64/ld-linux-x86-64.so.2")
      } else if (targetArch == ARM64) {
        if (option("musl")) {
          addLine("-ProgramInterpreter=/lib/ld-musl-aarch64.so.1")
        } else {
          addLine("-ProgramInterpreter=/lib/ld-linux-aarch64.so.1")
        }
      } /*else if (targetArch == RV64)*/ else {
        addLine("-ProgramInterpreter=UNKNOWN")
      }

      addLine("-NoDynStrSizeCheck")
    } /*else if (targetOS.isOSX)*/
    if (targetArch == AMD64) {
      addLine("-arch=amd64")
    } else if (targetArch == ARM64) {
      addLine("-arch=arm64")
    } else {
      addLine("-arch=unknown")
    }

    if (targetArch == AMD64) {
      addLine("-PIC")
    }
    if (targetArch == ARM64) {
      addLine("-genveneers")
      addLine("-veneeroffsetlimit=67108864")
    }

    if (equation("minorJETVersion") != null && equation("majorJETVersion") != null) {
      addLine(f"-jetcomponent=${equation("majorJETVersion")}.${equation("minorJETVersion")}")
    } else {
      addLine("-jetcomponent")
    }

    if (equation("JETEdition") != null) {
      addLine(f"-jetedition=${equation("JETEdition")}")
    }
    
    if (languagePack == JAVA) {
      addLine(s"-vcode=${equation("vcode")}")
    }
    addLine("-sys=C")
    addLine("-nosmart")
    addLine("-LargeAddressAware")
    
    if (targetArch == Windows) {
      addLine("-stack=100000")
    }
    addLine(s"-name=../jre/jetrt/${equation("dllname")}.${equation("dllext_target")}")
    if (targetArch == Windows) {
      if (equationNonEmpty("SetAutoImagebase")) {
        addLine(s"-AutoImageBase=../jre/jetrt/${equation("SetAutoImagebase")}.DLL")
      } else {
        addLine(s"-base=${equation("imagebase")}")
      }
    }
    
    if (equation("component").toString == "XKRN") {
      addLine("-dll=rt.expdef")
      addLine("-config=JETRuntime:")
      addLine(s"-config=JREHome:${equation("jet_jre_home")}")
      
      addLine(s"-EmbeddedFileSys=${fileName(XString("rt"), XString("efs"), false)}")
      addLine("-EmbeddedFileSys=rt-vcf.efs")
      addLine(s"-EmbeddedFileSys=${fileName(XString("nativelibs"), XString("efs"), false)}")
      addLine(s"-EmbeddedFileSys=${fileName(XString("zi"), XString("efs"), false)}")
      
      if (equationNonEmpty("componentClassPath")) {
        if (targetOS.isLinux) {
          addLine(s"-config=BootClassPath:${equation("componentClassPath")}:*{exe.dir}/xresources.jar")
        } else {
          addLine(s"-config=BootClassPath:${equation("componentClassPath")};*{exe.dir}\\xresources.jar")
        }
      }
      
      addLine(fileName(XString("aj-lowlevel"), XString("zip"), false).toString)
      addLine(fileName(XString("aj-java-base-rt-lowlevel"), XString("zip"), false).toString)
      addLine(fileName(XString("aj-java-lp-rt-lowlevel"), XString("zip"), false).toString)
    } else {
      addLine("-dll")
    }
    
    if (languagePack == JAVA) {
      addLine(s"-config=JETProfile:${equation("profile")}")
      addLine(s"-config=JETProfileName:${equation("profile_name")}")
    }

    if (equationNonEmpty("version_info")) {
      addLine(s"-config=VersionInfo:${equation("version_info")}")
    }
    if (equationNonEmpty("compatibility_info")) {
      addLine(s"-config=CompatibilityString:${equation("compatibility_info")}")
    }

    if (equation("CPURequirements") != null) {
      addLine(s"-config=CPURequirements:${equation("CPURequirements")}")
    } else {
      addLine(s"-config=CPURequirements:0")
    }
    
    if (equationNonEmpty("packaging_options")) {
      addLine(s"-config=PackagingOptions:${equation("packaging_options")}")
    }
    
    val rtcomponent = targetOS match {
      case OS.WINDOWS => "XKRN.DLL"
      case OS.LINUX => "libXKRN.so"
    }
    addLine(s"-Config=RTComponent:$rtcomponent")
    
    addLine(equation("OBJECTS").toString)
    iterator("", "lib")
    
    if (targetOS.isLinux) {
      iterator("", "so")
    } else {
      iterator("", "dll")
      iterator("", "ico")
    }
    iterator("", "res")
    
    addLine(s"-LinkFileAsRData=LINK_SplashPicture:${fileName(XString("dummysplash"), XString("lib"), false)}")

    if (targetOS.isWindows) {
      addLine("-entry=DllEntryPoint")
    } else if (targetOS.isLinux) {
      addLine("-constr=_xconstr")
    }

    if (targetOS.isWindows && targetArch == AMD64) {
      addLine("-GenExceptionInfo=JR_SystemExceptionHandler")
    }

    if (targetOS.isLinux) {
      if (option("bionic")) {
        addLine(fileName(XString("_bionic_libc"), XString("so"), false).toString)
        addLine(fileName(XString("_bionic_libdl"), XString("so"), false).toString)
      } else if (option("musl")) {
        addLine(fileName(XString("_musl_libc"), XString("so"), false).toString)
      } else if (option("ohos")) {
        addLine(fileName(XString("_ohos_libc"), XString("so"), false).toString)
      } else {
        addLine(fileName(XString("_libpthread"), XString("so"), false).toString)
        addLine(fileName(XString("_libc"), XString("so"), false).toString)
        addLine(fileName(XString("_libdl"), XString("so"), false).toString)
        addLine(fileName(XString("_libm"), XString("so"), false).toString)
      }
    } else {
      addLine(fileName(XString("import32"), XString("lib"), false).toString)
    }
    
    if (equationNonEmpty("EmbeddedFileSys")) {
      addLine(s"-EmbeddedFileSys=${equation("EmbeddedFileSys")}")
    }
    if (equation("printSectionSizes") != null) {
      addLine(s"-printSectionSizes=${equation("printSectionSizes")}")
    }
    
    addLine("-noconsistencyinfo")
    addLine("-noefstimestamps")
    
    if (O2Env.env.enabled(GenDebug) || option("reusertdwarf")) {
      addLine("-dwarf")
      if (option("gendebugbylinker")) {
        addLine("-gendwarfbylinker")
      }

      addLine(s"-dwarfparts=abbrev:${equation("DEBUG_ABBREV")}")
      addLine(s"-dwarfparts=frame:${equation("DEBUG_FRAME")}")
      addLine(s"-dwarfparts=frame.fx:${equation("DEBUG_FRAME_FX")}")
      addLine(s"-dwarfparts=info:${equation("DEBUG_INFO")}")
      addLine(s"-dwarfparts=info.fx:${equation("DEBUG_INFO_FX")}")
      addLine(s"-dwarfparts=line:${equation("DEBUG_LINE")}")
      addLine(s"-dwarfparts=line.fx:${equation("DEBUG_LINE_FX")}")
      addLine(s"-dwarfparts=pubnames:${equation("DEBUG_PUBNAMES")}")
      addLine(s"-dwarfparts=pubnames.fx:${equation("DEBUG_PUBNAMES_FX")}")
      addLine(s"-dwarfparts=str:${equation("DEBUG_STR")}")
    }
    
    if (option("gensymbols")) {
      addLine("-symbols")
    }
    if (option("genmapfile")) {
      addLine("-map")
    }
    stringBuffer.toString()
  }
}

