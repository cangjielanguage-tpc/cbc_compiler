/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u

enum ErrMsg(val no: Int, val format: String) {
  case ErrMsg020 extends ErrMsg(20, "Resolve error: \"%S\". \nIf you are sure your project is consistent, enable IgnoreResolveErrors option")

  case ErrMsg190 extends ErrMsg(190, "incorrect header in symbol file \"%S\"")
  case ErrMsg191 extends ErrMsg(191, "incorrect version of symbol file \"%S\" (%d instead of %d).\nRemove old pdb files.")
  case ErrMsg209 extends ErrMsg(209, "INTERNAL FAULT: missing import %S into %S")

  case ErrMsg250 extends ErrMsg(250, "\nThe User's Guide available at\n\n    %S\n")

  case ErrMsg263 extends ErrMsg(263, "The %S option is obsolete. \nTo compile this project file, you should remove this option from it.")

  // ------- WARNINGS
  case ErrMsg320 extends ErrMsg(320, "undeclared option \"%s\"")
  case ErrMsg321 extends ErrMsg(321, "option \"%s\" is already defined, redefinition has ignored")
  case ErrMsg322 extends ErrMsg(322, "undeclared equation \"%s\"")
  case ErrMsg323 extends ErrMsg(323, "equation \"%s\" is already defined, redefinition has ignored")
  case ErrMsg326 extends ErrMsg(326, "option \"%s\" can not be defined because equation with the same name is already defined")
  case ErrMsg327 extends ErrMsg(327, "equation \"%s\" can not be defined because option with the same name is already defined")
  case ErrMsg349 extends ErrMsg(349, "file %S.%S not found.")

  case ErrMsg355 extends ErrMsg(355, "The splash image \"%S\" has the BMP format which is no longer supported.\n Please, convert the image into PNG or JPEG formats.")

  case ErrMsg357 extends ErrMsg(357, "Version Info support is not available.\nTo compile this project file, you should remove the version info settings from it.")
  case ErrMsg358 extends ErrMsg(358, "#Bad version format: \"%S\". Please, correct VersionInfoProductVersion equation.")

  case ErrMsg369 extends ErrMsg(369, "Compiler has failed to detect the main class.")
  case ErrMsg370 extends ErrMsg(370, "No classes contain \"main\"")
  case ErrMsg374 extends ErrMsg(374, "\nThe class specified in the MAIN equation does not have the entry method. Valid values are: %S\nAdd one of the above equations to the JC command line or your project file.")
  case ErrMsg375 extends ErrMsg(375, "\nThe MAIN equation must be specified to resolve ambiguity. Possible values:%S\nAdd one of the above equations to the JC command line or your project file.")
  case ErrMsg379 extends ErrMsg(379, "#Compilation of %S ignored:\n class \"%S\" is given in incorrect case.")
  case ErrMsg381 extends ErrMsg(381, "Invalid \"-pack=noneandomitclasses\" pack mode is specified for \"%S\" entry. \"-pack=noncompiled\" is used instead.")
  case ErrMsg385 extends ErrMsg(385, "The class specified in the MAIN equation is not verifiable \n(see the above error message)")
  case ErrMsg386 extends ErrMsg(386, "The class specified in the MAIN equation has unsupported version.")

  case ErrMsg389 extends ErrMsg(389, "WORKSTATION runtime is obsolete. DESKTOP runtime will be used instead.")
  case ErrMsg391 extends ErrMsg(391, "Valid values of the JETRT equation are CLASSIC, DESKTOP and SERVER")

  case ErrMsg399 extends ErrMsg(399, "#splash image not found \"%S\"")

  // ------- make errors

  case ErrMsg402 extends ErrMsg(402, "#compile \"%S\" (\"%S\" not found).\nAdd class to lookup or, if you are sure your project does not\ncontain it, set the CLASSABSENCE equation to HANDLE")
  case ErrMsg405 extends ErrMsg(405, "#module not found \"%S\".\nWARNING: directives \"!module\" and \"!batch\" allow\nTHE ONLY file(directory) to be specified")
  case ErrMsg406 extends ErrMsg(406, "#undefined file extension \"%S\"")
  case ErrMsg407 extends ErrMsg(407, "#module not found \"%S\"")
  case ErrMsg413 extends ErrMsg(413, "#no modules in the project")
  case ErrMsg414 extends ErrMsg(414, "#file \"%S\" (line %d): %S")
  case ErrMsg415 extends ErrMsg(415, "#unknown operation mode \"%s\"")
  case ErrMsg416 extends ErrMsg(416, "#inconsistent set of operation modes")
  case ErrMsg417 extends ErrMsg(417, "#no class-files in the project")
  case ErrMsg419 extends ErrMsg(419, "Unsupported file format: %S.java")
  case ErrMsg424 extends ErrMsg(424, "file create error: %S")
  case ErrMsg425 extends ErrMsg(425, "file open error: %S")
  case ErrMsg428 extends ErrMsg(428, "syntax error")
  case ErrMsg430 extends ErrMsg(430, "#invalid argument (%S): %S")
  case ErrMsg431 extends ErrMsg(431, "#ERRFMT equation: wrong syntax (position %d)")
  case ErrMsg439 extends ErrMsg(439, "external command fault %d:\n%S")
  case ErrMsg441 extends ErrMsg(441, "#file \"%S\" (line %d.%02d): %S")
  case ErrMsg442 extends ErrMsg(442, "incompatible types")
  case ErrMsg443 extends ErrMsg(443, "undeclared identifier")
  case ErrMsg445 extends ErrMsg(445, "#file \"%S\": read error")
  case ErrMsg447 extends ErrMsg(447, "could not execute external command:\n%S")
  case ErrMsg448 extends ErrMsg(448, "#file \"%S\": name conflict with class %S declared in the file")
  case ErrMsg450 extends ErrMsg(450, "compilation aborted: %s")
  case ErrMsg455 extends ErrMsg(455, "#class name \"%S\" may not contain backslashes (\"\\\").Use slashes (\"/\") instead.")
  case ErrMsg456 extends ErrMsg(456, "#class name \"%S\" may not contain dots. Use slashes (\"/\") instead.")
  case ErrMsg459 extends ErrMsg(459, "#option and equation settings should be put before !module and !batch \n declarations in project file: %S")
  case ErrMsg460 extends ErrMsg(460, "configuration error: class \"%S\" wasn't found in %S: check name/symfile/VCODE correctness")
  case ErrMsg461 extends ErrMsg(461, "#The lookup directory or file \"%S\" does not exist: \n   %S")
  case ErrMsg462 extends ErrMsg(462, "Main class \"%S\" has unresolved super class or interface. \nPlease complete your project or specify another main class.")
  case ErrMsg464 extends ErrMsg(464, "\"outputname\" equation should be specified when \"gendll\" option enabled")
  case ErrMsg466 extends ErrMsg(466, "#class name \"%S\" may not contain spaces.\nWARNING: Directives \"!module\" and \"!batch\" allow\nTHE ONLY file(directory) to be specified")
  case ErrMsg467 extends ErrMsg(467, "Package \"%S\" not found")
  case ErrMsg468 extends ErrMsg(468, "\"%s\" equation value is missing.")
  case ErrMsg469 extends ErrMsg(469, "\"main\" equation should be explicitly specified, for example\n\n         -main=com/myCompany/AppMain")
  case ErrMsg471 extends ErrMsg(471, "#project contains !push directive without !pop")
  case ErrMsg473 extends ErrMsg(473, "%S is not a project file so it can not be used in \"=p\" mode.")
  case ErrMsg474 extends ErrMsg(474, "%S is a project file. Please specify \"=p\" mode.")
  case ErrMsg475 extends ErrMsg(475, "#project contains !classpathentry directive without !end")
  case ErrMsg476 extends ErrMsg(476, "#can not create output directory \"%S\"")

  case ErrMsg477 extends ErrMsg(477, "#classloaderentry \"%S\" is defined in the project but was not identified as valid entry")
  case ErrMsg478 extends ErrMsg(478, "#classloaderentry \"%S\" not found: \"%S\" does not exist")
  case ErrMsg479 extends ErrMsg(479, "#classloaderentry \"%S\" not found:  classpath entry \"%S\" does not exit in host \"%S\"")
  case ErrMsg480 extends ErrMsg(480, "#classpath entry not found \"%S\"")

  case ErrMsg483 extends ErrMsg(483, "Incorrect -parallelism value: \"%S\".\nAcceptable values: \"nice\", \"greedy\", \"max\" or a positive number.")

  case ErrMsg487 extends ErrMsg(487, "Unsupported lookup directive found: \"%S\".")
  case ErrMsg489 extends ErrMsg(489, "Internal PDB error: cannot open zip archive \"%S\" for read.")
  case ErrMsg491 extends ErrMsg(491, "Internal PDB error: cannot find \"%S\" in \"%S\"")
  case ErrMsg492 extends ErrMsg(492, "Internal PDB error: cannot read \"%S\" from \"%S\"")
  case ErrMsg493 extends ErrMsg(493, "Internal PDB error: cannot write \"%S\" to \"%S\"\nPlease ensure that you possess sufficient priveledges\nand there is enough free space on your hard drive.")
  case ErrMsg496 extends ErrMsg(496, "It seems that you are already compiling project %S.\nPlease wait until compilation is finished.\n\nIf you are not compiling it, please remove\n\"%S\" directory and try again.")
  case ErrMsg498 extends ErrMsg(498, "It seems that you are already compiling something\nin the current directory.\nPlease wait until compilation is finished.\n\nIf you are not compiling anything,\nplease remove \"%S\" directory and try again.")

  case ErrMsg503 extends ErrMsg(503, "Cannot create %s \"%S\" for Project Data Base.\nPlease ensure that you possess sufficient priveledges\nand there is enough free space on your hard drive.")
  case ErrMsg504 extends ErrMsg(504, "Attempt to use inconsistent symfile \"%S\".\nPlease remove pdb files.")

  case ErrMsg510 extends ErrMsg(510, "Bad classloader type used in !classloaderentry directive: %S")
  case ErrMsg511 extends ErrMsg(511, "!classloaderentry directive cannot be used for this project.")

  case ErrMsg517 extends ErrMsg(517, "Unknown locale %S")

  case ErrMsg520 extends ErrMsg(520, "\"IncludeDetectedLocales\" option is not supported")
  case ErrMsg522 extends ErrMsg(522, "Unsupported section in .fus file")

  case ErrMsg524 extends ErrMsg(524, "Configuration error: the profile %S is invalid or corrupted and couldn't be opened.")
  case ErrMsg526 extends ErrMsg(526, "The library %S is invalid or corrupted.")
  case ErrMsg528 extends ErrMsg(528, "\"genlibrary\" option should be set when \"genprofilelibrary\" option enabled")

  case ErrMsg550 extends ErrMsg(550, "unknown compiler frontend \"%S\"")

  case ErrMsg599 extends ErrMsg(599, "Failed to clean cache. Unable to delete file \"%S\".")

  case ErrMsg603 extends ErrMsg(603, "Incorrect \"%S.usg\" file version")
  case ErrMsg607 extends ErrMsg(607, "Wrong value \"%S\" for the CLASSABSENCE equation.\nPermitted values of the CLASSABSENCE equation are ERR or HANDLE.")
  case ErrMsg613 extends ErrMsg(613, "Obsolete \"%S.usg\" file version.")

  case ErrMsg636 extends ErrMsg(636, "JIT cache compilation is not longer supported.")

  case ErrMsg640 extends ErrMsg(640, "Wrong value \"%S\" for the PACK equation.\nPermitted values of the PACK equation are NONE, NONCOMPILED, and ALL.")
  case ErrMsg642 extends ErrMsg(642, "Wrong value \"%S\" for the OPTIMIZE equation.\nPermitted values of the OPTIMIZE equation are AUTODETECT and ALL.")
  case ErrMsg643 extends ErrMsg(643, "Wrong value \"%S\" for the PROTECT equation.\nPermitted values of the PROTECT equation are NOMATTER and ALL.")

  case ErrMsg645 extends ErrMsg(645, "Wrong value \"%S\" for the APPTYPE equation.")
  case ErrMsg646 extends ErrMsg(646, "APPDIR equation is not specified")
  case ErrMsg648 extends ErrMsg(648, "Internal Error: file \"%S\" was not generated")

  case ErrMsg652 extends ErrMsg(652, "Application is not specified")
  case ErrMsg653 extends ErrMsg(653, "Too many application modules are specified: %S")

  case ErrMsg665 extends ErrMsg(665, "Wrong value \"%S\" for the COMPACTPROFILE equation.\nPermitted values of the COMPACTPROFILE equation are COMPACT1, COMPACT2, COMPACT3 and FULL.")

  case ErrMsg671 extends ErrMsg(671, "The following feature was deprecated and is removed in this release: %s")

  case ErrMsg680 extends ErrMsg(680, "Invalid JPROFILE equation specified:\n file \"%S\" is not found")
  case ErrMsg681 extends ErrMsg(681, "JPROFILEDIR equation overrides JPROFILE, but both specified")
  case ErrMsg683 extends ErrMsg(683, "PGO WARNING: method `%S` from jprof not found or became inconsistent.\nIt is recommended to recollect application profile.")
  case ErrMsg684 extends ErrMsg(684, "PGO ERROR: hot method `%S` from jprof not found or became inconsistent.\nIt might negatively affect performance, so please recollect application profile.")
  case ErrMsg685 extends ErrMsg(685, "JPROFILE or JPROFILEDIR equation must be specified with PGO option.")
  case ErrMsg686 extends ErrMsg(686, "Invalid jprof file: %S\n\nPlease delete existing jprof file and recollect application profile.")

  case ErrMsg689 extends ErrMsg(689, "Jprof file %s is empty.")

  // ------- x86 compiler warnings

  // ------- x86 compiler fatal errors
  case ErrMsg950 extends ErrMsg(950, "Unable to compile the project due to not enough memory.")
  case ErrMsg954 extends ErrMsg(954, "Cannot compile class file conflicting by name with profile class: %S")
  case ErrMsg957 extends ErrMsg(957, "#Compilation of %S ignored:\nsymfile \"%S\" already exists.")
  case ErrMsg959 extends ErrMsg(959, "Inconsistent compilation set detected when processing the following classes:\n\n%S.class\n%S.class\n\nIf you compile your classes into single EXE or DLL, remove .sym files and \nre-compile the project.\n\nFor multi-component applications (DLLs plus EXE), you have to specify \nnon-overlapping LOOKUPs for each component's project.")

  case ErrMsg980 extends ErrMsg(980, "AJ error: method \"%S\" is not found in class %S")
  case ErrMsg981 extends ErrMsg(981, "AJ error: %s: invalid value of 'declaringClassName' argument: \"%S\"")
  case ErrMsg991 extends ErrMsg(991, "AJ error: class %S is in environment, but its supertype %S is not")
  case ErrMsg994 extends ErrMsg(994, "AJ error: @StdCall methods with varargs are forbidden on Linux (incompatible with @CCall varargs): %S")
  case ErrMsg997 extends ErrMsg(997, "AJ error: class %S is AJ @Managed, but its supertype %S is not")

  // Cangjie errors
  case ErrMsg2001 extends ErrMsg(2001, "File \"%S\" not found")
  case ErrMsg2002 extends ErrMsg(2002, "Only files with \".bc\" extension are accepted: %S")
  case ErrMsg2003 extends ErrMsg(2003, "Main module was not found among input files")
  case ErrMsg2004 extends ErrMsg(2004, "multiple definition of \"main\": %S and %S")
  case ErrMsg2005 extends ErrMsg(2005, "-main equation is set to wrong main module: %S")
  case ErrMsg2006 extends ErrMsg(2006, "%s directive is not supported for Cangjie projects")

  // Cangjie bitcode errors
  case ErrMsg2501 extends ErrMsg(2501, "function %S has too many parameters")
}