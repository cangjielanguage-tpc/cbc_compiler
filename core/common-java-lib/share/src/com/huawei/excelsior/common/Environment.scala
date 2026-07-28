/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common

import xscala.properties.OS
import xscala.properties.Properties as Props
import java.io.IOException
import java.util.Properties
import scala.collection.mutable

/** Part of Utils class moved here in order to reduce Utils size and
  * simplify initialization chains - this class has very simple clinit and
  * can be called from everywhere, while
  * Utils loads native libraries and attempts to show messages upon failure.
  *
  * '''NOTICE'''
  *
  * This namespace relies on automatically generated [[JETConfig]] utility class.
  * If you encounter compilation errors (in IDEA or Ant) related to the absence of this file,
  * try executing the following commands from the command line:
  *
  * {{{
  *   ant clean-all # cleanup all build artifacts
  *   ant config    # provoke the code generation
  *                 #  (alternatively, you can execute any other Ant target
  *                 #   which builds `common-java-lib` as one of the steps)
  * }}}
  */
object Environment {

  /** The target operating system. */
  val TARGET_OS = OS(JETConfig.targetOS)

  /** The target CPU or bytecode architecture. */
  val TARGET_CPU_ARCH = Arch(JETConfig.targetCPU)

  /** The [[Mode]] ("work" s. "enduser") of the current JET framework build. */
  val MODE = Mode(JETConfig.buildMode)

  /** The [[LanguagePack]] selected for the current JET framework build. */
  val LANGUAGE_PACK = LanguagePack(JETConfig.languagePack)

  /** The host [[OS]] for the current JET framework build. */
  val HOST_OS: OS = OS.host

  /** The host CPU [[Arch]] for the current JET framework build. */
  val HOST_CPU_ARCH: Arch = Props.get.arch() match {
    case "amd64" => Arch.AMD64
    case "aarch64" => Arch.ARM64
    case x => throw new IllegalArgumentException(s"Unknown architecture: '$x'")
  }

  /** Flag indicating whether jc should run independently of JET toolchain. */
  val JC_STANDALONE = JETConfig.jcStandalone

  // Version could also be set by properties but it is inconvenient for
  // JET developers to remember to change this value on every JET version change.

  /** The current [[JetVersion]]. */
  val JET_VERSION = JetVersion.fromVersionCode(1530)
}
