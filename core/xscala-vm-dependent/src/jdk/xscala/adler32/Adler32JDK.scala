/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.adler32

object Adler32JDK {
  private lazy val ensureInited = doInit()
  private def doInit(): Boolean = { System.loadLibrary("xminizip"); true }
}

final class Adler32JDK extends Adler32VMDependent {

  override private[adler32] def init(): Unit = Adler32JDK.ensureInited

  @native def adler32Update(adler: Int, bytes: Array[Byte], len: Int): Int
}
