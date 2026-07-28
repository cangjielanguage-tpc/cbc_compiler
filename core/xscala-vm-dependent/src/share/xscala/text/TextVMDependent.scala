/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

import xscala.vm.VMDependent

trait TextVMDependent {
  def setLocale(category: PlatformEncoding.LocaleCategory, locale: String): String
  def nativeEncoding(): Encoding
  def stdInEncoding(): Encoding
  def stdOutEncoding(): Encoding
  def stdErrEncoding(): Encoding
}

object TextVMDependent extends VMDependent[TextVMDependent]
