/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.io

import xscala.text.{Encoding, Utf8Encoding}

// TODO: specify proper PlatformEncoding as Encoding for each stream
private val defaultEncoding: Encoding = Utf8Encoding

lazy val stdin = TextInput.wrapHandle(InputStreamVMDependent.get.getStdin(), defaultEncoding, close = false)

lazy val stdout = TextOutput.wrapHandle(OutputStreamVMDependent.get.getStdout(), defaultEncoding, close = false)

lazy val stderr = TextOutput.wrapHandle(OutputStreamVMDependent.get.getStderr(), defaultEncoding, close = false)
