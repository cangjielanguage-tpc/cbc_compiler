/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.text

abstract class EncodingException(message: String = null, cause: Throwable = null) extends RuntimeException(message, cause)

final class EncodingMalformedException(message: String) extends EncodingException(message)

final class EncodingUnmappableException(message: String) extends EncodingException(message)

final class EncodingOverflowException(message: String) extends EncodingException(message)

final class EncodingUnderflowException(message: String) extends EncodingException(message)
