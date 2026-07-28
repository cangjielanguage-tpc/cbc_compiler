/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.time

/** Current Unix time, measured in milliseconds. */
def unixMilliseconds: Long = TimeVMDependent.get.nowMilliseconds()

/** Current Unix time, measured in nanoseconds, from the clock source with the best available resolution. */
def unixNanoseconds: Long = TimeVMDependent.get.nowNanoseconds()
