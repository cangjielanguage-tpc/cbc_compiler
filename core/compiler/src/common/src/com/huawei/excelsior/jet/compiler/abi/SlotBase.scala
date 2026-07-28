/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.abi

/** This class should be inner of [[Frame.Slot]] but aj-javac failed trying to access it. See JET-14219 for more details. */
enum SlotBase:
  case SP, FMR, TR
