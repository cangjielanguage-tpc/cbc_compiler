/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

/** AJ specific call kind of some method.
  *
  * @author cypok
  */
enum MethodAJCallKind {
  case
    NORMAL,
    INTRINSIC_CALL,
    INTRINSIC_WITH_BODY_CALL,
    INDIRECT_CALL,
    CALL_TO_MANAGED,
    UNCHECKED_CALL,
    UNCHECKED_NEW,
    THIN_UNCHECKED_CAST,
    GET_FLAT_THIN_INTRINSIC
}
