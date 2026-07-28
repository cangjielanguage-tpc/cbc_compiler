/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.common

import com.huawei.excelsior.dotty.annot.javaFriendly

/** Built-in Java fields.
  *
  * @author alexm
  */
@javaFriendly
enum BuiltInField {
  case METHOD_HANDLE_FORM       // field java.lang.invoke.MethodHandle.form
  case LAMBDA_FORM_VMENTRY      // field java.lang.invoke.LambdaForm.vmentry
  case MEMBER_NAME_ENTRY_POINT  // field java.lang.invoke.MemberName.entryPoint (JET-specific)
}
