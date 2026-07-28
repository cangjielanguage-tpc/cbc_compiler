/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.common

import com.huawei.excelsior.dotty.annot.javaFriendly

/** Kinds of references used for deferred access. */
@javaFriendly
enum DAIRefKind {
  case
    UNDEFINED,

    /** REF_getField */
    GET_FIELD,

    /** REF_getStatic */
    GET_STATIC,

    /** REF_putField */
    PUT_FIELD,

    /** REF_putStatic */
    PUT_STATIC,

    /** REF_invokeVirtual */
    INVOKE_VIRTUAL,

    /** REF_invokeStatic */
    INVOKE_STATIC,

    /** REF_invokeSpecial */
    INVOKE_SPECIAL,

    /** REF_newInvokeSpecial */
    NEW_INVOKE_SPECIAL,

    /** REF_invokeInterface */
    INVOKE_INTERFACE,

    INVOKE_DYNAMIC,
    INVOKE_SIGPOLY,

    /** Operation on a CPEntry which doesn't require indirect invocation of resolving procedure.
      * Covers following operations:
      * {{{
      *   ldc CO/MT/MH
      *   new CO
      *   checkcast CO
      *   instanceof CO
      * }}}
      * where `CO/MT/MH` denote CPEntries with reference to a ClassObject/MethodType/MethodHandle respectively.
      */
    DIRECT_CP_ENTRY_OPERATION
}
