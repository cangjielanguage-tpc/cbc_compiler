/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.assembler.cbc

import com.huawei.excelsior.jet.assembler.AsmType

import scala.annotation.nowarn

enum CbcTypeKind {
    case INVALID    // 0x00
    case VOID       // 0x01
    case U1         // 0x02
    case I8         // 0x03
    case U8         // 0x04
    case CHAR       // 0x05
    case I32        // 0x06
    case U32        // 0x07
    case F32        // 0x08
    case F64        // 0x09
    case I64        // 0x0a
    case U64        // 0x0b
    case NREF       // 0x0c aka non-nullable ref
    case REF        // 0x0d
    case REC        // 0x0e
    case I16        // 0x0f
    case U16        // 0x10
    case F16        // 0x11
    case IN         // 0x12 int native
    case UN         // 0x13 uint native
    case AS         // 0x14 array slice
    case VA         // 0x15 varray
    case TTI        // 0x16 ThisTypeInfo
}

object CbcTypeKind {
    @nowarn("msg=match may not be exhaustive")
    def apply(tpe: AsmType) = tpe match {
        case AsmType.I8  => CbcTypeKind.I8
        case AsmType.U8  => CbcTypeKind.U8
        case AsmType.I16 => CbcTypeKind.I16
        case AsmType.U16 => CbcTypeKind.U16
        case AsmType.I32 => CbcTypeKind.I32
        case AsmType.U32 => CbcTypeKind.U32
        case AsmType.I64 => CbcTypeKind.I64
        case AsmType.U64 => CbcTypeKind.U64
        case AsmType.F16 => CbcTypeKind.F16
        case AsmType.F32 => CbcTypeKind.F32
        case AsmType.F64 => CbcTypeKind.F64
        case AsmType.PTR => CbcTypeKind.U64
    }
}