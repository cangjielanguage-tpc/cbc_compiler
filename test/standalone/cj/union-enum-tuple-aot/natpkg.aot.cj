/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2025. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
// Regression test for bug #161: union enum <-> tuple representation mismatch
// at the CBC/native boundary. The enum is constructed in CBC-compiled code and
// passed to natively compiled functions; CJNative packs union enums (tag u32
// at +0, payload at +4, unaligned), while plain tuples are naturally aligned.

package natpkg

public enum MySeek {
    Begin(Int64) | Current(Int64) | End(Int64)
}

public func probeTuple(t: (UInt32, Int64)): Int64 {
    let (tag, payload) = t
    println("[natpkg] tuple: tag = ${tag}, payload = ${payload}")
    return payload
}

public func probeEnum(e: MySeek): Int64 {
    let r = match (e) {
        case Begin(v) => v
        case Current(v) => v
        case End(v) => v
    }
    println("[natpkg] enum payload = ${r}")
    return r
}

public func probeEnumPair(p: (MySeek, MySeek)): Int64 {
    let (e1, e2) = p
    let r1 = match (e1) {
        case Begin(v) => v
        case Current(v) => v
        case End(v) => v
    }
    let r2 = match (e2) {
        case Begin(v) => v
        case Current(v) => v
        case End(v) => v
    }
    println("[natpkg] enum pair: first = ${r1}, second = ${r2}")
    return r1 * 100 + r2
}
