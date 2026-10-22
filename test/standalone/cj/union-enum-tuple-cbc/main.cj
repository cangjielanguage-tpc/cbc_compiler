/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2025. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
// Regression test for bug #161: union enum <-> tuple representation mismatch.
// CJNative packs union enums (tag u32 at +0, payload at +4, unaligned), while
// plain tuples are naturally aligned. All uses of the enum must go through the
// packed representation, otherwise values are read shifted by 32 bits.

enum MySeek {
    Begin(Int64) | Current(Int64) | End(Int64)
}

func probeTuple(t: (UInt32, Int64)): Int64 {
    let (tag, payload) = t
    println("[same pkg] tuple: tag = ${tag}, payload = ${payload}")
    return payload
}

func probeEnum(e: MySeek): Int64 {
    let r = match (e) {
        case Begin(v) => v
        case Current(v) => v
        case End(v) => v
    }
    println("[same pkg] enum payload = ${r}")
    return r
}

func probeEnumPair(p: (MySeek, MySeek)): Int64 {
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
    println("[same pkg] enum pair: first = ${r1}, second = ${r2}")
    return r1 * 100 + r2
}

main(): Int64 {
    let t: (UInt32, Int64) = (0, 5)
    let r = probeTuple(t)
    if (r != 5) { println("FAIL tuple") }

    let e = MySeek.Current(2)
    let p = probeEnum(e)
    if (p != 2) { println("FAIL enum") }

    let pair: (MySeek, MySeek) = (MySeek.Current(3), MySeek.Begin(4))
    let q = probeEnumPair(pair)
    if (q == 304) { println("PASS") } else { println("FAIL pair (expected 304, got ${q})") }
    return 0
}
