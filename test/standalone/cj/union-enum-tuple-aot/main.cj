/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2025. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
// Regression test for bug #161: union enum <-> tuple representation mismatch
// at the CBC/native boundary. See natpkg.aot.cj for details.

import natpkg.*

main(): Int64 {
    let t: (UInt32, Int64) = (0, 5)
    let r = probeTuple(t)
    println("main got tuple payload = ${r}")
    if (r != 5) { println("FAIL tuple") }

    let e = MySeek.Current(2)
    let p = probeEnum(e)
    println("main got enum payload = ${p}")
    if (p != 2) { println("FAIL enum (expected 2; ${p} = 2<<32?)") }

    let pair: (MySeek, MySeek) = (MySeek.Current(3), MySeek.Begin(4))
    let q = probeEnumPair(pair)
    println("main got pair result = ${q}")
    if (q == 304) { println("PASS") } else { println("FAIL pair (expected 304, got ${q})") }
    return 0
}
