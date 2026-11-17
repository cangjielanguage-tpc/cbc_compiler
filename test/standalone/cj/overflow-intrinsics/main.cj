import std.overflow.*

func runBin<A, B, R>(prefix: String, cases: Array<(String, A, B)>, op: (A, B) -> R) where R <: ToString {
    for (c in cases) {
        let (label, a, b) = c
        println("${prefix} ${label} ${op(a, b)}")
    }
}

func runUn<A, R>(prefix: String, cases: Array<(String, A)>, op: (A) -> R) where R <: ToString {
    for (c in cases) {
        let (label, a) = c
        println("${prefix} ${label} ${op(a)}")
    }
}

func throwBin<A, B, R>(cases: Array<(String, A, B, Bool)>, op: (A, B) -> R) where R <: ToString {
    for (c in cases) {
        let (label, a, b, ov) = c
        if (ov) {
            try {
                op(a, b)
                println("throw ${label} fail")
            } catch (e: OverflowException) {
                println("throw ${label} pass")
            }
        } else {
            println("throw ${label} ${op(a, b)}")
        }
    }
}

func throwUn<A, R>(cases: Array<(String, A, Bool)>, op: (A) -> R) where R <: ToString {
    for (c in cases) {
        let (label, a, ov) = c
        if (ov) {
            try {
                op(a)
                println("throw ${label} fail")
            } catch (e: OverflowException) {
                println("throw ${label} pass")
            }
        } else {
            println("throw ${label} ${op(a)}")
        }
    }
}

func chkBin<A, B, R>(cases: Array<(String, A, B)>, op: (A, B) -> ?R) where R <: ToString {
    for (c in cases) {
        let (label, a, b) = c
        print("chk ${label} ")
        match (op(a, b)) {
            case Some(v) => println("Some(${v})")
            case None => println("None")
        }
    }
}

func chkUn<A, R>(cases: Array<(String, A)>, op: (A) -> ?R) where R <: ToString {
    for (c in cases) {
        let (label, a) = c
        print("chk ${label} ")
        match (op(a)) {
            case Some(v) => println("Some(${v})")
            case None => println("None")
        }
    }
}

func div0<A, B, R>(label: String, a: A, b: B, op: (A, B) -> R) {
    try {
        op(a, b)
        println("${label} fail")
    } catch (e: ArithmeticException) {
        println("${label} pass")
    }
}

func wrappingTests(i8: Array<Int8>, i64: Array<Int64>, u8: Array<UInt8>, u64: Array<UInt64>, exp: Array<UInt64>) {
    runBin("wrap", [
        ("add i8", i8[0], i8[1]),
        ("add i8 ov", i8[2], i8[3])
    ], { a: Int8, b: Int8 => a.wrappingAdd(b) })
    runBin("wrap", [
        ("sub i8", i8[4], i8[5]),
        ("sub i8 ov", i8[6], i8[3])
    ], { a: Int8, b: Int8 => a.wrappingSub(b) })
    runBin("wrap", [
        ("mul i8", i8[7], i8[8]),
        ("mul i8 ov", i8[9], i8[9])
    ], { a: Int8, b: Int8 => a.wrappingMul(b) })
    runBin("wrap", [("div i8", i8[6], i8[10])], { a: Int8, b: Int8 => a.wrappingDiv(b) })
    runBin("wrap", [("mod i8", i8[6], i8[10])], { a: Int8, b: Int8 => a.wrappingMod(b) })
    runUn("wrap", [("inc i8", i8[2])], { a: Int8 => a.wrappingInc() })
    runUn("wrap", [("dec i8", i8[6])], { a: Int8 => a.wrappingDec() })
    runUn("wrap", [("neg i8", i8[6])], { a: Int8 => a.wrappingNeg() })

    runBin("wrap", [
        ("add i64", i64[0], i64[1]),
        ("add i64 ov", i64[2], i64[3])
    ], { a: Int64, b: Int64 => a.wrappingAdd(b) })
    runBin("wrap", [("sub i64", i64[4], i64[5])], { a: Int64, b: Int64 => a.wrappingSub(b) })
    runBin("wrap", [("mul i64", i64[6], i64[7])], { a: Int64, b: Int64 => a.wrappingMul(b) })
    runBin("wrap", [("div i64", i64[8], i64[9])], { a: Int64, b: Int64 => a.wrappingDiv(b) })
    runBin("wrap", [("mod i64", i64[8], i64[9])], { a: Int64, b: Int64 => a.wrappingMod(b) })
    runUn("wrap", [("inc i64", i64[2])], { a: Int64 => a.wrappingInc() })
    runUn("wrap", [("dec i64", i64[8])], { a: Int64 => a.wrappingDec() })
    runUn("wrap", [("neg i64", i64[8])], { a: Int64 => a.wrappingNeg() })
    runBin("wrap", [
        ("pow", i64[10], exp[0]),
        ("pow ov", i64[12], exp[1])
    ], { a: Int64, b: UInt64 => a.wrappingPow(b) })

    runBin("wrap", [
        ("add u8", u8[0], u8[1]),
        ("add u8 ov", u8[2], u8[3])
    ], { a: UInt8, b: UInt8 => a.wrappingAdd(b) })
    runBin("wrap", [("sub u8 ov", u8[4], u8[3])], { a: UInt8, b: UInt8 => a.wrappingSub(b) })
    runBin("wrap", [("mul u8 ov", u8[5], u8[5])], { a: UInt8, b: UInt8 => a.wrappingMul(b) })
    runBin("wrap", [("div u8", u8[6], u8[7])], { a: UInt8, b: UInt8 => a.wrappingDiv(b) })
    runBin("wrap", [("mod u8", u8[6], u8[7])], { a: UInt8, b: UInt8 => a.wrappingMod(b) })
    runUn("wrap", [("inc u8", u8[2])], { a: UInt8 => a.wrappingInc() })
    runUn("wrap", [("dec u8", u8[4])], { a: UInt8 => a.wrappingDec() })
    runUn("wrap", [
        ("neg u8", u8[3]),
        ("neg u8z", u8[4])
    ], { a: UInt8 => a.wrappingNeg() })

    runBin("wrap", [
        ("add u64", u64[0], u64[1]),
        ("add u64 ov", u64[2], u64[3])
    ], { a: UInt64, b: UInt64 => a.wrappingAdd(b) })
    runBin("wrap", [("sub u64 ov", u64[4], u64[3])], { a: UInt64, b: UInt64 => a.wrappingSub(b) })
    runBin("wrap", [("mul u64", u64[5], u64[6])], { a: UInt64, b: UInt64 => a.wrappingMul(b) })
    runUn("wrap", [("inc u64", u64[2])], { a: UInt64 => a.wrappingInc() })
    runUn("wrap", [("dec u64", u64[4])], { a: UInt64 => a.wrappingDec() })
    runUn("wrap", [("neg u64", u64[3])], { a: UInt64 => a.wrappingNeg() })
}

func throwingTests(i8: Array<Int8>, i64: Array<Int64>, u8: Array<UInt8>, u64: Array<UInt64>, exp: Array<UInt64>) {
    throwBin([
        ("add i8", i8[0], i8[1], false),
        ("add i8", i8[2], i8[3], true)
    ], { a: Int8, b: Int8 => a.throwingAdd(b) })
    throwBin([
        ("sub i8", i8[4], i8[5], false),
        ("sub i8", i8[6], i8[3], true)
    ], { a: Int8, b: Int8 => a.throwingSub(b) })
    throwBin([
        ("mul i8", i8[7], i8[8], false),
        ("mul i8", i8[9], i8[9], true)
    ], { a: Int8, b: Int8 => a.throwingMul(b) })
    throwBin([("div i8", i8[6], i8[10], true)], { a: Int8, b: Int8 => a.throwingDiv(b) })
    throwBin([("mod i8", i8[6], i8[10], true)], { a: Int8, b: Int8 => a.throwingMod(b) })
    throwUn([("inc i8", i8[2], true)], { a: Int8 => a.throwingInc() })
    throwUn([("dec i8", i8[6], true)], { a: Int8 => a.throwingDec() })
    throwUn([("neg i8", i8[6], true)], { a: Int8 => a.throwingNeg() })

    throwBin([
        ("add i64", i64[0], i64[1], false),
        ("add i64", i64[2], i64[3], true)
    ], { a: Int64, b: Int64 => a.throwingAdd(b) })
    throwBin([("sub i64", i64[4], i64[5], false)], { a: Int64, b: Int64 => a.throwingSub(b) })
    throwBin([("mul i64", i64[6], i64[7], false)], { a: Int64, b: Int64 => a.throwingMul(b) })
    throwBin([("div i64", i64[8], i64[9], true)], { a: Int64, b: Int64 => a.throwingDiv(b) })
    throwBin([("mod i64", i64[8], i64[9], true)], { a: Int64, b: Int64 => a.throwingMod(b) })
    throwUn([("inc i64", i64[2], true)], { a: Int64 => a.throwingInc() })
    throwUn([("dec i64", i64[8], true)], { a: Int64 => a.throwingDec() })
    throwUn([("neg i64", i64[8], true)], { a: Int64 => a.throwingNeg() })
    throwBin([
        ("pow", i64[10], exp[0], false),
        ("pow", i64[12], exp[1], true)
    ], { a: Int64, b: UInt64 => a.throwingPow(b) })

    throwBin([
        ("add u8", u8[0], u8[1], false),
        ("add u8", u8[2], u8[3], true)
    ], { a: UInt8, b: UInt8 => a.throwingAdd(b) })
    throwBin([("sub u8", u8[4], u8[3], true)], { a: UInt8, b: UInt8 => a.throwingSub(b) })
    throwBin([("mul u8", u8[5], u8[5], true)], { a: UInt8, b: UInt8 => a.throwingMul(b) })
    throwBin([("div u8", u8[6], u8[7], false)], { a: UInt8, b: UInt8 => a.throwingDiv(b) })
    throwBin([("mod u8", u8[6], u8[7], false)], { a: UInt8, b: UInt8 => a.throwingMod(b) })
    throwUn([("inc u8", u8[2], true)], { a: UInt8 => a.throwingInc() })
    throwUn([("dec u8", u8[4], true)], { a: UInt8 => a.throwingDec() })
    throwUn([
        ("neg u8", u8[3], true),
        ("neg u8z", u8[4], false)
    ], { a: UInt8 => a.throwingNeg() })

    throwBin([
        ("add u64", u64[0], u64[1], false),
        ("add u64", u64[2], u64[3], true)
    ], { a: UInt64, b: UInt64 => a.throwingAdd(b) })
    throwBin([("sub u64", u64[4], u64[3], true)], { a: UInt64, b: UInt64 => a.throwingSub(b) })
    throwBin([("mul u64", u64[5], u64[6], false)], { a: UInt64, b: UInt64 => a.throwingMul(b) })
    throwUn([("inc u64", u64[2], true)], { a: UInt64 => a.throwingInc() })
    throwUn([("dec u64", u64[4], true)], { a: UInt64 => a.throwingDec() })
    throwUn([("neg u64", u64[3], true)], { a: UInt64 => a.throwingNeg() })
}

func saturatingTests(i8: Array<Int8>, i64: Array<Int64>, u8: Array<UInt8>, u64: Array<UInt64>, exp: Array<UInt64>) {
    runBin("sat", [("add i8", i8[2], i8[4])], { a: Int8, b: Int8 => a.saturatingAdd(b) })
    runBin("sat", [("sub i8", i8[6], i8[4])], { a: Int8, b: Int8 => a.saturatingSub(b) })
    runBin("sat", [("mul i8", i8[6], i8[10])], { a: Int8, b: Int8 => a.saturatingMul(b) })
    runBin("sat", [("div i8", i8[6], i8[10])], { a: Int8, b: Int8 => a.saturatingDiv(b) })
    runBin("sat", [("mod i8", i8[6], i8[10])], { a: Int8, b: Int8 => a.saturatingMod(b) })
    runUn("sat", [("inc i8", i8[2])], { a: Int8 => a.saturatingInc() })
    runUn("sat", [("dec i8", i8[6])], { a: Int8 => a.saturatingDec() })
    runUn("sat", [("neg i8", i8[6])], { a: Int8 => a.saturatingNeg() })
    runBin("sat", [("add i8 ok", i8[13], i8[14])], { a: Int8, b: Int8 => a.saturatingAdd(b) })

    runBin("sat", [("add i64", i64[2], i64[1])], { a: Int64, b: Int64 => a.saturatingAdd(b) })
    runBin("sat", [("sub i64", i64[8], i64[1])], { a: Int64, b: Int64 => a.saturatingSub(b) })
    runBin("sat", [("mul i64", i64[8], i64[9])], { a: Int64, b: Int64 => a.saturatingMul(b) })
    runBin("sat", [("div i64", i64[8], i64[9])], { a: Int64, b: Int64 => a.saturatingDiv(b) })
    runUn("sat", [("inc i64", i64[2])], { a: Int64 => a.saturatingInc() })
    runUn("sat", [("dec i64", i64[8])], { a: Int64 => a.saturatingDec() })
    runUn("sat", [("neg i64", i64[8])], { a: Int64 => a.saturatingNeg() })
    runBin("sat", [
        ("pow", i64[10], exp[0]),
        ("pow ov", i64[12], exp[1])
    ], { a: Int64, b: UInt64 => a.saturatingPow(b) })

    runBin("sat", [("add u8", u8[2], u8[3])], { a: UInt8, b: UInt8 => a.saturatingAdd(b) })
    runBin("sat", [("sub u8", u8[4], u8[3])], { a: UInt8, b: UInt8 => a.saturatingSub(b) })
    runBin("sat", [("mul u8", u8[2], u8[8])], { a: UInt8, b: UInt8 => a.saturatingMul(b) })
    runUn("sat", [("inc u8", u8[2])], { a: UInt8 => a.saturatingInc() })
    runUn("sat", [("dec u8", u8[4])], { a: UInt8 => a.saturatingDec() })
    runUn("sat", [("neg u8", u8[3])], { a: UInt8 => a.saturatingNeg() })

    runBin("sat", [("add u64", u64[2], u64[3])], { a: UInt64, b: UInt64 => a.saturatingAdd(b) })
    runBin("sat", [("sub u64", u64[4], u64[3])], { a: UInt64, b: UInt64 => a.saturatingSub(b) })
    runUn("sat", [("inc u64", u64[2])], { a: UInt64 => a.saturatingInc() })
    runUn("sat", [("dec u64", u64[4])], { a: UInt64 => a.saturatingDec() })
    runUn("sat", [("neg u64", u64[3])], { a: UInt64 => a.saturatingNeg() })
}

func checkedTests(i8: Array<Int8>, i64: Array<Int64>, u8: Array<UInt8>, u64: Array<UInt64>, exp: Array<UInt64>) {
    chkBin([
        ("add i8", i8[0], i8[1]),
        ("add i8 ov", i8[2], i8[3])
    ], { a: Int8, b: Int8 => a.checkedAdd(b) })
    chkBin([
        ("sub i8", i8[4], i8[5]),
        ("sub i8 ov", i8[6], i8[3])
    ], { a: Int8, b: Int8 => a.checkedSub(b) })
    chkBin([
        ("mul i8", i8[7], i8[8]),
        ("mul i8 ov", i8[9], i8[9])
    ], { a: Int8, b: Int8 => a.checkedMul(b) })
    chkBin([
        ("div i8", i8[15], i8[5]),
        ("div i8 ov", i8[6], i8[10])
    ], { a: Int8, b: Int8 => a.checkedDiv(b) })
    chkBin([
        ("mod i8", i8[15], i8[5]),
        ("mod i8 ov", i8[6], i8[10])
    ], { a: Int8, b: Int8 => a.checkedMod(b) })
    chkUn([
        ("inc i8", i8[11]),
        ("inc i8 ov", i8[2])
    ], { a: Int8 => a.checkedInc() })
    chkUn([
        ("dec i8", i8[11]),
        ("dec i8 ov", i8[6])
    ], { a: Int8 => a.checkedDec() })
    chkUn([
        ("neg i8", i8[12]),
        ("neg i8 ov", i8[6])
    ], { a: Int8 => a.checkedNeg() })

    chkBin([
        ("add i64", i64[0], i64[1]),
        ("add i64 ov", i64[2], i64[3])
    ], { a: Int64, b: Int64 => a.checkedAdd(b) })
    chkBin([("sub i64", i64[4], i64[5])], { a: Int64, b: Int64 => a.checkedSub(b) })
    chkBin([("mul i64", i64[6], i64[7])], { a: Int64, b: Int64 => a.checkedMul(b) })
    chkBin([("div i64 ov", i64[8], i64[9])], { a: Int64, b: Int64 => a.checkedDiv(b) })
    chkBin([("mod i64 ov", i64[8], i64[9])], { a: Int64, b: Int64 => a.checkedMod(b) })
    chkUn([("inc i64 ov", i64[2])], { a: Int64 => a.checkedInc() })
    chkUn([("dec i64 ov", i64[8])], { a: Int64 => a.checkedDec() })
    chkUn([("neg i64 ov", i64[8])], { a: Int64 => a.checkedNeg() })
    chkBin([
        ("pow", i64[10], exp[0]),
        ("pow0", i64[11], exp[2]),
        ("pow ov", i64[12], exp[1])
    ], { a: Int64, b: UInt64 => a.checkedPow(b) })

    chkBin([
        ("add u8", u8[0], u8[1]),
        ("add u8 ov", u8[2], u8[3])
    ], { a: UInt8, b: UInt8 => a.checkedAdd(b) })
    chkBin([("sub u8 ov", u8[4], u8[3])], { a: UInt8, b: UInt8 => a.checkedSub(b) })
    chkBin([("mul u8 ov", u8[5], u8[5])], { a: UInt8, b: UInt8 => a.checkedMul(b) })
    chkBin([("div u8", u8[6], u8[7])], { a: UInt8, b: UInt8 => a.checkedDiv(b) })
    chkBin([("mod u8", u8[6], u8[7])], { a: UInt8, b: UInt8 => a.checkedMod(b) })
    chkUn([("inc u8 ov", u8[2])], { a: UInt8 => a.checkedInc() })
    chkUn([("dec u8 ov", u8[4])], { a: UInt8 => a.checkedDec() })
    chkUn([
        ("neg u8 ov", u8[3]),
        ("neg u8z", u8[4])
    ], { a: UInt8 => a.checkedNeg() })

    chkBin([
        ("add u64", u64[0], u64[1]),
        ("add u64 ov", u64[2], u64[3])
    ], { a: UInt64, b: UInt64 => a.checkedAdd(b) })
    chkBin([("sub u64 ov", u64[4], u64[3])], { a: UInt64, b: UInt64 => a.checkedSub(b) })
    chkBin([("mul u64", u64[5], u64[6])], { a: UInt64, b: UInt64 => a.checkedMul(b) })
    chkUn([("inc u64 ov", u64[2])], { a: UInt64 => a.checkedInc() })
    chkUn([("dec u64 ov", u64[4])], { a: UInt64 => a.checkedDec() })
    chkUn([("neg u64 ov", u64[3])], { a: UInt64 => a.checkedNeg() })
}

func divZeroTests(i8: Array<Int8>, u8: Array<UInt8>) {
    div0("wrap div0", i8[3], i8[11], { a: Int8, b: Int8 => a.wrappingDiv(b) })
    div0("throw div0", i8[3], i8[11], { a: Int8, b: Int8 => a.throwingDiv(b) })
    div0("sat div0", i8[3], i8[11], { a: Int8, b: Int8 => a.saturatingDiv(b) })
    div0("chk div0", i8[3], i8[11], { a: Int8, b: Int8 => a.checkedDiv(b) })
    div0("wrap mod0", u8[3], u8[4], { a: UInt8, b: UInt8 => a.wrappingMod(b) })
    div0("throw mod0", u8[3], u8[4], { a: UInt8, b: UInt8 => a.throwingMod(b) })
    div0("sat mod0", u8[3], u8[4], { a: UInt8, b: UInt8 => a.saturatingMod(b) })
    div0("chk mod0", u8[3], u8[4], { a: UInt8, b: UInt8 => a.checkedMod(b) })
}

main(): Int64 {
    let i8: Array<Int8> = [
        Int8(100), Int8(20), Int8.Max, Int8(1), Int8(10), Int8(3),
        Int8.Min, Int8(6), Int8(7), Int8(16), Int8(-1), Int8(0),
        Int8(2), Int8(-10), Int8(8), Int8(20)
    ]
    let i64: Array<Int64> = [
        Int64(7), Int64(20), Int64.Max, Int64(1), Int64(-10), Int64(5),
        Int64(78), Int64(456), Int64.Min, Int64(-1), Int64(3), Int64(9),
        Int64(2)
    ]
    let u8: Array<UInt8> = [
        UInt8(200), UInt8(50), UInt8.Max, UInt8(1), UInt8(0),
        UInt8(16), UInt8(20), UInt8(3), UInt8(2)
    ]
    let u64: Array<UInt64> = [
        UInt64(7), UInt64(20), UInt64.Max, UInt64(1), UInt64(0),
        UInt64(32), UInt64(10)
    ]
    let exp: Array<UInt64> = [UInt64(5), UInt64(63), UInt64(0)]
    wrappingTests(i8, i64, u8, u64, exp)
    throwingTests(i8, i64, u8, u64, exp)
    saturatingTests(i8, i64, u8, u64, exp)
    checkedTests(i8, i64, u8, u64, exp)
    divZeroTests(i8, u8)
    0
}
