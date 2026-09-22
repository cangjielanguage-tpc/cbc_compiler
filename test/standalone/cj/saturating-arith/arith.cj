func check(errMsg: String, res: Bool): Int64 {
    if (!res) {
        println(errMsg)
        return 1
    }
    return 0
}

// Saturating Int8 operations
@OverflowSaturating
func satAddint8(a: Int8, b: Int8): Int8 {
    a + b
}

@OverflowSaturating
func satSubint8(a: Int8, b: Int8): Int8 {
    a - b
}

@OverflowSaturating
func satMulint8(a: Int8, b: Int8): Int8 {
    a * b
}

@OverflowSaturating
func satDivint8(a: Int8, b: Int8): Int8 {
    a / b
}

@OverflowSaturating
func satModint8(a: Int8, b: Int8): Int8 {
    a % b
}

@OverflowSaturating
func satIncint8(a: Int8): Int8 {
    a + 1
}

@OverflowSaturating
func satDecint8(a: Int8): Int8 {
    a - 1
}

@OverflowSaturating
func satNegint8(a: Int8): Int8 {
    -a
}

func int8Test(): Int64 {
    var failCount: Int64 = 0

    let max: Int8 = Int8.Max
    let min: Int8 = Int8.Min

    failCount += check("[FAIL] Int8: -10 + 8 != -2", satAddint8(-10, 8) == -2) 
    failCount += check("[FAIL] Int8: 10 - 8 != 2", satSubint8(10, 8) == 2)
    failCount += check("[FAIL] Int8: 2 * 8 != 16", satMulint8(2, 8) == 16)
    failCount += check("[FAIL] Int8: 10 / 6 != 1", satDivint8(10, 6) == 1)
    failCount += check("[FAIL] Int8: 10 % 6 != 4", satModint8(10, 6) == 4)
    failCount += check("[FAIL] Int8: inc 0 != 1", satIncint8(0) == 1)
    failCount += check("[FAIL] Int8: dec 0 != -1", satDecint8(0) == -1)
    failCount += check("[FAIL] Int8: -(2) != -2", satNegint8(2) == -2)
    failCount += check("[FAIL] Int8: -(0) != 0", satNegint8(0) == 0)

    failCount += check("[FAIL] Int8: MAX + 10 != MAX", satAddint8(max, 10) == max)
    failCount += check("[FAIL] Int8: MIN + (-10) != MIN", satAddint8(min, -10) == min)
    failCount += check("[FAIL] Int8: (MAX - 9) + 10 != MAX", satAddint8(max - 9, 10) == max)
    failCount += check("[FAIL] Int8: (MIN + 9) + (-10) != MIN", satAddint8(min + 9, -10) == min)
    failCount += check("[FAIL] Int8: (MAX - 11) + 10 != MAX - 1", satAddint8(max - 11, 10) == max - 1)
    failCount += check("[FAIL] Int8: (MIN + 11) + (-10) != MIN + 1", satAddint8(min + 11, -10) == min + 1)

    failCount += check("[FAIL] Int8: MAX - (-10) != MAX", satSubint8(max, -10) == max)
    failCount += check("[FAIL] Int8: MIN - 10 != MIN", satSubint8(min, 10) == min)
    failCount += check("[FAIL] Int8: (MAX - 9) - (-10) != MAX", satSubint8(max - 9, -10) == max)
    failCount += check("[FAIL] Int8: (MIN + 9) - 10 != MIN", satSubint8(min + 9, 10) == min)
    failCount += check("[FAIL] Int8: (MAX - 11) - (-10) != MAX - 1", satSubint8(max - 11, -10) == max - 1)
    failCount += check("[FAIL] Int8: (MIN + 11) - 10 != MIN + 1", satSubint8(min + 11, 10) == min + 1)

    failCount += check("[FAIL] Int8: (MAX / 2) * (-4) != MIN", satMulint8(max / 2, -4) == min)
    failCount += check("[FAIL] Int8: MAX * (-1) != MIN + 1", satMulint8(max, -1) == min + 1)
    failCount += check("[FAIL] Int8: (-1) * MAX != MIN + 1", satMulint8(-1, max) == min + 1)
    failCount += check("[FAIL] Int8: MAX saturating.* (-1) != MAX * (-1)", satMulint8(max, -1) == max * -1)
    failCount += check("[FAIL] Int8: MIN * (-1) != MAX", satMulint8(min, -1) == max)
    failCount += check("[FAIL] Int8: (-1) * MIN != MAX", satMulint8(-1, min) == max)

    failCount += check("[FAIL] Int8: MIN / (-1) != MAX", satDivint8(min, -1) == max)
    failCount += check("[FAIL] Int8: MAX / (-1) != MIN + 1", satDivint8(max, -1) == min + 1)

    failCount += check("[FAIL] Int8: -(MAX) != MIN + 1", satNegint8(max) == min + 1)
    failCount += check("[FAIL] Int8: -(MIN) != MAX", satNegint8(min) == max)

    failCount += check("[FAIL] Int8: (MAX / 2) * 4 != MAX", satMulint8(max / 2, 4) == max)
    failCount += check("[FAIL] Int8: MAX * 1 != MAX", satMulint8(max, 1) == max)
    failCount += check("[FAIL] Int8: 1 * MAX != MAX", satMulint8(1, max) == max)
    failCount += check("[FAIL] Int8: MIN * 1 != MIN", satMulint8(min, 1) == min)
    failCount += check("[FAIL] Int8: 1 * MIN != MIN", satMulint8(1, min) == min)

    failCount += check("[FAIL] Int8: MIN / 1 != MIN", satDivint8(min, 1) == min)
    failCount += check("[FAIL] Int8: MAX / 1 != MAX", satDivint8(max, 1) == max)
    // We cannot check division by zero and remainder of it, since there is an assert that does not throw an exception

    failCount += check("[FAIL] Int8: inc MAX != MAX", satIncint8(max) == max)
    failCount += check("[FAIL] Int8: dec MIN != MIN", satDecint8(min) == min)

    if (failCount != 0) {
        println("[ERR] int8Test() failed. Fail count = ${failCount};")
    }
    return failCount
}

// Saturating UInt8 operations
@OverflowSaturating
func satAdduint8(a: UInt8, b: UInt8): UInt8 {
    a + b
}

@OverflowSaturating
func satSubuint8(a: UInt8, b: UInt8): UInt8 {
    a - b
}

@OverflowSaturating
func satMuluint8(a: UInt8, b: UInt8): UInt8 {
    a * b
}

@OverflowSaturating
func satDivuint8(a: UInt8, b: UInt8): UInt8 {
    a / b
}

@OverflowSaturating
func satModuint8(a: UInt8, b: UInt8): UInt8 {
    a % b
}

@OverflowSaturating
func satIncuint8(a: UInt8): UInt8 {
    a + 1
}

@OverflowSaturating
func satDecuint8(a: UInt8): UInt8 {
    a - 1
}

@OverflowSaturating
func satNeguint8(a: UInt8): UInt8 {
    0 - a
}

func uint8Test(): Int64 {
    var failCount: Int64 = 0

    let max: UInt8 = UInt8.Max
    let min: UInt8 = UInt8(0)

    failCount += check("[FAIL] UInt8: 10 + 8 != 18", satAdduint8(10, 8) == 18)
    failCount += check("[FAIL] UInt8: 10 - 8 != 2", satSubuint8(10, 8) == 2)
    failCount += check("[FAIL] UInt8: 2 * 8 != 16", satMuluint8(2, 8) == 16)
    failCount += check("[FAIL] UInt8: 10 / 6 != 1", satDivuint8(10, 6) == 1)
    failCount += check("[FAIL] UInt8: 10 % 6 != 4", satModuint8(10, 6) == 4)
    failCount += check("[FAIL] UInt8: inc 0 != 1", satIncuint8(0) == 1)
    failCount += check("[FAIL] UInt8: dec 3 != 2", satDecuint8(3) == 2)
    failCount += check("[FAIL] UInt8: -(2) != 0", satNeguint8(2) == 0)
    failCount += check("[FAIL] UInt8: -(0) != 0", satNeguint8(0) == 0)

    failCount += check("[FAIL] UInt8: MAX + 10 != MAX", satAdduint8(max, 10) == max)
    failCount += check("[FAIL] UInt8: (MAX - 9) + 10 != MAX", satAdduint8(max - 9, 10) == max)
    failCount += check("[FAIL] UInt8: (MAX - 11) + 10 != MAX - 1", satAdduint8(max - 11, 10) == max - 1)

    failCount += check("[FAIL] UInt8: MIN - 10 != MIN", satSubuint8(min, 10) == min)
    failCount += check("[FAIL] UInt8: (MIN + 9) - 10 != MIN", satSubuint8(min + 9, 10) == min)
    failCount += check("[FAIL] UInt8: (MIN + 11) - 10 != MIN + 1", satSubuint8(min + 11, 10) == min + 1)

    failCount += check("[FAIL] UInt8: (MAX / 2) * 4 != MAX", satMuluint8(max / 2, 4) == max)
    failCount += check("[FAIL] UInt8: MAX * 1 != MAX", satMuluint8(max, 1) == max)
    failCount += check("[FAIL] UInt8: 1 * MAX != MAX", satMuluint8(1, max) == max)
    failCount += check("[FAIL] UInt8: MIN * 1 != MIN", satMuluint8(min, 1) == min)
    failCount += check("[FAIL] UInt8: 1 * MIN != MIN", satMuluint8(1, min) == min)

    failCount += check("[FAIL] UInt8: MIN / 1 != MIN", satDivuint8(min, 1) == min)
    failCount += check("[FAIL] UInt8: MAX / 1 != MAX", satDivuint8(max, 1) == max)
    // We cannot check division by zero and remainder of it, since there is an assert that does not throw an exception

    failCount += check("[FAIL] UInt8: inc MAX != MAX", satIncuint8(max) == max)
    failCount += check("[FAIL] UInt8: dec MIN != MIN", satDecuint8(min) == min)

    if (failCount != 0) {
        println("[ERR] uint8Test() failed. Fail count = ${failCount};")
    }
    return failCount
}

// Saturating Int16 operations
@OverflowSaturating
func satAddint16(a: Int16, b: Int16): Int16 {
    a + b
}

@OverflowSaturating
func satSubint16(a: Int16, b: Int16): Int16 {
    a - b
}

@OverflowSaturating
func satMulint16(a: Int16, b: Int16): Int16 {
    a * b
}

@OverflowSaturating
func satDivint16(a: Int16, b: Int16): Int16 {
    a / b
}

@OverflowSaturating
func satModint16(a: Int16, b: Int16): Int16 {
    a % b
}

@OverflowSaturating
func satIncint16(a: Int16): Int16 {
    a + 1
}

@OverflowSaturating
func satDecint16(a: Int16): Int16 {
    a - 1
}

@OverflowSaturating
func satNegint16(a: Int16): Int16 {
    -a
}

func int16Test(): Int64 {
    var failCount: Int64 = 0

    let max: Int16 = Int16.Max
    let min: Int16 = Int16.Min

    failCount += check("[FAIL] Int16: -10 + 8 != -2", satAddint16(-10, 8) == -2) 
    failCount += check("[FAIL] Int16: 10 - 8 != 2", satSubint16(10, 8) == 2)
    failCount += check("[FAIL] Int16: 2 * 8 != 16", satMulint16(2, 8) == 16)
    failCount += check("[FAIL] Int16: 10 / 6 != 1", satDivint16(10, 6) == 1)
    failCount += check("[FAIL] Int16: 10 % 6 != 4", satModint16(10, 6) == 4)
    failCount += check("[FAIL] Int16: inc 0 != 1", satIncint16(0) == 1)
    failCount += check("[FAIL] Int16: dec 0 != -1", satDecint16(0) == -1)
    failCount += check("[FAIL] Int16: -(2) != -2", satNegint16(2) == -2)
    failCount += check("[FAIL] Int16: -(0) != 0", satNegint16(0) == 0)

    failCount += check("[FAIL] Int16: MAX + 10 != MAX", satAddint16(max, 10) == max)
    failCount += check("[FAIL] Int16: MIN + (-10) != MIN", satAddint16(min, -10) == min)
    failCount += check("[FAIL] Int16: (MAX - 9) + 10 != MAX", satAddint16(max - 9, 10) == max)
    failCount += check("[FAIL] Int16: (MIN + 9) + (-10) != MIN", satAddint16(min + 9, -10) == min)
    failCount += check("[FAIL] Int16: (MAX - 11) + 10 != MAX - 1", satAddint16(max - 11, 10) == max - 1)
    failCount += check("[FAIL] Int16: (MIN + 11) + (-10) != MIN + 1", satAddint16(min + 11, -10) == min + 1)

    failCount += check("[FAIL] Int16: MAX - (-10) != MAX", satSubint16(max, -10) == max)
    failCount += check("[FAIL] Int16: MIN - 10 != MIN", satSubint16(min, 10) == min)
    failCount += check("[FAIL] Int16: (MAX - 9) - (-10) != MAX", satSubint16(max - 9, -10) == max)
    failCount += check("[FAIL] Int16: (MIN + 9) - 10 != MIN", satSubint16(min + 9, 10) == min)
    failCount += check("[FAIL] Int16: (MAX - 11) - (-10) != MAX - 1", satSubint16(max - 11, -10) == max - 1)
    failCount += check("[FAIL] Int16: (MIN + 11) - 10 != MIN + 1", satSubint16(min + 11, 10) == min + 1)

    failCount += check("[FAIL] Int16: (MAX / 2) * (-4) != MIN", satMulint16(max / 2, -4) == min)
    failCount += check("[FAIL] Int16: MAX * (-1) != MIN + 1", satMulint16(max, -1) == min + 1)
    failCount += check("[FAIL] Int16: (-1) * MAX != MIN + 1", satMulint16(-1, max) == min + 1)
    failCount += check("[FAIL] Int16: MAX saturating.* (-1) != MAX * (-1)", satMulint16(max, -1) == max * -1)
    failCount += check("[FAIL] Int16: MIN * (-1) != MAX", satMulint16(min, -1) == max)
    failCount += check("[FAIL] Int16: (-1) * MIN != MAX", satMulint16(-1, min) == max)

    failCount += check("[FAIL] Int16: MIN / (-1) != MAX", satDivint16(min, -1) == max)
    failCount += check("[FAIL] Int16: MAX / (-1) != MIN + 1", satDivint16(max, -1) == min + 1)

    failCount += check("[FAIL] Int16: -(MAX) != MIN + 1", satNegint16(max) == min + 1)
    failCount += check("[FAIL] Int16: -(MIN) != MAX", satNegint16(min) == max)

    failCount += check("[FAIL] Int16: (MAX / 2) * 4 != MAX", satMulint16(max / 2, 4) == max)
    failCount += check("[FAIL] Int16: MAX * 1 != MAX", satMulint16(max, 1) == max)
    failCount += check("[FAIL] Int16: 1 * MAX != MAX", satMulint16(1, max) == max)
    failCount += check("[FAIL] Int16: MIN * 1 != MIN", satMulint16(min, 1) == min)
    failCount += check("[FAIL] Int16: 1 * MIN != MIN", satMulint16(1, min) == min)

    failCount += check("[FAIL] Int16: MIN / 1 != MIN", satDivint16(min, 1) == min)
    failCount += check("[FAIL] Int16: MAX / 1 != MAX", satDivint16(max, 1) == max)
    // We cannot check division by zero and remainder of it, since there is an assert that does not throw an exception

    failCount += check("[FAIL] Int16: inc MAX != MAX", satIncint16(max) == max)
    failCount += check("[FAIL] Int16: dec MIN != MIN", satDecint16(min) == min)

    if (failCount != 0) {
        println("[ERR] int16Test() failed. Fail count = ${failCount};")
    }
    return failCount
}

// Saturating UInt16 operations
@OverflowSaturating
func satAdduint16(a: UInt16, b: UInt16): UInt16 {
    a + b
}

@OverflowSaturating
func satSubuint16(a: UInt16, b: UInt16): UInt16 {
    a - b
}

@OverflowSaturating
func satMuluint16(a: UInt16, b: UInt16): UInt16 {
    a * b
}

@OverflowSaturating
func satDivuint16(a: UInt16, b: UInt16): UInt16 {
    a / b
}

@OverflowSaturating
func satModuint16(a: UInt16, b: UInt16): UInt16 {
    a % b
}

@OverflowSaturating
func satIncuint16(a: UInt16): UInt16 {
    a + 1
}

@OverflowSaturating
func satDecuint16(a: UInt16): UInt16 {
    a - 1
}

@OverflowSaturating
func satNeguint16(a: UInt16): UInt16 {
    0 - a
}

func uint16Test(): Int64 {
    var failCount: Int64 = 0

    let max: UInt16 = UInt16.Max
    let min: UInt16 = UInt16(0)

    failCount += check("[FAIL] UInt16: 10 + 8 != 18", satAdduint16(10, 8) == 18)
    failCount += check("[FAIL] UInt16: 10 - 8 != 2", satSubuint16(10, 8) == 2)
    failCount += check("[FAIL] UInt16: 2 * 8 != 16", satMuluint16(2, 8) == 16)
    failCount += check("[FAIL] UInt16: 10 / 6 != 1", satDivuint16(10, 6) == 1)
    failCount += check("[FAIL] UInt16: 10 % 6 != 4", satModuint16(10, 6) == 4)
    failCount += check("[FAIL] UInt16: inc 0 != 1", satIncuint16(0) == 1)
    failCount += check("[FAIL] UInt16: dec 3 != 2", satDecuint16(3) == 2)
    failCount += check("[FAIL] UInt16: -(2) != 0", satNeguint16(2) == 0)
    failCount += check("[FAIL] UInt16: -(0) != 0", satNeguint16(0) == 0)

    failCount += check("[FAIL] UInt16: MAX + 10 != MAX", satAdduint16(max, 10) == max)
    failCount += check("[FAIL] UInt16: (MAX - 9) + 10 != MAX", satAdduint16(max - 9, 10) == max)
    failCount += check("[FAIL] UInt16: (MAX - 11) + 10 != MAX - 1", satAdduint16(max - 11, 10) == max - 1)

    failCount += check("[FAIL] UInt16: MIN - 10 != MIN", satSubuint16(min, 10) == min)
    failCount += check("[FAIL] UInt16: (MIN + 9) - 10 != MIN", satSubuint16(min + 9, 10) == min)
    failCount += check("[FAIL] UInt16: (MIN + 11) - 10 != MIN + 1", satSubuint16(min + 11, 10) == min + 1)

    failCount += check("[FAIL] UInt16: (MAX / 2) * 4 != MAX", satMuluint16(max / 2, 4) == max)
    failCount += check("[FAIL] UInt16: MAX * 1 != MAX", satMuluint16(max, 1) == max)
    failCount += check("[FAIL] UInt16: 1 * MAX != MAX", satMuluint16(1, max) == max)
    failCount += check("[FAIL] UInt16: MIN * 1 != MIN", satMuluint16(min, 1) == min)
    failCount += check("[FAIL] UInt16: 1 * MIN != MIN", satMuluint16(1, min) == min)

    failCount += check("[FAIL] UInt16: MIN / 1 != MIN", satDivuint16(min, 1) == min)
    failCount += check("[FAIL] UInt16: MAX / 1 != MAX", satDivuint16(max, 1) == max)
    // We cannot check division by zero and remainder of it, since there is an assert that does not throw an exception

    failCount += check("[FAIL] UInt16: inc MAX != MAX", satIncuint16(max) == max)
    failCount += check("[FAIL] UInt16: dec MIN != MIN", satDecuint16(min) == min)

    if (failCount != 0) {
        println("[ERR] uint16Test() failed. Fail count = ${failCount};")
    }
    return failCount
}

// Saturating Int32 operations
@OverflowSaturating
func satAddint32(a: Int32, b: Int32): Int32 {
    a + b
}

@OverflowSaturating
func satSubint32(a: Int32, b: Int32): Int32 {
    a - b
}

@OverflowSaturating
func satMulint32(a: Int32, b: Int32): Int32 {
    a * b
}

@OverflowSaturating
func satDivint32(a: Int32, b: Int32): Int32 {
    a / b
}

@OverflowSaturating
func satModint32(a: Int32, b: Int32): Int32 {
    a % b
}

@OverflowSaturating
func satIncint32(a: Int32): Int32 {
    a + 1
}

@OverflowSaturating
func satDecint32(a: Int32): Int32 {
    a - 1
}

@OverflowSaturating
func satNegint32(a: Int32): Int32 {
    -a
}

func int32Test(): Int64 {
    var failCount: Int64 = 0

    let max: Int32 = Int32.Max
    let min: Int32 = Int32.Min

    failCount += check("[FAIL] Int32: -10 + 8 != -2", satAddint32(-10, 8) == -2) 
    failCount += check("[FAIL] Int32: 10 - 8 != 2", satSubint32(10, 8) == 2)
    failCount += check("[FAIL] Int32: 2 * 8 != 16", satMulint32(2, 8) == 16)
    failCount += check("[FAIL] Int32: 10 / 6 != 1", satDivint32(10, 6) == 1)
    failCount += check("[FAIL] Int32: 10 % 6 != 4", satModint32(10, 6) == 4)
    failCount += check("[FAIL] Int32: inc 0 != 1", satIncint32(0) == 1)
    failCount += check("[FAIL] Int32: dec 0 != -1", satDecint32(0) == -1)
    failCount += check("[FAIL] Int32: -(2) != -2", satNegint32(2) == -2)
    failCount += check("[FAIL] Int32: -(0) != 0", satNegint32(0) == 0)

    failCount += check("[FAIL] Int32: MAX + 10 != MAX", satAddint32(max, 10) == max)
    failCount += check("[FAIL] Int32: MIN + (-10) != MIN", satAddint32(min, -10) == min)
    failCount += check("[FAIL] Int32: (MAX - 9) + 10 != MAX", satAddint32(max - 9, 10) == max)
    failCount += check("[FAIL] Int32: (MIN + 9) + (-10) != MIN", satAddint32(min + 9, -10) == min)
    failCount += check("[FAIL] Int32: (MAX - 11) + 10 != MAX - 1", satAddint32(max - 11, 10) == max - 1)
    failCount += check("[FAIL] Int32: (MIN + 11) + (-10) != MIN + 1", satAddint32(min + 11, -10) == min + 1)

    failCount += check("[FAIL] Int32: MAX - (-10) != MAX", satSubint32(max, -10) == max)
    failCount += check("[FAIL] Int32: MIN - 10 != MIN", satSubint32(min, 10) == min)
    failCount += check("[FAIL] Int32: (MAX - 9) - (-10) != MAX", satSubint32(max - 9, -10) == max)
    failCount += check("[FAIL] Int32: (MIN + 9) - 10 != MIN", satSubint32(min + 9, 10) == min)
    failCount += check("[FAIL] Int32: (MAX - 11) - (-10) != MAX - 1", satSubint32(max - 11, -10) == max - 1)
    failCount += check("[FAIL] Int32: (MIN + 11) - 10 != MIN + 1", satSubint32(min + 11, 10) == min + 1)

    failCount += check("[FAIL] Int32: (MAX / 2) * (-4) != MIN", satMulint32(max / 2, -4) == min)
    failCount += check("[FAIL] Int32: MAX * (-1) != MIN + 1", satMulint32(max, -1) == min + 1)
    failCount += check("[FAIL] Int32: (-1) * MAX != MIN + 1", satMulint32(-1, max) == min + 1)
    failCount += check("[FAIL] Int32: MAX saturating.* (-1) != MAX * (-1)", satMulint32(max, -1) == max * -1)
    failCount += check("[FAIL] Int32: MIN * (-1) != MAX", satMulint32(min, -1) == max)
    failCount += check("[FAIL] Int32: (-1) * MIN != MAX", satMulint32(-1, min) == max)

    failCount += check("[FAIL] Int32: MIN / (-1) != MAX", satDivint32(min, -1) == max)
    failCount += check("[FAIL] Int32: MAX / (-1) != MIN + 1", satDivint32(max, -1) == min + 1)

    failCount += check("[FAIL] Int32: -(MAX) != MIN + 1", satNegint32(max) == min + 1)
    failCount += check("[FAIL] Int32: -(MIN) != MAX", satNegint32(min) == max)

    failCount += check("[FAIL] Int32: (MAX / 2) * 4 != MAX", satMulint32(max / 2, 4) == max)
    failCount += check("[FAIL] Int32: MAX * 1 != MAX", satMulint32(max, 1) == max)
    failCount += check("[FAIL] Int32: 1 * MAX != MAX", satMulint32(1, max) == max)
    failCount += check("[FAIL] Int32: MIN * 1 != MIN", satMulint32(min, 1) == min)
    failCount += check("[FAIL] Int32: 1 * MIN != MIN", satMulint32(1, min) == min)

    failCount += check("[FAIL] Int32: MIN / 1 != MIN", satDivint32(min, 1) == min)
    failCount += check("[FAIL] Int32: MAX / 1 != MAX", satDivint32(max, 1) == max)
    // We cannot check division by zero and remainder of it, since there is an assert that does not throw an exception

    failCount += check("[FAIL] Int32: inc MAX != MAX", satIncint32(max) == max)
    failCount += check("[FAIL] Int32: dec MIN != MIN", satDecint32(min) == min)

    if (failCount != 0) {
        println("[ERR] int32Test() failed. Fail count = ${failCount};")
    }
    return failCount
}

// Saturating UInt32 operations
@OverflowSaturating
func satAdduint32(a: UInt32, b: UInt32): UInt32 {
    a + b
}

@OverflowSaturating
func satSubuint32(a: UInt32, b: UInt32): UInt32 {
    a - b
}

@OverflowSaturating
func satMuluint32(a: UInt32, b: UInt32): UInt32 {
    a * b
}

@OverflowSaturating
func satDivuint32(a: UInt32, b: UInt32): UInt32 {
    a / b
}

@OverflowSaturating
func satModuint32(a: UInt32, b: UInt32): UInt32 {
    a % b
}

@OverflowSaturating
func satIncuint32(a: UInt32): UInt32 {
    a + 1
}

@OverflowSaturating
func satDecuint32(a: UInt32): UInt32 {
    a - 1
}

@OverflowSaturating
func satNeguint32(a: UInt32): UInt32 {
    0 - a
}

func uint32Test(): Int64 {
    var failCount: Int64 = 0

    let max: UInt32 = UInt32.Max
    let min: UInt32 = UInt32(0)

    failCount += check("[FAIL] UInt32: 10 + 8 != 18", satAdduint32(10, 8) == 18)
    failCount += check("[FAIL] UInt32: 10 - 8 != 2", satSubuint32(10, 8) == 2)
    failCount += check("[FAIL] UInt32: 2 * 8 != 16", satMuluint32(2, 8) == 16)
    failCount += check("[FAIL] UInt32: 10 / 6 != 1", satDivuint32(10, 6) == 1)
    failCount += check("[FAIL] UInt32: 10 % 6 != 4", satModuint32(10, 6) == 4)
    failCount += check("[FAIL] UInt32: inc 0 != 1", satIncuint32(0) == 1)
    failCount += check("[FAIL] UInt32: dec 3 != 2", satDecuint32(3) == 2)
    failCount += check("[FAIL] UInt32: -(2) != 0", satNeguint32(2) == 0)
    failCount += check("[FAIL] UInt32: -(0) != 0", satNeguint32(0) == 0)

    failCount += check("[FAIL] UInt32: MAX + 10 != MAX", satAdduint32(max, 10) == max)
    failCount += check("[FAIL] UInt32: (MAX - 9) + 10 != MAX", satAdduint32(max - 9, 10) == max)
    failCount += check("[FAIL] UInt32: (MAX - 11) + 10 != MAX - 1", satAdduint32(max - 11, 10) == max - 1)

    failCount += check("[FAIL] UInt32: MIN - 10 != MIN", satSubuint32(min, 10) == min)
    failCount += check("[FAIL] UInt32: (MIN + 9) - 10 != MIN", satSubuint32(min + 9, 10) == min)
    failCount += check("[FAIL] UInt32: (MIN + 11) - 10 != MIN + 1", satSubuint32(min + 11, 10) == min + 1)

    failCount += check("[FAIL] UInt32: (MAX / 2) * 4 != MAX", satMuluint32(max / 2, 4) == max)
    failCount += check("[FAIL] UInt32: MAX * 1 != MAX", satMuluint32(max, 1) == max)
    failCount += check("[FAIL] UInt32: 1 * MAX != MAX", satMuluint32(1, max) == max)
    failCount += check("[FAIL] UInt32: MIN * 1 != MIN", satMuluint32(min, 1) == min)
    failCount += check("[FAIL] UInt32: 1 * MIN != MIN", satMuluint32(1, min) == min)

    failCount += check("[FAIL] UInt32: MIN / 1 != MIN", satDivuint32(min, 1) == min)
    failCount += check("[FAIL] UInt32: MAX / 1 != MAX", satDivuint32(max, 1) == max)
    // We cannot check division by zero and remainder of it, since there is an assert that does not throw an exception

    failCount += check("[FAIL] UInt32: inc MAX != MAX", satIncuint32(max) == max)
    failCount += check("[FAIL] UInt32: dec MIN != MIN", satDecuint32(min) == min)

    if (failCount != 0) {
        println("[ERR] uint32Test() failed. Fail count = ${failCount};")
    }
    return failCount
}

// Saturating Int64 operations
@OverflowSaturating
func satAddint64(a: Int64, b: Int64): Int64 {
    a + b
}

@OverflowSaturating
func satSubint64(a: Int64, b: Int64): Int64 {
    a - b
}

@OverflowSaturating
func satMulint64(a: Int64, b: Int64): Int64 {
    a * b
}

@OverflowSaturating
func satDivint64(a: Int64, b: Int64): Int64 {
    a / b
}

@OverflowSaturating
func satModint64(a: Int64, b: Int64): Int64 {
    a % b
}

@OverflowSaturating
func satIncint64(a: Int64): Int64 {
    a + 1
}

@OverflowSaturating
func satDecint64(a: Int64): Int64 {
    a - 1
}

@OverflowSaturating
func satNegint64(a: Int64): Int64 {
    -a
}

@OverflowSaturating
func satPowint64(a: Int64, b: UInt64): Int64 {
    a ** b
}

func int64Test(): Int64 {
    var failCount: Int64 = 0

    let max: Int64 = Int64.Max
    let min: Int64 = Int64.Min

    failCount += check("[FAIL] Int64: -10 + 8 != -2", satAddint64(-10, 8) == -2) 
    failCount += check("[FAIL] Int64: 10 - 8 != 2", satSubint64(10, 8) == 2)
    failCount += check("[FAIL] Int64: 2 * 8 != 16", satMulint64(2, 8) == 16)
    failCount += check("[FAIL] Int64: 10 / 6 != 1", satDivint64(10, 6) == 1)
    failCount += check("[FAIL] Int64: 10 % 6 != 4", satModint64(10, 6) == 4)
    failCount += check("[FAIL] Int64: inc 0 != 1", satIncint64(0) == 1)
    failCount += check("[FAIL] Int64: dec 0 != -1", satDecint64(0) == -1)
    failCount += check("[FAIL] Int64: -(2) != -2", satNegint64(2) == -2)
    failCount += check("[FAIL] Int64: -(0) != 0", satNegint64(0) == 0)

    failCount += check("[FAIL] Int64: MAX + 10 != MAX", satAddint64(max, 10) == max)
    failCount += check("[FAIL] Int64: MIN + (-10) != MIN", satAddint64(min, -10) == min)
    failCount += check("[FAIL] Int64: (MAX - 9) + 10 != MAX", satAddint64(max - 9, 10) == max)
    failCount += check("[FAIL] Int64: (MIN + 9) + (-10) != MIN", satAddint64(min + 9, -10) == min)
    failCount += check("[FAIL] Int64: (MAX - 11) + 10 != MAX - 1", satAddint64(max - 11, 10) == max - 1)
    failCount += check("[FAIL] Int64: (MIN + 11) + (-10) != MIN + 1", satAddint64(min + 11, -10) == min + 1)

    failCount += check("[FAIL] Int64: MAX - (-10) != MAX", satSubint64(max, -10) == max)
    failCount += check("[FAIL] Int64: MIN - 10 != MIN", satSubint64(min, 10) == min)
    failCount += check("[FAIL] Int64: (MAX - 9) - (-10) != MAX", satSubint64(max - 9, -10) == max)
    failCount += check("[FAIL] Int64: (MIN + 9) - 10 != MIN", satSubint64(min + 9, 10) == min)
    failCount += check("[FAIL] Int64: (MAX - 11) - (-10) != MAX - 1", satSubint64(max - 11, -10) == max - 1)
    failCount += check("[FAIL] Int64: (MIN + 11) - 10 != MIN + 1", satSubint64(min + 11, 10) == min + 1)

    failCount += check("[FAIL] Int64: (MAX / 2) * (-4) != MIN", satMulint64(max / 2, -4) == min)
    failCount += check("[FAIL] Int64: MAX * (-1) != MIN + 1", satMulint64(max, -1) == min + 1)
    failCount += check("[FAIL] Int64: (-1) * MAX != MIN + 1", satMulint64(-1, max) == min + 1)
    failCount += check("[FAIL] Int64: MAX saturating.* (-1) != MAX * (-1)", satMulint64(max, -1) == max * -1)
    failCount += check("[FAIL] Int64: MIN * (-1) != MAX", satMulint64(min, -1) == max)
    failCount += check("[FAIL] Int64: (-1) * MIN != MAX", satMulint64(-1, min) == max)

    failCount += check("[FAIL] Int64: MIN / (-1) != MAX", satDivint64(min, -1) == max)
    failCount += check("[FAIL] Int64: MAX / (-1) != MIN + 1", satDivint64(max, -1) == min + 1)

    failCount += check("[FAIL] Int64: -(MAX) != MIN + 1", satNegint64(max) == min + 1)
    failCount += check("[FAIL] Int64: -(MIN) != MAX", satNegint64(min) == max)

    failCount += check("[FAIL] Int64: (MAX / 2) * 4 != MAX", satMulint64(max / 2, 4) == max)
    failCount += check("[FAIL] Int64: MAX * 1 != MAX", satMulint64(max, 1) == max)
    failCount += check("[FAIL] Int64: 1 * MAX != MAX", satMulint64(1, max) == max)
    failCount += check("[FAIL] Int64: MIN * 1 != MIN", satMulint64(min, 1) == min)
    failCount += check("[FAIL] Int64: 1 * MIN != MIN", satMulint64(1, min) == min)

    failCount += check("[FAIL] Int64: MIN / 1 != MIN", satDivint64(min, 1) == min)
    failCount += check("[FAIL] Int64: MAX / 1 != MAX", satDivint64(max, 1) == max)
    // We cannot check division by zero and remainder of it, since there is an assert that does not throw an exception

    failCount += check("[FAIL] Int64: inc MAX != MAX", satIncint64(max) == max)
    failCount += check("[FAIL] Int64: dec MIN != MIN", satDecint64(min) == min)

    failCount += check("[FAIL] Int64: 2 ^ 10 != 1024", satPowint64(2, 10) == 1024)
    failCount += check("[FAIL] Int64: 2 ^ 512 != MAX", satPowint64(2, 512) == max)

    if (failCount != 0) {
        println("[ERR] int64Test() failed. Fail count = ${failCount};")
    }
    return failCount
}

// Saturating UInt64 operations
@OverflowSaturating
func satAdduint64(a: UInt64, b: UInt64): UInt64 {
    a + b
}

@OverflowSaturating
func satSubuint64(a: UInt64, b: UInt64): UInt64 {
    a - b
}

@OverflowSaturating
func satMuluint64(a: UInt64, b: UInt64): UInt64 {
    a * b
}

@OverflowSaturating
func satDivuint64(a: UInt64, b: UInt64): UInt64 {
    a / b
}

@OverflowSaturating
func satModuint64(a: UInt64, b: UInt64): UInt64 {
    a % b
}

@OverflowSaturating
func satIncuint64(a: UInt64): UInt64 {
    a + 1
}

@OverflowSaturating
func satDecuint64(a: UInt64): UInt64 {
    a - 1
}

@OverflowSaturating
func satNeguint64(a: UInt64): UInt64 {
    0 - a
}

func uint64Test(): Int64 {
    var failCount: Int64 = 0

    let max: UInt64 = UInt64.Max
    let min: UInt64 = UInt64(0)

    failCount += check("[FAIL] UInt64: 10 + 8 != 18", satAdduint64(10, 8) == 18)
    failCount += check("[FAIL] UInt64: 10 - 8 != 2", satSubuint64(10, 8) == 2)
    failCount += check("[FAIL] UInt64: 2 * 8 != 16", satMuluint64(2, 8) == 16)
    failCount += check("[FAIL] UInt64: 10 / 6 != 1", satDivuint64(10, 6) == 1)
    failCount += check("[FAIL] UInt64: 10 % 6 != 4", satModuint64(10, 6) == 4)
    failCount += check("[FAIL] UInt64: inc 0 != 1", satIncuint64(0) == 1)
    failCount += check("[FAIL] UInt64: dec 3 != 2", satDecuint64(3) == 2)
    failCount += check("[FAIL] UInt64: -(2) != 0", satNeguint64(2) == 0)
    failCount += check("[FAIL] UInt64: -(0) != 0", satNeguint64(0) == 0)

    failCount += check("[FAIL] UInt64: MAX + 10 != MAX", satAdduint64(max, 10) == max)
    failCount += check("[FAIL] UInt64: (MAX - 9) + 10 != MAX", satAdduint64(max - 9, 10) == max)
    failCount += check("[FAIL] UInt64: (MAX - 11) + 10 != MAX - 1", satAdduint64(max - 11, 10) == max - 1)

    failCount += check("[FAIL] UInt64: MIN - 10 != MIN", satSubuint64(min, 10) == min)
    failCount += check("[FAIL] UInt64: (MIN + 9) - 10 != MIN", satSubuint64(min + 9, 10) == min)
    failCount += check("[FAIL] UInt64: (MIN + 11) - 10 != MIN + 1", satSubuint64(min + 11, 10) == min + 1)

    failCount += check("[FAIL] UInt64: (MAX / 2) * 4 != MAX", satMuluint64(max / 2, 4) == max)
    failCount += check("[FAIL] UInt64: MAX * 1 != MAX", satMuluint64(max, 1) == max)
    failCount += check("[FAIL] UInt64: 1 * MAX != MAX", satMuluint64(1, max) == max)
    failCount += check("[FAIL] UInt64: MIN * 1 != MIN", satMuluint64(min, 1) == min)
    failCount += check("[FAIL] UInt64: 1 * MIN != MIN", satMuluint64(1, min) == min)

    failCount += check("[FAIL] UInt64: MIN / 1 != MIN", satDivuint64(min, 1) == min)
    failCount += check("[FAIL] UInt64: MAX / 1 != MAX", satDivuint64(max, 1) == max)
    // We cannot check division by zero and remainder of it, since there is an assert that does not throw an exception

    failCount += check("[FAIL] UInt64: inc MAX != MAX", satIncuint64(max) == max)
    failCount += check("[FAIL] UInt64: dec MIN != MIN", satDecuint64(min) == min)

    if (failCount != 0) {
        println("[ERR] uint64Test() failed. Fail count = ${failCount};")
    }
    return failCount
}

main(): Int64 {
    var result: Int64 = 0
    result += int8Test()
    result += uint8Test()
    result += int16Test()
    result += uint16Test()
    result += int32Test()
    result += uint32Test()
    result += int64Test()
    result += uint64Test()

    return result
}
