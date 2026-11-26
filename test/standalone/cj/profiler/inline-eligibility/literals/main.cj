var globalOffset: Int64 = 8

@OverflowWrapping
func allowedLiteralLoad(value: Int64): Int64 {
    return value + globalOffset
}

func rejectedInstruction(value: Int64): Int64 {
    return value + globalOffset
}

@OverflowWrapping
func allowedLiteralValue(x: Int64, seed: Int64): Int64 {
    if (x == 65537) {
        var y = seed
        // Keep this call in the CBC even in the optimized build.
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        y = y ^ (y << 3) ^ (y >> 5) ^ (y << 7) ^ (y >> 11) ^ (y << 13) ^ (y >> 17) ^ (y << 19) ^ (y >> 23)
        return x + y + 65535
    }
    return x - 65537
}

@OverflowWrapping
func literalValueCaller(x: Int64): Int64 {
    return allowedLiteralValue(x, x & 2) + 1000003
}

@OverflowWrapping
func allowedLiteralBranchDelta(x0: Int64, longPath: Bool): Int64 {
    var x = x0
    if (longPath) {
        // Keep the arithmetic unrolled so the branch skips more than 2047 runtime CBC bytes.
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
        x = x ^ (x << 3) ^ (x >> 5) ^ (x << 7) ^ (x >> 11) ^ (x << 13) ^ (x >> 17) ^ (x << 19) ^ (x >> 23)
    }
    return x
}

@OverflowWrapping
func literalBranchDeltaCaller(x: Int64, longPath: Bool): Int64 {
    return allowedLiteralBranchDelta(x, longPath) ^ 65537
}

@OverflowWrapping
main(): Int64 {
    let longResult = allowedLiteralBranchDelta(1, true)
    if (longResult == 1) {
        return 1
    }

    for (i in 0..200000) {
        let expected = if (i == 65537) { 1131075 } else { i + 934466 }
        if (literalValueCaller(i) != expected) {
            return 1
        }
        if (allowedLiteralLoad(i) != i + 8 || rejectedInstruction(i) != i + 8) {
            return 1
        }
        if (literalBranchDeltaCaller(i, false) != (i ^ 65537)) {
            return 1
        }
    }

    if (literalBranchDeltaCaller(1, true) != (longResult ^ 65537) ||
        literalBranchDeltaCaller(1, false) != (1 ^ 65537)) {
        return 1
    }

    println("OK")
    return 0
}
