@OverflowWrapping
func allowedAdd(value: Int64): Int64 {
    return value + 1
}

func allowedIdentity(value: Int64): Int64 {
    return value
}

func rejectedComparison(value: Int64, other: Int64): Bool {
    return value < other
}

func rejectedImmediateComparison(value: Int64): Bool {
    return value < 10
}

func rejectedInt32Comparison(value: Int32, other: Int32): Bool {
    return value < other
}

func rejectedConversion(value: Int64): Int32 {
    return Int32(value)
}

func rejectedCheckedAdd(value: Int64): Int64 {
    return value + 1
}

func rejectedCheckedInt32Add(value: Int32): Int32 {
    return value + 1
}

func rejectedCheckedInt8Add(value: Int8, other: Int8): Int8 {
    return value + other
}

func rejectedCheckedInt16Wide(value: Int16): Int16 {
    return value + 4095
}

func rejectedCheckedInt32Wide(value: Int32): Int32 {
    return value + 8000
}

func rejectedCheckedInt64Add(value: Int64, other: Int64): Int64 {
    return value + other
}

@OverflowSaturating
func allowedSaturatingInt8Add(value: Int8, other: Int8): Int8 {
    return value + other
}

@OverflowSaturating
func allowedSaturatingInt64Wide(value: Int64): Int64 {
    return value + 8000
}

@OverflowSaturating
func allowedSaturatingShift(value: Int64, shift: Int64): Int64 {
    return value << shift
}

// Currently fails during runtime-bytecode preparation with "Register kind mismatch":
// the CBC call passes Float16 arguments in integer registers, while the rewriter expects float registers.
// func allowedFloat16Arithmetic(value: Float16, other: Float16): Float16 {
//     return value * other + other
// }

func allowedFloat32Arithmetic(value: Float32, other: Float32): Float32 {
    return value * other + other
}

func allowedFloat64Arithmetic(value: Float64, other: Float64): Float64 {
    return value * other + other
}

func rejectedTooLarge(value: Int64): Int64 {
    var result = value
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    if (result >= 0) { result++ } else { result-- }
    return result
}

func checkArithmetic(value: Int64): Int64 {
    if (allowedAdd(value) != value + 1 || allowedIdentity(value) != value) {
        return 1
    }
    if (!rejectedComparison(value, 10) || !rejectedImmediateComparison(value) ||
        !rejectedInt32Comparison(1, 10) || rejectedConversion(value) != Int32(value)) {
        return 1
    }
    if (rejectedCheckedAdd(value) != value + 1 || rejectedCheckedInt32Add(1) != 2 ||
        rejectedCheckedInt8Add(1, 2) != 3 || rejectedCheckedInt16Wide(1) != 4096 ||
        rejectedCheckedInt32Wide(1) != 8001 || rejectedCheckedInt64Add(value, 2) != value + 2 ||
        rejectedTooLarge(value) != value + 24) {
        return 1
    }
    if (allowedSaturatingInt8Add(120, 20) != 127 || allowedSaturatingInt64Wide(value) != value + 8000 ||
        allowedSaturatingShift(value, 1) != value * 2) {
        return 1
    }
    // allowedFloat16Arithmetic(1.5f16, 2.0f16)
    if (allowedFloat32Arithmetic(1.5f32, 2.0f32) != 5.0f32 ||
        allowedFloat64Arithmetic(1.5f64, 2.0f64) != 5.0f64) {
        return 1
    }
    return 0
}

main(): Int64 {
    for (iteration in 0..500000) {
        if (checkArithmetic(iteration & 7) != 0) {
            return 1
        }
    }
    println("OK")
    return 0
}
