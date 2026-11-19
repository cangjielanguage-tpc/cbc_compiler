import reference.*

// these C identity functions keep the operands opaque to constant folding
foreign func opaqueFloat(value: Float64): Float64
foreign func opaqueInt(value: Int64): Int64

func same(actual: Float64, expected: Float64): Bool {
    // a fractional power such as (-2.0) ** 0.5 yields NaN, which never equals itself
    if (expected.isNaN()) {
        return actual.isNaN()
    }
    if (actual != expected) {
        return false
    }
    // an odd power of -0.0 yields -0.0, so compare reciprocals to distinguish it from +0.0
    return expected != 0.0 || 1.0 / actual == 1.0 / expected
}

@OverflowThrowing
func check(base: Float64, exponent: Float64): Int64 {
    let x = unsafe { opaqueFloat(base) }
    let y = unsafe { opaqueFloat(exponent) }
    let actual = x ** y
    let expected = power(x, y)
    if (same(actual, expected)) {
        return 0
    }
    println("Float64 ** Float64: ${x} ** ${y}: ${actual}, expected ${expected}")
    return 1
}

@OverflowThrowing
func check(base: Float64, exponent: Int64): Int64 {
    let x = unsafe { opaqueFloat(base) }
    let y = unsafe { opaqueInt(exponent) }
    let actual = x ** y
    let expected = power(x, y)
    if (same(actual, expected)) {
        return 0
    }
    println("Float64 ** Int64: ${x} ** ${y}: ${actual}, expected ${expected}")
    return 1
}

main(): Int64 {
    var failures = 0
    let floatingCases = [
        (2.0, 10.0), (2.0, -2.0), (9.0, 0.5), (-2.0, 3.0), (-2.0, 4.0),
        (-2.0, 0.5), (0.0, 0.0), (0.0, 3.0), (0.0, -3.0),
        (-0.0, 3.0), (-0.0, 4.0), (-0.0, -3.0), (-0.0, -4.0),
        (Float64.Inf, 2.0), (Float64.Inf, -2.0), (-Float64.Inf, 3.0),
        (-Float64.Inf, -3.0), (Float64.NaN, 0.0), (Float64.NaN, 2.0),
        (1.0, Float64.NaN), (-1.0, Float64.Inf), (1.0, Float64.Inf),
        (2.0, Float64.Inf), (2.0, -Float64.Inf), (2.0, 1024.0),
        (2.0, -1074.0), (2.0, -1075.0)
    ]
    for ((base, exponent) in floatingCases) {
        failures += check(base, exponent)
    }
    let integerCases = [
        (2.0, 10), (2.0, -2), (-2.0, 3), (-2.0, 4),
        (0.0, 0), (0.0, 3), (0.0, -3), (-0.0, 3), (-0.0, 4),
        (-0.0, -3), (-0.0, -4), (Float64.NaN, 0), (Float64.NaN, 2),
        (Float64.Inf, 2), (Float64.Inf, -2), (-Float64.Inf, 3),
        (-Float64.Inf, -3), (2.0, 1024), (2.0, -1074), (2.0, -1075),
        (-1.0, 9007199254740993), (-1.0, -9007199254740993),
        (-1.0, Int64.Max), (-1.0, Int64.Min), (2.0, Int64.Max),
        (2.0, Int64.Min), (-0.0, Int64.Max), (-Float64.Inf, Int64.Max)
    ]
    for ((base, exponent) in integerCases) {
        failures += check(base, exponent)
    }

    // use literal operands to catch incorrect constant folding
    if (!same((-1.0) ** 9007199254740993, -1.0) ||
        !same(1.0 ** (0.0 / 0.0), 1.0) || !same((-1.0) ** (1.0 / 0.0), 1.0)) {
        println("constant exponentiation failed")
        failures += 1
    }

    // reproduce the std.core Float64.MinDenormal calculation
    let minDenormal = 1.0 / (2.0 ** 1022) * (1.0 / Float64(1 << 52))
    if (minDenormal != Float64.MinDenormal) {
        println("Float64.MinDenormal calculation failed")
        failures += 1
    }
    println("floating Exp failures: ${failures}")
    return failures
}
