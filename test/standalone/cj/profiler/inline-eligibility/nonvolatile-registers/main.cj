@OverflowWrapping
func nonVolatileIntLoop(value: Int64, rounds: Int64): Int64 {
    var a = value
    var b = value + 1
    var c = value + 2
    var d = value + 3
    var e = value + 4
    var remaining = rounds
    while (remaining > 0) {
        a = a ^ b
        b = b + c
        c = c ^ d
        d = d + e
        e = e ^ a
        remaining--
    }
    return a
}

@OverflowWrapping
func nonVolatileIntCaller(value: Int64, rounds: Int64): Int64 {
    let keep = value + 2
    return nonVolatileIntLoop(keep, rounds) + keep
}

@OverflowWrapping
func nonVolatileFloatLoop(value: Float64, rounds: Int64): Float64 {
    var a = value
    var b = a + 1.0
    var c = b + 1.0
    var d = c + 1.0
    var e = d + 1.0
    var f = e + 1.0
    var g = f + 1.0
    var h = g + 1.0
    var i = h + 1.0
    var remaining = rounds
    while (remaining > 0) {
        a = a + b
        b = b + c
        c = c - d
        d = d + e
        e = e - f
        f = f + g
        g = g - h
        h = h + i
        i = i - a
        remaining--
    }
    return a
}

func nonVolatileFloatCaller(value: Float64, rounds: Int64): Float64 {
    let keep = value + 1.0
    return nonVolatileFloatLoop(keep, rounds) + keep
}

@OverflowWrapping
func checkRegisters(seed: Int64): Int64 {
    let selector = seed & 1
    let rounds = selector + 1

    let keptInt = seed + 2
    var expectedInt = keptInt ^ (keptInt + 1)
    if (rounds == 2) {
        expectedInt = expectedInt ^ (2 * keptInt + 3)
    }
    if (nonVolatileIntCaller(seed, rounds) != expectedInt + keptInt) {
        println("nonVolatileIntCaller failed")
        return 1
    }

    let keptFloat = Float64(seed) + 1.0
    let expectedFloat = if (rounds == 1) { 3.0 * keptFloat + 1.0 } else { 5.0 * keptFloat + 4.0 }
    if (nonVolatileFloatCaller(Float64(seed), rounds) != expectedFloat) {
        println("nonVolatileFloatCaller failed")
        return 1
    }
    return 0
}

main(): Int64 {
    for (iteration in 0..500000) {
        if (checkRegisters(iteration & 7) != 0) {
            return 1
        }
    }
    println("OK")
    return 0
}
