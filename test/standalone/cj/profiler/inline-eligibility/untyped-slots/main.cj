@OverflowWrapping
func untypedSlotIntLoop(value: Int64, rounds: Int64): Int64 {
    var a = value
    var b = value + 1
    var c = value + 2
    var d = value + 3
    var e = value + 4
    var f = value + 5
    var g = value + 6
    var h = value + 7
    var i = value + 8
    var j = value + 9
    var k = value + 10
    var remaining = rounds
    while (remaining > 0) {
        a = a ^ b
        b = b + c
        c = c ^ d
        d = d + e
        e = e ^ f
        f = f + g
        g = g ^ h
        h = h + i
        i = i ^ j
        j = j + k
        k = k ^ a
        remaining--
    }
    return a
}

@OverflowWrapping
func untypedSlotIntCaller(value: Int64, rounds: Int64): Int64 {
    let keep = value + 2
    return untypedSlotIntLoop(keep, rounds) + keep
}

@OverflowWrapping
func checkUntypedSlots(seed: Int64): Int64 {
    let selector = seed & 1
    let rounds = selector + 1

    let keptInt = seed + 2
    var expectedInt = keptInt ^ (keptInt + 1)
    if (rounds == 2) {
        expectedInt = expectedInt ^ (2 * keptInt + 3)
    }
    if (untypedSlotIntCaller(seed, rounds) != expectedInt + keptInt) {
        println("untypedSlotIntCaller failed")
        return 1
    }
    return 0
}

main(): Int64 {
    for (iteration in 0..500000) {
        if (checkUntypedSlots(iteration & 7) != 0) {
            return 1
        }
    }
    println("OK")
    return 0
}
