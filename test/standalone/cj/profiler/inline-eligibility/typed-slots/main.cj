struct InlinePair {
    InlinePair(var first: Int64, var second: Int64) {}
}

func rejectedStackSlots(value: Int64): Int64 {
    var pair = InlinePair(value + 4, 0)
    return pair.first + pair.second
}

@OverflowWrapping
func typedSlotBranch(value: Int64, selector: Int64): Int64 {
    var pair: VArray<Int64, $2> = [value, value + 2]
    if (selector == 0) {
        pair[0] = pair[1] + 3
    }
    return pair[0] + pair[1]
}

@OverflowWrapping
func typedSlotCopy(value: Int64, selector: Int64): Int64 {
    var selected: VArray<Int64, $2> = [value, value + 2]
    let replacement: VArray<Int64, $2> = [value + 4, value + 6]
    if (selector != 0) {
        selected = replacement
    }
    selected[1] = selected[1] + 3
    return selected[0] + selected[1] + replacement[1]
}

@OverflowWrapping
func typedSlotPairSum(pair: VArray<Int64, $2>): Int64 {
    return pair[0] + pair[1]
}

@OverflowWrapping
func typedSlotLoopCaller(value: Int64, rounds: Int64): Int64 {
    let own: VArray<Int64, $2> = [value, value + 3]
    var result: Int64 = 0
    var iteration: Int64 = 0
    while (iteration < rounds) {
        result += typedSlotCopy(own[0], iteration)
        iteration++
    }
    return result + typedSlotPairSum(own)
}

@OverflowWrapping
func checkTypedSlots(seed: Int64): Int64 {
    let selector = seed & 1
    let rounds = selector + 1

    let expectedBranch = if (selector == 0) { 2 * seed + 7 } else { 2 * seed + 2 }
    if (typedSlotBranch(seed, selector) != expectedBranch) {
        println("typedSlotBranch failed")
        return 1
    }
    
     if (typedSlotBranch(seed, selector) != expectedBranch) {
        println("typedSlotBranch failed")
        return 1
    }

    let expectedCopy = if (rounds == 1) { 5 * seed + 14 } else { 8 * seed + 33 }
    if (typedSlotLoopCaller(seed, rounds) != expectedCopy) {
        println("typedSlotLoopCaller failed")
        return 1
    }

    if (typedSlotLoopCaller(seed, rounds) != expectedCopy) {
        println("typedSlotLoopCaller failed")
        return 1
    }
    if (rejectedStackSlots(seed) != seed + 4) {
        return 1
    }
    return 0
}

main(): Int64 {
    for (iteration in 0..500000) {
        if (checkTypedSlots(iteration & 7) != 0) {
            return 1
        }
    }
    println("OK")
    return 0
}
