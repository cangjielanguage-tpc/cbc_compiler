import std.runtime.*

@OverflowWrapping
func hotLeaf(value: Int64): Int64 {
    return value + 1
}

@OverflowWrapping
func laterLeaf(value: Int64): Int64 {
    return value + 8
}

func hotCaller(input: Int64, firstPhase: Bool): Int64 {
    if (!firstPhase) {
        return laterLeaf(input)
    }

    var value = input
    value = hotLeaf(value)
    value = hotLeaf(value)
    value = hotLeaf(value)
    value = hotLeaf(value)
    value = hotLeaf(value)
    value = hotLeaf(value)
    value = hotLeaf(value)
    value = hotLeaf(value)
    return value
}

func worker(id: Int64): Int64 {
    var value = id
    for (iteration in 0..4000000) {
        value = hotCaller(value, iteration < 1000000)

        if (iteration % 500000 == 0) {
            gc()
            try {
                throw IllegalArgumentException()
            } catch (_: IllegalArgumentException) {
            }
        }
    }
    return value
}

main(): Int64 {
    let first = spawn { => worker(0) }
    let second = spawn { => worker(1) }
    let third = spawn { => worker(2) }

    let result = first.get() + second.get() + third.get()
    println(result)
    return 0
}
