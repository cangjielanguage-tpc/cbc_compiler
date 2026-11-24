import std.runtime.*

func rejectedLiveness(value: Int64): Int64 {
    gc()
    return value + 5
}

func rejectedExceptionRegions(value: Int64): Int64 {
    try {
        if (value < 0) {
            throw IllegalArgumentException()
        }
        return value + 6
    } catch (_: IllegalArgumentException) {
        return value
    }
}

func checkMetadata(value: Int64): Int64 {
    if (rejectedLiveness(value) != value + 5 || rejectedExceptionRegions(value) != value + 6) {
        return 1
    }
    return 0
}

main(): Int64 {
    for (iteration in 0..500000) {
        if (checkMetadata(iteration & 7) != 0) {
            return 1
        }
    }
    println("OK")
    return 0
}
