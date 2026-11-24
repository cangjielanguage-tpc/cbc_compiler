func rejectedRecursion(value: Int64, depth: Int64): Int64 {
    if (depth == 0) {
        return value + 3
    }
    return rejectedRecursion(value, depth - 1)
}

func checkRecursion(value: Int64): Int64 {
    return if (rejectedRecursion(value, 1) == value + 3) { 0 } else { 1 }
}

main(): Int64 {
    for (iteration in 0..500000) {
        if (checkRecursion(iteration & 7) != 0) {
            return 1
        }
    }
    println("OK")
    return 0
}
