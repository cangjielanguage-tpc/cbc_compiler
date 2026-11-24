func checkedLeaf(value: Int64): Int64 {
    return value + 1
}

@OverflowWrapping
func rejectedNestedCall(value: Int64): Int64 {
    return checkedLeaf(value) + 2
}

main(): Int64 {
    for (iteration in 0..500000) {
        let value = iteration & 7
        if (rejectedNestedCall(value) != value + 3) {
            return 1
        }
    }
    println("OK")
    return 0
}
