import genericbox.*

main(): Int64 {
    let holder = PairHolder<Int64>(Pair<Int64>(1, 2), Pair<Int64>(30, 40))
    holder.copySecondToFirst()
    let assigned = holder.getFirst()
    println(assigned.first)
    println(assigned.second)

    let local = copyLocal<Int64>(Pair<Int64>(5, 6), Pair<Int64>(50, 60))
    println(local.first)
    println(local.second)

    let values = [Pair<Int64>(3, 4), Pair<Int64>(70, 80)]
    let arrayElement = copyArrayElement<Int64>(values, 1, 0)
    println(arrayElement.first)
    println(arrayElement.second)
    println(values[0].first)
    println(values[0].second)

    let strings = copyLocal<String>(Pair<String>("old first", "old second"), Pair<String>("first", "second"))
    println(strings.first)
    println(strings.second)

    let original = Pair<Int64>(10, 20)
    println(original.first)
    println(original.second)

    let copied = original.copy()
    println(copied.first)
    println(copied.second)
    0
}
