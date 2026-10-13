import genericbox.*

main(): Int64 {
    let original = Pair<Int64>(10, 20)
    println(original.first)
    println(original.second)

    let copied = original.copy()
    println(copied.first)
    println(copied.second)

    let holder = PairHolder<Int64>(Pair<Int64>(1, 2), Pair<Int64>(30, 40))
    holder.copySecondToFirst()
    let assigned = holder.getFirst()
    println(assigned.first)
    println(assigned.second)
    0
}
