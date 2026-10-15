import genericcopy.*

main(): Int64 {
    let original = Copyable<String>(10)
    let copied = original.copy()
    println(copied.value)

    let local = copyLocal<String>(Copyable<String>(20))
    println(local.value)

    let holder = CopyableHolder<String>(Copyable<String>(1), Copyable<String>(30))
    holder.copySecondToFirst()
    let assigned = holder.getFirst()
    println(assigned.value)
    0
}
