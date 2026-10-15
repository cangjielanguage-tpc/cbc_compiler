import genericcopyarray.*

main(): Int64 {
    let values = [Copyable<String>(1), Copyable<String>(30)]
    let copied = copyArrayElement<String>(values, 1, 0)
    println(copied.value)
    println(values[0].value)
    0
}
