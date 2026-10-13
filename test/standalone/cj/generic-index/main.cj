import genericarray.*
func foo() {
  var arr1: Array<VArray<Int64, $5>> = [[1, 2, 3, 4, 5], [11, 12, 13, 14, 15], [111, 112, 113, 114, 115]]
  arr1[1][3]
}

main() {
    let array = [1, 2, 3]
    println(getElement(array, 1))
    setElement(array, 1, 42)
    println(getElement(array, 1))

    let records = [(1, "first"), (2, "second"), (3, "third")]
    println(getElement(records, 1)[0])
    setElement(records, 1, (4, "replacement"))
    println(getElement(records, 1)[1])

    allocateEmptyArray<Int64>()
    println(repeatedArrayElement<Int64>(7))
    println(arrayOf<Int64>(9)[0])
    println(arrayify<Int64>({ => 5 })()[0])
    println(foo())
}
