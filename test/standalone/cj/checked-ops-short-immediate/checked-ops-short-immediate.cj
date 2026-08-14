@OverflowThrowing
func add_test_short_imm(a: Int32): Int32 {
    a + 2000
}

@OverflowThrowing
func test_signed8(a: Int8): Int8 {
    a * (-2)
}

@OverflowThrowing
func test_signed16(a: Int16): Int16 {
    a * (-2048)
}

main() : Int32 {
    let test_short = add_test_short_imm(20)
    println(test_short)
    let test8 = test_signed8(3)
    println(test8)
    let test16 = test_signed16(2)
    println(test16)
    0
}
