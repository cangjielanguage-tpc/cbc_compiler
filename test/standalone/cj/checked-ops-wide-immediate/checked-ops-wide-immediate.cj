@OverflowThrowing
func add_test_wide_imm32(a: Int32): Int32 {
    a + 8000
}

@OverflowThrowing
func add_test_wide_imm16(a: Int16): Int16 {
    a + 4095
}

@OverflowThrowing
func add_test_wide_imm32u(a: UInt32): UInt32 {
    a + 2147483650
}

@OverflowThrowing
func add_test_wide_imm64u(a: UInt64): UInt64 {
    a + 9223372036854775810
}

@OverflowThrowing
func add_test_wide_neg_imm32(a: Int32): Int32 {
    a + (-100000)
}


main() : Int32 {
    let test_imm32 = add_test_wide_imm32(20)
    println(test_imm32)
    let test_imm16 = add_test_wide_imm16(42)
    println(test_imm16)
    let test_imm32u = add_test_wide_imm32u(20)
    println(test_imm32u)
    let test_imm64u = add_test_wide_imm64u(20)
    println(test_imm64u)
    let test_neg_imm32 = add_test_wide_neg_imm32(1)
    println(test_neg_imm32)
    0
}
