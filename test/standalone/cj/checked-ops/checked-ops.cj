@OverflowThrowing
func add_test64(a: Int64, b: Int64): Int64 {
    a + b
}

@OverflowThrowing
func sub_test64(a: Int64, b: Int64): Int64 {
    a - b
}

@OverflowThrowing
func mul_test64(a: Int64, b: Int64): Int64 {
    a * b
}

@OverflowThrowing
func div_test64(a: Int64, b: Int64): Int64 {
    a / b
}

@OverflowThrowing
func uadd_test64(a: UInt64, b: UInt64): UInt64 {
    a + b
}

@OverflowThrowing
func usub_test64(a: UInt64, b: UInt64): UInt64 {
    a - b
}

@OverflowThrowing
func umul_test64(a: UInt64, b: UInt64): UInt64 {
    a * b
}

@OverflowThrowing
func pow_test64(a: Int64, b: UInt64): Int64 {
    a ** b
}

@OverflowThrowing
func add_test32(a: Int32, b: Int32): Int32 {
    a + b
}

@OverflowThrowing
func sub_test32(a: Int32, b: Int32): Int32 {
    a - b
}

@OverflowThrowing
func mul_test32(a: Int32, b: Int32): Int32 {
    a * b
}

@OverflowThrowing
func div_test32(a: Int32, b: Int32): Int32 {
    a / b
}

@OverflowThrowing
func uadd_test32(a: UInt32, b: UInt32): UInt32 {
    a + b
}

@OverflowThrowing
func usub_test32(a: UInt32, b: UInt32): UInt32 {
    a - b
}

@OverflowThrowing
func umul_test32(a: UInt32, b: UInt32): UInt32 {
    a * b
}

func test64(): Unit {
    try {
        println("add_test64 without overflow")
	    let n = add_test64(7, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("sub_test64 without overflow")
	    let n = sub_test64(-476657, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("mul_test64 without overflow")
	    let n = mul_test64(78, 456)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("div_test64 without overflow")
	    let n = div_test64(56344, 23)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }
    try {
        println("uadd_test64 without overflow")
	    let n = uadd_test64(922337203685477580, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("usub_test64 without overflow")
	    let n = usub_test64(78, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("umul_test64 without overflow")
	    let n = umul_test64(32, 10)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("pow_test64 without overflow")
        let n = pow_test64(3, 5)
        println(n)
    } catch (e: OverflowException) {
        println("fail")
    }
}

func testOverflow64(): Unit {
    try {
        println("add_test64 with overflow")
	    add_test64(9223372036854775800, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("sub_test64 with overflow")
	    sub_test64(-20, 9223372036854775800)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("mul_test64 with overflow")
	    mul_test64(9223372036854775800, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("div_test64 with overflow")
	    div_test64(-9223372036854775808, -1)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }
    try {
        println("uadd_test64 with overflow")
	    uadd_test64(18446744073709551610, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("usub_test64 with overflow")
	    usub_test64(87, 18446744073709551615)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("umul_test64 with overflow")
	    umul_test64(9223372036854775800, 9223372036854775800)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("pow_test64 with overflow")
        pow_test64(30, 40)
        println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }
}

func test32(): Unit {
    try {
        println("add_test32 without overflow")
	    let n = add_test32(7, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("sub_test32 without overflow")
	    let n = sub_test32(-476657, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("mul_test32 without overflow")
	    let n = mul_test32(78, 456)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("div_test32 without overflow")
	    let n = div_test32(56344, 23)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }
    try {
        println("uadd_test32 without overflow")
	    let n = uadd_test32(34, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("usub_test32 without overflow")
	    let n = usub_test32(78, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("umul_test32 without overflow")
	    let n = umul_test32(135, 453)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }
}

func testOverflow32(): Unit {
    try {
        println("add_test32 with overflow")
	    add_test32(2147483643, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("sub_test32 with overflow")
	    sub_test32(-20, 2147483642)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("mul_test32 with overflow")
	    mul_test32(214748364, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("div_test32 with overflow")
	    div_test32(-2147483648, -1)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }
    try {
        println("uadd_test32 with overflow")
	    uadd_test32(4294967294, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("usub_test32 with overflow")
	    usub_test32(87, 65756)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("umul_test32 with overflow")
	    umul_test32(429496729, 4294967)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }
}

main() : Int64 {
    test64()
    testOverflow64()
    test32()
    testOverflow32()
    0
}
