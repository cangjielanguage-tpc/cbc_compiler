@OverflowThrowing
func add_test8(a: Int8, b: Int8): Int8 {
    a + b
}

@OverflowThrowing
func sub_test8(a: Int8, b: Int8): Int8 {
    a - b
}

@OverflowThrowing
func mul_test8(a: Int8, b: Int8): Int8 {
    a * b
}

@OverflowThrowing
func div_test8(a: Int8, b: Int8): Int8 {
    a / b
}

@OverflowThrowing
func uadd_test8(a: UInt8, b: UInt8): UInt8 {
    a + b
}

@OverflowThrowing
func usub_test8(a: UInt8, b: UInt8): UInt8 {
    a - b
}

@OverflowThrowing
func umul_test8(a: UInt8, b: UInt8): UInt8 {
    a * b
}

@OverflowThrowing
func add_test16(a: Int16, b: Int16): Int16 {
    a + b
}

@OverflowThrowing
func sub_test16(a: Int16, b: Int16): Int16 {
    a - b
}

@OverflowThrowing
func mul_test16(a: Int16, b: Int16): Int16 {
    a * b
}

@OverflowThrowing
func div_test16(a: Int16, b: Int16): Int16 {
    a / b
}

@OverflowThrowing
func uadd_test16(a: UInt16, b: UInt16): UInt16 {
    a + b
}

@OverflowThrowing
func usub_test16(a: UInt16, b: UInt16): UInt16 {
    a - b
}

@OverflowThrowing
func umul_test16(a: UInt16, b: UInt16): UInt16 {
    a * b
}

func test8(): Unit {
    try {
        println("add_test8 without overflow")
	    let n = add_test8(7, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("sub_test8 without overflow")
	    let n = sub_test8(-3, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("mul_test8 without overflow")
	    let n = mul_test8(2, 3)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("div_test8 without overflow")
	    let n = div_test8(120, 6)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }
    try {
        println("uadd_test8 without overflow")
	    let n = uadd_test8(12, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("usub_test8 without overflow")
	    let n = usub_test8(43, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("umul_test8 without overflow")
	    let n = umul_test8(60, 4)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }
}

func testOverflow8(): Unit {
    try {
        println("add_test8 with overflow")
	    add_test8(120, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("sub_test8 with overflow")
	    sub_test8(-20, 120)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("mul_test8 with overflow")
	    mul_test8(34, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("div_test8 with overflow")
	    div_test8(-128, -1)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }
    try {
        println("uadd_test8 with overflow")
	    uadd_test8(200, 100)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("usub_test8 with overflow")
	    usub_test8(87, 200)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("umul_test8 with overflow")
	    umul_test8(100, 200)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }
}

func test16(): Unit {
    try {
        println("add_test16 without overflow")
	    let n = add_test16(7, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("sub_test16 without overflow")
	    let n = sub_test16(-34, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("mul_test16 without overflow")
	    let n = mul_test16(78, 2)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("div_test16 without overflow")
	    let n = div_test16(230, 23)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }
    try {
        println("uadd_test16 without overflow")
	    let n = uadd_test16(34, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("usub_test16 without overflow")
	    let n = usub_test16(78, 20)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }

    try {
        println("umul_test16 without overflow")
	    let n = umul_test16(345, 4)
	    println(n)
    } catch (e: OverflowException) {
        println("fail")
    }
}

func testOverflow16(): Unit {
    try {
        println("add_test16 with overflow")
	    add_test16(32762, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("sub_test16 with overflow")
	    sub_test16(-20, 32766)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("mul_test16 with overflow")
	    mul_test16(3276, 20)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("div_test16 with overflow")
	    div_test16(-32768, -1)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }
    try {
        println("uadd_test16 with overflow")
	    uadd_test16(32768, 32768)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("usub_test16 with overflow")
	    usub_test16(87, 3276)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }

    try {
        println("umul_test16 with overflow")
	    umul_test16(3276, 327)
	    println("fail")
    } catch (e: OverflowException) {
        println("pass")
    }
}

main() : Int64 {
    test8()
    testOverflow8()
    test16()
    testOverflow16()
    0
}
