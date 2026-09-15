@OverflowThrowing
public func neg(a: Int64): Int64 {
	try {
		return -a
	} catch (e: ArithmeticException) {
		println("NEG exception")
		return 0
	}
}

@OverflowThrowing
public func uneg(a: UInt64): UInt64 {
	try {
		return -a
	} catch (e: ArithmeticException) {
		println("UNEG exception")
		return 0
	}
}

@OverflowThrowing
public func lShift(a: Int64, b: Int64): Int64 {
	try {
		return a << b
	} catch (e: ArithmeticException) {
		println("LSH exception")
		return 0
	}
}

@OverflowThrowing
public func ulShift(a: UInt64, b: Int64): UInt64 {
	try {
		return a << b
	} catch (e: ArithmeticException) {
		println("ULSH exception")
		return 0
	}
}

@OverflowThrowing
public func rShift(a: Int64, b: Int64): Int64 {
	try {
		return a >> b
	} catch (e: ArithmeticException) {
		println("RSH exception")
		return 0
	}
}

@OverflowThrowing
public func urShift(a: UInt64, b: Int64): UInt64 {
	try {
		return a >> b
	} catch (e: ArithmeticException) {
		println("URSH exception")
		return 0
	}
}

public var resS: Int64  = 0
public var resU: UInt64 = 0

main(): Int64 {
	resS = neg(123)       // ok
	println(resS)
	resS = neg(Int64.Min) // error only in this case
	println(resS)

	println()

	resU = uneg(0)        // ok
	println(resU)
	resU = uneg(1)		  // error for all (n <- Nat | n != 0)
	println(resU)

	println()

	resS = lShift(128, 2)    // ok
	println(resS)
	resS = lShift(-128, 2)   // ok
	println(resS)
	resS = lShift(1024, 64)  // error, too big shift
	println(resS)
	resS = lShift(123, -2)   // error, negative shift
	println(resS)

	println()

	resU = ulShift(128, 2)   // ok
	println(resU)
	resU = ulShift(1024, 63) // ok
	println(resU)
	resU = ulShift(1024, 64) // error, too big shift
	println(resU)
	resU = ulShift(123, -2)  // error, negative shift
	println(resU)

	println()

	resS = rShift(128, 2)    // ok
	println(resS)
	resS = rShift(-128, 2)   // ok
	println(resS)
	resS = rShift(-2, 10)    // ok
	println(resS)
	resS = rShift(1024, 64)  // error, too big shift
	println(resS)
	resS = rShift(123, -2)   // error, negative shift
	println(resS)

	println()

	resU = urShift(128, 2)   // ok
	println(resU)
	resU = urShift(1, 16)    // ok
	println(resU)
	resU = urShift(1024, 64) // error, too big shift
	println(resU)
	resU = urShift(123, -2)  // error, negative shift
	println(resU)

	return 0
}
