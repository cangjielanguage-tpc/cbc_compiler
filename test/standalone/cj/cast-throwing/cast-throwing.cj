func expectTrue(name: String, cond: Bool): Unit {
    if (cond) {
        println("${name}: ok")
    } else {
        println("${name}: FAIL (unexpected value)")
    }
}

func expectThrowsF64ToI8(name: String, v: Float64): Unit {
    try {
        f64ToI8(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF64ToI16(name: String, v: Float64): Unit {
    try {
        f64ToI16(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF64ToI32(name: String, v: Float64): Unit {
    try {
        f64ToI32(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF64ToI64(name: String, v: Float64): Unit {
    try {
        f64ToI64(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF64ToU8(name: String, v: Float64): Unit {
    try {
        f64ToU8(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF64ToU16(name: String, v: Float64): Unit {
    try {
        f64ToU16(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF64ToU32(name: String, v: Float64): Unit {
    try {
        f64ToU32(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF64ToU64(name: String, v: Float64): Unit {
    try {
        f64ToU64(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF32ToI8(name: String, v: Float32): Unit {
    try {
        f32ToI8(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF32ToI32(name: String, v: Float32): Unit {
    try {
        f32ToI32(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsF32ToU16(name: String, v: Float32): Unit {
    try {
        f32ToU16(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsI64ToI8(name: String, v: Int64): Unit {
    try {
        i64ToI8(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsU64ToU8(name: String, v: UInt64): Unit {
    try {
        u64ToU8(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsI64ToU8(name: String, v: Int64): Unit {
    try {
        i64ToU8(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsI64ToU16(name: String, v: Int64): Unit {
    try {
        i64ToU16(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsI32ToU8(name: String, v: Int32): Unit {
    try {
        i32ToU8(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsU64ToI8(name: String, v: UInt64): Unit {
    try {
        u64ToI8(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsI16ToU16(name: String, v: Int16): Unit {
    try {
        i16ToU16(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectThrowsU16ToI16(name: String, v: UInt16): Unit {
    try {
        u16ToI16(v)
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

// ---------- Float64 -> signed integers ----------

@OverflowThrowing
func f64ToI8(v: Float64): Int8 { Int8(v) }

@OverflowThrowing
func f64ToI16(v: Float64): Int16 { Int16(v) }

@OverflowThrowing
func f64ToI32(v: Float64): Int32 { Int32(v) }

@OverflowThrowing
func f64ToI64(v: Float64): Int64 { Int64(v) }

// ---------- Float64 -> unsigned integers ----------

@OverflowThrowing
func f64ToU8(v: Float64): UInt8 { UInt8(v) }

@OverflowThrowing
func f64ToU16(v: Float64): UInt16 { UInt16(v) }

@OverflowThrowing
func f64ToU32(v: Float64): UInt32 { UInt32(v) }

@OverflowThrowing
func f64ToU64(v: Float64): UInt64 { UInt64(v) }

// ---------- Float32 -> integers ----------

@OverflowThrowing
func f32ToI8(v: Float32): Int8 { Int8(v) }

@OverflowThrowing
func f32ToI32(v: Float32): Int32 { Int32(v) }

@OverflowThrowing
func f32ToU16(v: Float32): UInt16 { UInt16(v) }

// ---------- Integer -> integer ----------

@OverflowThrowing
func i64ToI8(v: Int64): Int8 { Int8(v) }

@OverflowThrowing
func u64ToU8(v: UInt64): UInt8 { UInt8(v) }

@OverflowThrowing
func i64ToU8(v: Int64): UInt8 { UInt8(v) }

@OverflowThrowing
func i64ToU16(v: Int64): UInt16 { UInt16(v) }

@OverflowThrowing
func i32ToU8(v: Int32): UInt8 { UInt8(v) }

@OverflowThrowing
func u64ToI8(v: UInt64): Int8 { Int8(v) }

@OverflowThrowing
func i16ToU16(v: Int16): UInt16 { UInt16(v) }

@OverflowThrowing
func u16ToI16(v: UInt16): Int16 { Int16(v) }

@OverflowThrowing
func i8ToI64(v: Int8): Int64 { Int64(v) }

@OverflowThrowing
func u8ToU64(v: UInt8): UInt64 { UInt64(v) }

func f64ToSignedChecks(): Unit {
    // In range: truncate towards zero, never round.
    expectTrue("f64->i8 42.75", f64ToI8(42.75) == 42)
    expectTrue("f64->i8 -42.75", f64ToI8(-42.75) == -42)
    expectTrue("f64->i8 127.9999", f64ToI8(127.9999) == 127)
    expectTrue("f64->i8 -128.9999", f64ToI8(-128.9999) == -128)
    expectTrue("f64->i8 -0.5", f64ToI8(-0.5) == 0)
    expectTrue("f64->i8 min", f64ToI8(-128.0) == -128)
    expectTrue("f64->i8 max", f64ToI8(127.0) == 127)
    expectTrue("f64->i16 min", f64ToI16(-32768.0) == -32768)
    expectTrue("f64->i16 max", f64ToI16(32767.0) == 32767)
    expectTrue("f64->i32 min", f64ToI32(-2147483648.0) == -2147483648)
    expectTrue("f64->i32 max", f64ToI32(2147483647.0) == 2147483647)
    expectTrue("f64->i64 min", f64ToI64(-9223372036854775808.0) == -9223372036854775808)
    expectTrue("f64->i64 near max", f64ToI64(9223372036854774784.0) == 9223372036854774784)

    // Out of range.
    expectThrowsF64ToI8("f64->i8 128.0", 128.0)
    expectThrowsF64ToI8("f64->i8 -129.0", -129.0)
    expectThrowsF64ToI16("f64->i16 32768.0", 32768.0)
    expectThrowsF64ToI16("f64->i16 -32769.0", -32769.0)
    expectThrowsF64ToI32("f64->i32 2^31", 2147483648.0)
    expectThrowsF64ToI64("f64->i64 2^63", 9223372036854775808.0)

    // NaN and infinities are never representable in an integer.
    expectThrowsF64ToI8("f64->i8 NaN", Float64.NaN)
    expectThrowsF64ToI8("f64->i8 +Inf", Float64.Inf)
    expectThrowsF64ToI8("f64->i8 -Inf", -Float64.Inf)
    expectThrowsF64ToI64("f64->i64 NaN", Float64.NaN)
    expectThrowsF64ToI64("f64->i64 +Inf", Float64.Inf)
    expectThrowsF64ToI64("f64->i64 -Inf", -Float64.Inf)
}

func f64ToUnsignedChecks(): Unit {
    // In range.
    expectTrue("f64->u8 0.0", f64ToU8(0.0) == 0)
    expectTrue("f64->u8 -0.5", f64ToU8(-0.5) == 0)
    expectTrue("f64->u8 255.9", f64ToU8(255.9) == 255)
    expectTrue("f64->u8 255.0", f64ToU8(255.0) == 255)
    expectTrue("f64->u16 max", f64ToU16(65535.0) == 65535)
    expectTrue("f64->u32 max", f64ToU32(4294967295.0) == 4294967295)
    expectTrue("f64->u64 2^63", f64ToU64(9223372036854775808.0) == 9223372036854775808)

    // Negative and too-large values are out of range.
    expectThrowsF64ToU8("f64->u8 -1.0", -1.0)
    expectThrowsF64ToU8("f64->u8 256.0", 256.0)
    expectThrowsF64ToU16("f64->u16 -1.5", -1.5)
    expectThrowsF64ToU16("f64->u16 65536.0", 65536.0)
    expectThrowsF64ToU32("f64->u32 -1.0", -1.0)
    expectThrowsF64ToU64("f64->u64 -1.0", -1.0)
    expectThrowsF64ToU8("f64->u8 NaN", Float64.NaN)
    expectThrowsF64ToU8("f64->u8 +Inf", Float64.Inf)
    expectThrowsF64ToU8("f64->u8 -Inf", -Float64.Inf)
    expectThrowsF64ToU64("f64->u64 +Inf", Float64.Inf)
}

func f32Checks(): Unit {
    expectTrue("f32->i8 42.75", f32ToI8(42.75) == 42)
    expectTrue("f32->i8 min", f32ToI8(-128.0) == -128)
    expectTrue("f32->i8 max", f32ToI8(127.0) == 127)
    // 2^31 - 1 is not representable in Float32, so use an exactly representable value.
    expectTrue("f32->i32 max", f32ToI32(2147483520.0) == 2147483520)
    expectTrue("f32->u16 max", f32ToU16(65535.0) == 65535)

    expectThrowsF32ToI8("f32->i8 128.0", 128.0)
    expectThrowsF32ToI8("f32->i8 NaN", Float32.NaN)
    expectThrowsF32ToI8("f32->i8 +Inf", Float32.Inf)
    expectThrowsF32ToI8("f32->i8 -Inf", -Float32.Inf)
    expectThrowsF32ToI32("f32->i32 2^31", 2147483648.0)
    expectThrowsF32ToU16("f32->u16 -1.0", -1.0)
    expectThrowsF32ToU16("f32->u16 65536.0", 65536.0)
    expectThrowsF32ToU16("f32->u16 -Inf", -Float32.Inf)
}

func intToIntChecks(): Unit {
    // Signed -> signed narrowing.
    expectTrue("i64->i8 127", i64ToI8(127) == 127)
    expectTrue("i64->i8 -128", i64ToI8(-128) == -128)
    expectTrue("i64->i8 0", i64ToI8(0) == 0)
    expectThrowsI64ToI8("i64->i8 128", 128)
    expectThrowsI64ToI8("i64->i8 -129", -129)

    // Unsigned -> unsigned narrowing.
    expectTrue("u64->u8 255", u64ToU8(255) == 255)
    expectTrue("u64->u8 0", u64ToU8(0) == 0)
    expectThrowsU64ToU8("u64->u8 256", 256)
    expectThrowsU64ToU8("u64->u8 max", 18446744073709551615)

    // Signed -> unsigned: negatives are out of range.
    expectTrue("i64->u8 255", i64ToU8(255) == 255)
    expectTrue("i64->u8 0", i64ToU8(0) == 0)
    expectThrowsI64ToU8("i64->u8 -1", -1)
    expectThrowsI64ToU8("i64->u8 256", 256)

    expectTrue("i64->u16 65535", i64ToU16(65535) == 65535)
    expectThrowsI64ToU16("i64->u16 -1", -1)
    expectThrowsI64ToU16("i64->u16 65536", 65536)

    expectTrue("i32->u8 200", i32ToU8(200) == 200)
    expectThrowsI32ToU8("i32->u8 -1", -1)
    expectThrowsI32ToU8("i32->u8 300", 300)

    // Unsigned -> signed: values above the signed maximum are out of range.
    expectTrue("u64->i8 127", u64ToI8(127) == 127)
    expectTrue("u64->i8 0", u64ToI8(0) == 0)
    expectThrowsU64ToI8("u64->i8 128", 128)
    expectThrowsU64ToI8("u64->i8 max", 18446744073709551615)

    // Same width, different signedness.
    expectTrue("i16->u16 32767", i16ToU16(32767) == 32767)
    expectTrue("i16->u16 0", i16ToU16(0) == 0)
    expectThrowsI16ToU16("i16->u16 -1", -1)

    expectTrue("u16->i16 32767", u16ToI16(32767) == 32767)
    expectThrowsU16ToI16("u16->i16 32768", 32768)
    expectThrowsU16ToI16("u16->i16 max", 65535)

    // Widening with the same signedness is never checked, so never throws.
    expectTrue("i8->i64 -128", i8ToI64(-128) == -128)
    expectTrue("i8->i64 127", i8ToI64(127) == 127)
    expectTrue("u8->u64 255", u8ToU64(255) == 255)
    expectTrue("u8->u64 0", u8ToU64(0) == 0)
}

main(): Int64 {
    f64ToSignedChecks()
    f64ToUnsignedChecks()
    f32Checks()
    intToIntChecks()
    return 0
}
