func expectThrows(name: String, body: () -> Unit): Unit {
    try {
        body()
        println("${name}: FAIL (no exception)")
    } catch (e: OverflowException) {
        println("${name}: ok (OverflowException)")
    }
}

func expectTrue(name: String, cond: Bool): Unit {
    if (cond) {
        println("${name}: ok")
    } else {
        println("${name}: FAIL (unexpected value)")
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
    expectThrows("f64->i8 128.0", { => f64ToI8(128.0) })
    expectThrows("f64->i8 -129.0", { => f64ToI8(-129.0) })
    expectThrows("f64->i16 32768.0", { => f64ToI16(32768.0) })
    expectThrows("f64->i16 -32769.0", { => f64ToI16(-32769.0) })
    expectThrows("f64->i32 2^31", { => f64ToI32(2147483648.0) })
    expectThrows("f64->i64 2^63", { => f64ToI64(9223372036854775808.0) })

    // NaN and infinities are never representable in an integer.
    expectThrows("f64->i8 NaN", { => f64ToI8(Float64.NaN) })
    expectThrows("f64->i8 +Inf", { => f64ToI8(Float64.Inf) })
    expectThrows("f64->i8 -Inf", { => f64ToI8(-Float64.Inf) })
    expectThrows("f64->i64 NaN", { => f64ToI64(Float64.NaN) })
    expectThrows("f64->i64 +Inf", { => f64ToI64(Float64.Inf) })
    expectThrows("f64->i64 -Inf", { => f64ToI64(-Float64.Inf) })
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
    expectThrows("f64->u8 -1.0", { => f64ToU8(-1.0) })
    expectThrows("f64->u8 256.0", { => f64ToU8(256.0) })
    expectThrows("f64->u16 -1.5", { => f64ToU16(-1.5) })
    expectThrows("f64->u16 65536.0", { => f64ToU16(65536.0) })
    expectThrows("f64->u32 -1.0", { => f64ToU32(-1.0) })
    expectThrows("f64->u64 -1.0", { => f64ToU64(-1.0) })
    expectThrows("f64->u8 NaN", { => f64ToU8(Float64.NaN) })
    expectThrows("f64->u8 +Inf", { => f64ToU8(Float64.Inf) })
    expectThrows("f64->u8 -Inf", { => f64ToU8(-Float64.Inf) })
    expectThrows("f64->u64 +Inf", { => f64ToU64(Float64.Inf) })
}

func f32Checks(): Unit {
    expectTrue("f32->i8 42.75", f32ToI8(42.75) == 42)
    expectTrue("f32->i8 min", f32ToI8(-128.0) == -128)
    expectTrue("f32->i8 max", f32ToI8(127.0) == 127)
    // 2^31 - 1 is not representable in Float32, so use an exactly representable value.
    expectTrue("f32->i32 max", f32ToI32(2147483520.0) == 2147483520)
    expectTrue("f32->u16 max", f32ToU16(65535.0) == 65535)

    expectThrows("f32->i8 128.0", { => f32ToI8(128.0) })
    expectThrows("f32->i8 NaN", { => f32ToI8(Float32.NaN) })
    expectThrows("f32->i8 +Inf", { => f32ToI8(Float32.Inf) })
    expectThrows("f32->i8 -Inf", { => f32ToI8(-Float32.Inf) })
    expectThrows("f32->i32 2^31", { => f32ToI32(2147483648.0) })
    expectThrows("f32->u16 -1.0", { => f32ToU16(-1.0) })
    expectThrows("f32->u16 65536.0", { => f32ToU16(65536.0) })
    expectThrows("f32->u16 -Inf", { => f32ToU16(-Float32.Inf) })
}

func intToIntChecks(): Unit {
    // Signed -> signed narrowing.
    expectTrue("i64->i8 127", i64ToI8(127) == 127)
    expectTrue("i64->i8 -128", i64ToI8(-128) == -128)
    expectTrue("i64->i8 0", i64ToI8(0) == 0)
    expectThrows("i64->i8 128", { => i64ToI8(128) })
    expectThrows("i64->i8 -129", { => i64ToI8(-129) })

    // Unsigned -> unsigned narrowing.
    expectTrue("u64->u8 255", u64ToU8(255) == 255)
    expectTrue("u64->u8 0", u64ToU8(0) == 0)
    expectThrows("u64->u8 256", { => u64ToU8(256) })
    expectThrows("u64->u8 max", { => u64ToU8(18446744073709551615) })

    // Signed -> unsigned: negatives are out of range.
    expectTrue("i64->u8 255", i64ToU8(255) == 255)
    expectTrue("i64->u8 0", i64ToU8(0) == 0)
    expectThrows("i64->u8 -1", { => i64ToU8(-1) })
    expectThrows("i64->u8 256", { => i64ToU8(256) })

    expectTrue("i64->u16 65535", i64ToU16(65535) == 65535)
    expectThrows("i64->u16 -1", { => i64ToU16(-1) })
    expectThrows("i64->u16 65536", { => i64ToU16(65536) })

    expectTrue("i32->u8 200", i32ToU8(200) == 200)
    expectThrows("i32->u8 -1", { => i32ToU8(-1) })
    expectThrows("i32->u8 300", { => i32ToU8(300) })

    // Unsigned -> signed: values above the signed maximum are out of range.
    expectTrue("u64->i8 127", u64ToI8(127) == 127)
    expectTrue("u64->i8 0", u64ToI8(0) == 0)
    expectThrows("u64->i8 128", { => u64ToI8(128) })
    expectThrows("u64->i8 max", { => u64ToI8(18446744073709551615) })

    // Same width, different signedness.
    expectTrue("i16->u16 32767", i16ToU16(32767) == 32767)
    expectTrue("i16->u16 0", i16ToU16(0) == 0)
    expectThrows("i16->u16 -1", { => i16ToU16(-1) })

    expectTrue("u16->i16 32767", u16ToI16(32767) == 32767)
    expectThrows("u16->i16 32768", { => u16ToI16(32768) })
    expectThrows("u16->i16 max", { => u16ToI16(65535) })

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
