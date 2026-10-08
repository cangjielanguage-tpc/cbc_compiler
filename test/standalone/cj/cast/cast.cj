func int32ToInt64(value: Int32) {
    println("i32_i64: ${Int64(value)}")
}

@OverflowWrapping
func toU8F32(value: Float32): UInt8 {
    UInt8(value)
}

@OverflowWrapping
func toU8F64(value: Float64): UInt8 {
    UInt8(value)
}

@OverflowWrapping
func toU16F32(value: Float32): UInt16 {
    UInt16(value)
}

@OverflowWrapping
func toU16F64(value: Float64): UInt16 {
    UInt16(value)
}

@OverflowWrapping
func toU32F32(value: Float32): UInt32 {
    UInt32(value)
}

@OverflowWrapping
func toU32F64(value: Float64): UInt32 {
    UInt32(value)
}

@OverflowWrapping
func toU64F32(value: Float32): UInt64 {
    UInt64(value)
}

@OverflowWrapping
func toU64F64(value: Float64): UInt64 {
    UInt64(value)
}

@OverflowWrapping
func toI8F32(value: Float32): Int8 {
    Int8(value)
}

@OverflowWrapping
func toI8F64(value: Float64): Int8 {
    Int8(value)
}

@OverflowWrapping
func toI16F32(value: Float32): Int16 {
    Int16(value)
}

@OverflowWrapping
func toI16F64(value: Float64): Int16 {
    Int16(value)
}

@OverflowWrapping
func toI32F32(value: Float32): Int32 {
    Int32(value)
}

@OverflowWrapping
func toI32F64(value: Float64): Int32 {
    Int32(value)
}

@OverflowWrapping
func toI64F32(value: Float32): Int64 {
    Int64(value)
}

@OverflowWrapping
func toI64F64(value: Float64): Int64 {
    Int64(value)
}

@OverflowWrapping
func floatToInteger() {
    println("f32_u8_zero: ${UInt64(toU8F32(0.0))}")
    println("f32_u8_fraction: ${UInt64(toU8F32(42.75))}")
    println("f32_u8_negative: ${UInt64(toU8F32(-10.5))}")
    println("f32_u8_max: ${UInt64(toU8F32(Float32(UInt8.Max)))}")
    println("f32_u8_overflow: ${UInt64(toU8F32(0x1p8))}")
    println("f32_u8_positive_infinity: ${UInt64(toU8F32(Float32.Inf))}")
    println("f32_u8_negative_infinity: ${UInt64(toU8F32(-Float32.Inf))}")
    println("f32_u8_nan: ${UInt64(toU8F32(Float32.NaN))}")
    println("f64_u8_zero: ${UInt64(toU8F64(0.0))}")
    println("f64_u8_fraction: ${UInt64(toU8F64(42.75))}")
    println("f64_u8_negative: ${UInt64(toU8F64(-10.5))}")
    println("f64_u8_max: ${UInt64(toU8F64(Float64(UInt8.Max)))}")
    println("f64_u8_overflow: ${UInt64(toU8F64(0x1p8))}")
    println("f64_u8_positive_infinity: ${UInt64(toU8F64(Float64.Inf))}")
    println("f64_u8_negative_infinity: ${UInt64(toU8F64(-Float64.Inf))}")
    println("f64_u8_nan: ${UInt64(toU8F64(Float64.NaN))}")
    println("f32_u16_zero: ${UInt64(toU16F32(0.0))}")
    println("f32_u16_fraction: ${UInt64(toU16F32(42.75))}")
    println("f32_u16_negative: ${UInt64(toU16F32(-10.5))}")
    println("f32_u16_max: ${UInt64(toU16F32(Float32(UInt16.Max)))}")
    println("f32_u16_overflow: ${UInt64(toU16F32(0x1p16))}")
    println("f32_u16_positive_infinity: ${UInt64(toU16F32(Float32.Inf))}")
    println("f32_u16_negative_infinity: ${UInt64(toU16F32(-Float32.Inf))}")
    println("f32_u16_nan: ${UInt64(toU16F32(Float32.NaN))}")
    println("f64_u16_zero: ${UInt64(toU16F64(0.0))}")
    println("f64_u16_fraction: ${UInt64(toU16F64(42.75))}")
    println("f64_u16_negative: ${UInt64(toU16F64(-10.5))}")
    println("f64_u16_max: ${UInt64(toU16F64(Float64(UInt16.Max)))}")
    println("f64_u16_overflow: ${UInt64(toU16F64(0x1p16))}")
    println("f64_u16_positive_infinity: ${UInt64(toU16F64(Float64.Inf))}")
    println("f64_u16_negative_infinity: ${UInt64(toU16F64(-Float64.Inf))}")
    println("f64_u16_nan: ${UInt64(toU16F64(Float64.NaN))}")
    println("f32_u32_zero: ${UInt64(toU32F32(0.0))}")
    println("f32_u32_fraction: ${UInt64(toU32F32(42.75))}")
    println("f32_u32_negative: ${UInt64(toU32F32(-10.5))}")
    println("f32_u32_max: ${UInt64(toU32F32(Float32(UInt32.Max)))}")
    println("f32_u32_overflow: ${UInt64(toU32F32(0x1p32))}")
    println("f32_u32_positive_infinity: ${UInt64(toU32F32(Float32.Inf))}")
    println("f32_u32_negative_infinity: ${UInt64(toU32F32(-Float32.Inf))}")
    println("f32_u32_nan: ${UInt64(toU32F32(Float32.NaN))}")
    println("f64_u32_zero: ${UInt64(toU32F64(0.0))}")
    println("f64_u32_fraction: ${UInt64(toU32F64(42.75))}")
    println("f64_u32_negative: ${UInt64(toU32F64(-10.5))}")
    println("f64_u32_max: ${UInt64(toU32F64(Float64(UInt32.Max)))}")
    println("f64_u32_overflow: ${UInt64(toU32F64(0x1p32))}")
    println("f64_u32_positive_infinity: ${UInt64(toU32F64(Float64.Inf))}")
    println("f64_u32_negative_infinity: ${UInt64(toU32F64(-Float64.Inf))}")
    println("f64_u32_nan: ${UInt64(toU32F64(Float64.NaN))}")
    println("f32_u64_zero: ${UInt64(toU64F32(0.0))}")
    println("f32_u64_fraction: ${UInt64(toU64F32(42.75))}")
    println("f32_u64_negative: ${UInt64(toU64F32(-10.5))}")
    println("f32_u64_max: ${UInt64(toU64F32(Float32(UInt64.Max)))}")
    println("f32_u64_overflow: ${UInt64(toU64F32(0x1p64))}")
    println("f32_u64_positive_infinity: ${UInt64(toU64F32(Float32.Inf))}")
    println("f32_u64_negative_infinity: ${UInt64(toU64F32(-Float32.Inf))}")
    println("f32_u64_nan: ${UInt64(toU64F32(Float32.NaN))}")
    println("f64_u64_zero: ${UInt64(toU64F64(0.0))}")
    println("f64_u64_fraction: ${UInt64(toU64F64(42.75))}")
    println("f64_u64_negative: ${UInt64(toU64F64(-10.5))}")
    println("f64_u64_max: ${UInt64(toU64F64(Float64(UInt64.Max)))}")
    println("f64_u64_overflow: ${UInt64(toU64F64(0x1p64))}")
    println("f64_u64_positive_infinity: ${UInt64(toU64F64(Float64.Inf))}")
    println("f64_u64_negative_infinity: ${UInt64(toU64F64(-Float64.Inf))}")
    println("f64_u64_nan: ${UInt64(toU64F64(Float64.NaN))}")
    println("f32_i8_max_fraction: ${Int64(toI8F32(127.75))}")
    println("f32_i8_min_fraction: ${Int64(toI8F32(-128.75))}")
    println("f64_i8_max_fraction: ${Int64(toI8F64(127.75))}")
    println("f64_i8_min_fraction: ${Int64(toI8F64(-128.75))}")
    println("f32_i16_max_fraction: ${Int64(toI16F32(32767.75))}")
    println("f32_i16_min_fraction: ${Int64(toI16F32(-32768.75))}")
    println("f64_i16_max_fraction: ${Int64(toI16F64(32767.75))}")
    println("f64_i16_min_fraction: ${Int64(toI16F64(-32768.75))}")
    println("f64_i32_max_fraction: ${Int64(toI32F64(2147483647.75))}")
    println("f64_i32_min_fraction: ${Int64(toI32F64(-2147483648.75))}")
    println("f32_i32_one: ${Int64(toI32F32(1.0))}")
    println("f64_i32_one: ${Int64(toI32F64(1.0))}")
    println("f32_i64_one: ${Int64(toI64F32(1.0))}")
    println("f64_i64_one: ${Int64(toI64F64(1.0))}")
    println("f32_i8_zero: ${Int64(toI8F32(0.0))}")
    println("f32_i8_negative_zero: ${Int64(toI8F32(-0.0))}")
    println("f32_i8_positive_fraction: ${Int64(toI8F32(42.75))}")
    println("f32_i8_negative_fraction: ${Int64(toI8F32(-42.75))}")
    println("f32_i8_max: ${Int64(toI8F32(Float32(Int8.Max)))}")
    println("f32_i8_min: ${Int64(toI8F32(Float32(Int8.Min)))}")
    println("f32_i8_overflow: ${Int64(toI8F32(0x1p7))}")
    println("f32_i8_underflow: ${Int64(toI8F32(-0x1p8))}")
    println("f32_i8_positive_infinity: ${Int64(toI8F32(Float32.Inf))}")
    println("f32_i8_negative_infinity: ${Int64(toI8F32(-Float32.Inf))}")
    println("f32_i8_nan: ${Int64(toI8F32(Float32.NaN))}")
    println("f64_i8_zero: ${Int64(toI8F64(0.0))}")
    println("f64_i8_negative_zero: ${Int64(toI8F64(-0.0))}")
    println("f64_i8_positive_fraction: ${Int64(toI8F64(42.75))}")
    println("f64_i8_negative_fraction: ${Int64(toI8F64(-42.75))}")
    println("f64_i8_max: ${Int64(toI8F64(Float64(Int8.Max)))}")
    println("f64_i8_min: ${Int64(toI8F64(Float64(Int8.Min)))}")
    println("f64_i8_overflow: ${Int64(toI8F64(0x1p7))}")
    println("f64_i8_underflow: ${Int64(toI8F64(-0x1p8))}")
    println("f64_i8_positive_infinity: ${Int64(toI8F64(Float64.Inf))}")
    println("f64_i8_negative_infinity: ${Int64(toI8F64(-Float64.Inf))}")
    println("f64_i8_nan: ${Int64(toI8F64(Float64.NaN))}")
    println("f32_i16_zero: ${Int64(toI16F32(0.0))}")
    println("f32_i16_negative_zero: ${Int64(toI16F32(-0.0))}")
    println("f32_i16_positive_fraction: ${Int64(toI16F32(42.75))}")
    println("f32_i16_negative_fraction: ${Int64(toI16F32(-42.75))}")
    println("f32_i16_max: ${Int64(toI16F32(Float32(Int16.Max)))}")
    println("f32_i16_min: ${Int64(toI16F32(Float32(Int16.Min)))}")
    println("f32_i16_overflow: ${Int64(toI16F32(0x1p15))}")
    println("f32_i16_underflow: ${Int64(toI16F32(-0x1p16))}")
    println("f32_i16_positive_infinity: ${Int64(toI16F32(Float32.Inf))}")
    println("f32_i16_negative_infinity: ${Int64(toI16F32(-Float32.Inf))}")
    println("f32_i16_nan: ${Int64(toI16F32(Float32.NaN))}")
    println("f64_i16_zero: ${Int64(toI16F64(0.0))}")
    println("f64_i16_negative_zero: ${Int64(toI16F64(-0.0))}")
    println("f64_i16_positive_fraction: ${Int64(toI16F64(42.75))}")
    println("f64_i16_negative_fraction: ${Int64(toI16F64(-42.75))}")
    println("f64_i16_max: ${Int64(toI16F64(Float64(Int16.Max)))}")
    println("f64_i16_min: ${Int64(toI16F64(Float64(Int16.Min)))}")
    println("f64_i16_overflow: ${Int64(toI16F64(0x1p15))}")
    println("f64_i16_underflow: ${Int64(toI16F64(-0x1p16))}")
    println("f64_i16_positive_infinity: ${Int64(toI16F64(Float64.Inf))}")
    println("f64_i16_negative_infinity: ${Int64(toI16F64(-Float64.Inf))}")
    println("f64_i16_nan: ${Int64(toI16F64(Float64.NaN))}")
    println("f32_i32_zero: ${Int64(toI32F32(0.0))}")
    println("f32_i32_negative_zero: ${Int64(toI32F32(-0.0))}")
    println("f32_i32_positive_fraction: ${Int64(toI32F32(42.75))}")
    println("f32_i32_negative_fraction: ${Int64(toI32F32(-42.75))}")
    println("f32_i32_max: ${Int64(toI32F32(Float32(Int32.Max)))}")
    println("f32_i32_min: ${Int64(toI32F32(Float32(Int32.Min)))}")
    println("f32_i32_overflow: ${Int64(toI32F32(0x1p31))}")
    println("f32_i32_underflow: ${Int64(toI32F32(-0x1p32))}")
    println("f32_i32_positive_infinity: ${Int64(toI32F32(Float32.Inf))}")
    println("f32_i32_negative_infinity: ${Int64(toI32F32(-Float32.Inf))}")
    println("f32_i32_nan: ${Int64(toI32F32(Float32.NaN))}")
    println("f32_i32_below_upper_limit: ${Int64(toI32F32(0x1.fffffep30))}")
    println("f64_i32_zero: ${Int64(toI32F64(0.0))}")
    println("f64_i32_negative_zero: ${Int64(toI32F64(-0.0))}")
    println("f64_i32_positive_fraction: ${Int64(toI32F64(42.75))}")
    println("f64_i32_negative_fraction: ${Int64(toI32F64(-42.75))}")
    println("f64_i32_max: ${Int64(toI32F64(Float64(Int32.Max)))}")
    println("f64_i32_min: ${Int64(toI32F64(Float64(Int32.Min)))}")
    println("f64_i32_overflow: ${Int64(toI32F64(0x1p31))}")
    println("f64_i32_underflow: ${Int64(toI32F64(-0x1p32))}")
    println("f64_i32_positive_infinity: ${Int64(toI32F64(Float64.Inf))}")
    println("f64_i32_negative_infinity: ${Int64(toI32F64(-Float64.Inf))}")
    println("f64_i32_nan: ${Int64(toI32F64(Float64.NaN))}")
    println("f32_i64_zero: ${Int64(toI64F32(0.0))}")
    println("f32_i64_negative_zero: ${Int64(toI64F32(-0.0))}")
    println("f32_i64_positive_fraction: ${Int64(toI64F32(42.75))}")
    println("f32_i64_negative_fraction: ${Int64(toI64F32(-42.75))}")
    println("f32_i64_max: ${Int64(toI64F32(Float32(Int64.Max)))}")
    println("f32_i64_min: ${Int64(toI64F32(Float32(Int64.Min)))}")
    println("f32_i64_overflow: ${Int64(toI64F32(0x1p63))}")
    println("f32_i64_underflow: ${Int64(toI64F32(-0x1p64))}")
    println("f32_i64_positive_infinity: ${Int64(toI64F32(Float32.Inf))}")
    println("f32_i64_negative_infinity: ${Int64(toI64F32(-Float32.Inf))}")
    println("f32_i64_nan: ${Int64(toI64F32(Float32.NaN))}")
    println("f32_i64_below_upper_limit: ${Int64(toI64F32(0x1.fffffep62))}")
    println("f64_i64_zero: ${Int64(toI64F64(0.0))}")
    println("f64_i64_negative_zero: ${Int64(toI64F64(-0.0))}")
    println("f64_i64_positive_fraction: ${Int64(toI64F64(42.75))}")
    println("f64_i64_negative_fraction: ${Int64(toI64F64(-42.75))}")
    println("f64_i64_max: ${Int64(toI64F64(Float64(Int64.Max)))}")
    println("f64_i64_min: ${Int64(toI64F64(Float64(Int64.Min)))}")
    println("f64_i64_overflow: ${Int64(toI64F64(0x1p63))}")
    println("f64_i64_underflow: ${Int64(toI64F64(-0x1p64))}")
    println("f64_i64_positive_infinity: ${Int64(toI64F64(Float64.Inf))}")
    println("f64_i64_negative_infinity: ${Int64(toI64F64(-Float64.Inf))}")
    println("f64_i64_nan: ${Int64(toI64F64(Float64.NaN))}")
    println("f64_i64_below_upper_limit: ${Int64(toI64F64(0x1.fffffffffffffp62))}")
}

main() {
    int32ToInt64(5)
    floatToInteger()
    0
}
