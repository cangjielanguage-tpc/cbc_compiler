import std.math.*

// Keep variable operands so the CHIR contains log intrinsics, not just constants.
func check32(x: Float32, base: Float32): Unit {
    let result: Float32 = log(x) / log(base)
    println(abs(result - 3.0f32) < 0.00001f32)
    println(abs(logBase(x, base) - result) < 0.00001f32)
}

func check64(x: Float64, base: Float64): Unit {
    let result: Float64 = log(x) / log(base)
    println(abs(result - 3.0) < 0.000000000001)
    println(abs(logBase(x, base) - result) < 0.000000000001)
}

func check16(x: Float16, base: Float16): Unit {
    let result: Float16 = log(x) / log(base)
    println(abs(Float32(result) - 3.0f32) < 0.01f32)
    println(abs(Float32(logBase(x, base)) - Float32(result)) < 0.01f32)
}

main(): Int64 {
    check32(8.0f32, 2.0f32)
    check64(8.0, 2.0)
    check16(8.0f16, 2.0f16)
    return 0
}
