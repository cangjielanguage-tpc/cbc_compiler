import std.math.*

func check32(x: Float32, exponent: Int32): Unit {
    let tolerance = 0.00001f32
    println(abs(sqrt(x) - 1.5f32) < tolerance)
    println(abs(sin(x) - 0.778073196887921f32) < tolerance)
    println(abs(cos(x) + 0.628173622722739f32) < tolerance)
    println(abs(log(x) - 0.810930216216329f32) < tolerance)
    println(abs(log10(x) - 0.352182518111362f32) < tolerance)
    println(abs(log2(x) - 1.169925001442312f32) < tolerance)
    println(abs(exp(x) - 9.487735836358526f32) < tolerance)
    println(abs(exp2(x) - 4.756828460010884f32) < tolerance)
    println(ceil(x) == 3.0f32 && ceil(-x) == -2.0f32)
    println(floor(x) == 2.0f32 && floor(-x) == -3.0f32)
    println(trunc(x) == 2.0f32 && trunc(-x) == -2.0f32)
    println(abs(-x) == x)
    println(abs(pow(x, 2.0f32) - 5.0625f32) < tolerance)
    println(abs(pow(x, exponent) - 0.197530864197531f32) < tolerance)
}

func check64(x: Float64): Unit {
    let tolerance = 0.000000000001
    println(abs(sqrt(x) - 1.5) < tolerance)
    println(abs(sin(x) - 0.778073196887921) < tolerance)
    println(abs(cos(x) + 0.628173622722739) < tolerance)
    println(abs(log(x) - 0.810930216216329) < tolerance)
    println(abs(log10(x) - 0.352182518111362) < tolerance)
    println(abs(log2(x) - 1.169925001442312) < tolerance)
    println(abs(exp(x) - 9.487735836358526) < tolerance)
    println(abs(exp2(x) - 4.756828460010884) < tolerance)
    println(ceil(x) == 3.0 && ceil(-x) == -2.0)
    println(floor(x) == 2.0 && floor(-x) == -3.0)
    println(trunc(x) == 2.0 && trunc(-x) == -2.0)
    println(abs(-x) == x)
    println(abs(pow(x, 2.0) - 5.0625) < tolerance)
}

func check16(x: Float16): Unit {
    let tolerance = 0.02f32
    println(abs(Float32(sqrt(x)) - 1.5f32) < tolerance)
    println(abs(Float32(sin(x)) - 0.7780732f32) < tolerance)
    println(abs(Float32(cos(x)) + 0.6281736f32) < tolerance)
    println(abs(Float32(log(x)) - 0.8109302f32) < tolerance)
    println(abs(Float32(log10(x)) - 0.3521825f32) < tolerance)
    println(abs(Float32(log2(x)) - 1.169925f32) < tolerance)
    println(abs(Float32(exp(x)) - 9.487736f32) < tolerance)
    println(abs(Float32(exp2(x)) - 4.756828f32) < tolerance)
    println(ceil(x) == 3.0f16 && ceil(-x) == -2.0f16)
    println(floor(x) == 2.0f16 && floor(-x) == -3.0f16)
    println(trunc(x) == 2.0f16 && trunc(-x) == -2.0f16)
    println(abs(-x) == x)
}

main(): Int64 {
    check16(2.25f16)
    check32(2.25f32, -2i32)
    check64(2.25)
    return 0
}
