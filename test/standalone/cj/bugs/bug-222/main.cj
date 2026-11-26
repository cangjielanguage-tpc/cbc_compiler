package default

import std.math.*

extend Float16 {
  public static prop Max2: Float16 {
    get() {
      Float16((1.0 + Float64((Float64(1024) - 1.0) / Float64(1024))) * Float64(2.0 ** 15))
    }
  }
}
func identityF16(x: Float16): Float16 {
  Float16(x)
}

func identityF32(x: Float32): Float32 {
  Float32(x)
}

func identityF64(x: Float64): Float64 {
  Float64(x)
}

main() {
  let _ = identityF16(Float16(Float32(1.0)))
  let _ = identityF32(1.0)
  let _ = identityF64(1.0)
  println("${Float16.Max2}")
}