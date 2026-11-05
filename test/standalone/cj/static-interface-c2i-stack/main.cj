import aot.*

class WeightedSum <: Operation {
  public static func calculate(
    a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, i: Int64,
    p: Float64, q: Float64, r: Float64, s: Float64, t: Float64,
    u: Float64, v: Float64, w: Float64, x: Float64): Int64 {
    a + 2*b + 3*c + 4*d + 5*e + 6*f + 7*g + 8*h + 9*i +
      Int64(p + 2.0*q + 3.0*r + 4.0*s + 5.0*t + 6.0*u + 7.0*v + 8.0*w + 9.0*x)
  }
}

main(): Int64 {
  if (invoke<WeightedSum>() != 570) { return 1 }
  0
}
