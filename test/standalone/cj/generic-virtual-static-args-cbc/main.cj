// Both type-info arguments spill after integer and floating-point arguments.
interface Operation<T> {
  static func calculate(value: T,
    a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64,
    p: Float64, q: Float64, r: Float64, s: Float64, t: Float64,
    u: Float64, v: Float64, w: Float64, x: Float64): T
}

class IntOperation <: Operation<Int64> {
  public static func calculate(value: Int64,
    a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64,
    p: Float64, q: Float64, r: Float64, s: Float64, t: Float64,
    u: Float64, v: Float64, w: Float64, x: Float64): Int64 {
    value + a + b + c + d + e + f + g + h + Int64(p + q + r + s + t + u + v + w + x)
  }
}

class Invoker<M, T> where M <: Operation<T> {
  public init() {}

  public func invoke(value: T): T {
    M.calculate(value, 1, 2, 3, 4, 5, 6, 7, 8, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
  }
}

main(): Int64 {
  if (Invoker<IntOperation, Int64>().invoke(100) != 181) { return 1 }
  0
}
