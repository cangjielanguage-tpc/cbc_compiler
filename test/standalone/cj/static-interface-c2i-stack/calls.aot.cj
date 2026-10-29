package aot

public interface Operation {
  static func calculate(
    a: Int64, b: Int64, c: Int64, d: Int64, e: Int64, f: Int64, g: Int64, h: Int64, i: Int64,
    p: Float64, q: Float64, r: Float64, s: Float64, t: Float64,
    u: Float64, v: Float64, w: Float64, x: Float64): Int64
}

public func invoke<M>(): Int64 where M <: Operation {
  // Exhaust integer and FP argument registers so the hidden type information spills.
  M.calculate(1, 2, 3, 4, 5, 6, 7, 8, 9, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
}
