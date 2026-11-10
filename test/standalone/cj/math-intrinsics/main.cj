import std.math.*

func check(x: Float64, n: Int64): Unit {
  println("log_10 (${x}) = ${log10(x)}")
  println("log_2 (${x}) = ${log2(x)}")
  println("exp (${x}) = ${exp(x)}")
  println("exp_2 (${x}) = ${exp2(x)}")
  println("trunc (${x}) = ${trunc(x)}")
  println("pow (${x}, ${n}) = ${pow(x, n)}")
}

main(): Int64 {
  check(100.0, 10)
  check(2.0, -2)
  check(0.0, 5)
  check(-2.0, 3)
  check(1.0e300, 2)
  check(0.5, 0)
  0
}
