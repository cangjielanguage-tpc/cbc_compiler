interface I<T> {
  mut func f1(x: T): T {
    println("I.f1")
    f2(x)
  }

  mut func f2(x: T): T
}

struct S <: I<Int64> {
  public mut func f2(x: Int64): Int64 {
    println("S.f2")
    0
  }
}

main(): Unit {
  var s = S()
  s.f1(42)
}
