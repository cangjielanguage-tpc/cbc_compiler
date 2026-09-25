interface I<T> {
  func foo(x: T): Unit
}

struct R <: I<Int64> {
  R(var i: Int64) {}

  public func foo(x: Int64): Unit {
    println("R.foo(${x})")
  }
}

func bar(a: R) {
  var b: I<Int64> = a
  b.foo(1)
}

main() {
  bar(R(111))
}
