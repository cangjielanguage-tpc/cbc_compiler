interface I<T> {
  func f1(a: T): Unit
  func f2(a: T): Unit {
    f1(a)
  }
}
class A <: I<Int64> & I<String> {
  public func f1(a: Int64): Unit {
    println("a")
  }
  public func f1(a: String): Unit {
    println(a)
  }
}

func foo(a: A) {
  a.f2("1")
}

main() {
  foo(A())
}
