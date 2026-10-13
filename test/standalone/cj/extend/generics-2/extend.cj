interface Eqq {
  func foo(): Unit {
    println("Eqq.foo")
  }
}
struct R<T> {
  public func add(): Unit {
    println("R.add")
  }
}
extend<T> R<T> <: Eqq {
  public func foo(): Unit {
    add()
  }
}
main() {
  let a: Eqq = R<Int64>();
  a.foo()
}
