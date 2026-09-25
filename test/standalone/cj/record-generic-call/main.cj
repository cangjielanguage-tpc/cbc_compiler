struct SA<T> {
  SA(let x: T) {}
  func foo(x: Int64) {
    println(x)
    this
  }
}
main(): Unit {
  SA<Int64>(1).foo(42)
}
