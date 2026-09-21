func foo<T>(x: T): (T, T) {
  func test<T>(y: T): T {
    y
  }
  test((x, x))
}

main() {
  let (a, b) = foo<Int64>(42)
  println(a)
  println(b)
  let (c, d) = foo<String>("foo")
  println(c)
  println(d)
}
