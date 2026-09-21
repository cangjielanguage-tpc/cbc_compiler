func foo<T>(x: T): (T, T) {
  func test<T>(y: T): (T, T) {
    func test2<T>(z: T): T {
      z
    }
    test2((y, y))
  }
  test(x)
}

main() {
  let (a, b) = foo<Int64>(42)
  println(a)
  println(b)
  let (c, d) = foo<String>("foo")
  println(c)
  println(d)
}
