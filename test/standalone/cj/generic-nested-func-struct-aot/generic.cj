import aot.*

func foo<T>(x: T): Pair<T> {
  func test<T>(y: T): T {
    y
  }
  test(Pair(x, x))
}

main() {
  let p1 = foo<Int64>(42)
  println(p1.a)
  println(p1.b)
  let p2 = foo<String>("foo")
  println(p2.a)
  println(p2.b)
}
