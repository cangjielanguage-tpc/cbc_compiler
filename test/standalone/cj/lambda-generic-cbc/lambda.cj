
func foo(f: (Int64) -> Unit): Unit {
  bar(f)(1)
  bar(f)(2)
  bar(f)(3)
}

func bar<T>(f: (T) -> Unit): (T) -> Unit {
  { x => f(x) }
}

func test1(): Unit {
  foo({ x => println(x) })
}

func test2(y: Int64): Unit {
  foo({ x => println(x + y) })
}

main() {
  test1()
  test2(42)
  0
}
