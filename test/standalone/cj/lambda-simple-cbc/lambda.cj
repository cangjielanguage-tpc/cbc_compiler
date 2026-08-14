
func foo(f: (Int64) -> Unit): Unit {
  f(1)
  f(2)
  f(3)
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
