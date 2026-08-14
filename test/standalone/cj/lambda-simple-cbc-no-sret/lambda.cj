
func foo(f: (Int64) -> Int64): Unit {
  f(1)
  f(2)
  f(3)
}

func test1(): Unit {
  foo({ x => println(x); x })
}

func test2(y: Int64): Unit {
  foo({ x => println(x + y); x })
}

main() {
  test1()
  test2(42)
  0
}
