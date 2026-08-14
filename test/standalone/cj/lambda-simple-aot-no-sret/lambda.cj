import aot.*

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
