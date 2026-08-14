import aot.*

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
