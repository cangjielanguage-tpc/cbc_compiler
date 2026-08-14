import aot

func testVirt(x: aot.A): Unit {
  println(x.foo())
  println(x.foo2())
  println(x.foo3())
}

func testInterf(x: aot.I): Unit {
  println(x.bar())
  println(x.bar2())
  println(x.bar3())
}

main() {
  testVirt(aot.B())
  testInterf(aot.B())
  0
}
