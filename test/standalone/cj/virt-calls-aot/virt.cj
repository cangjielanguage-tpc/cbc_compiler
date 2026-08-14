import aot

func testVirt(x: aot.A): Unit {
  x.foo()
  x.foo2()
  x.foo3()
}

func testInterf(x: aot.I): Unit {
  x.bar()
  x.bar2()
  x.bar3()
}

main() {
  testVirt(aot.B())
  testInterf(aot.B())
  0
}
