import aot

public open class C <: aot.A & aot.I {
  public open func foo(): Unit {
    println("C.foo")
  }

  public open func foo3(): Unit {
    println("C.foo3")
  }

  public open func bar(): Unit {
    println("C.bar")
  }

  public open func bar3(): Unit {
    println("C.bar3")
  }
}

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
  testVirt(C())

  testInterf(aot.B())
  testInterf(C())
  0
}
