import aot

public open class C <: aot.A & aot.I {
  public open func foo(): Int64 {
    println("C.foo")
    1
  }

  public open func foo3(): Int64 {
    println("C.foo3")
    3
  }

  public open func bar(): Int64 {
    println("C.bar")
    1
  }

  public open func bar3(): Int64 {
    println("C.bar3")
    3
  }
}

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
  testVirt(C())

  testInterf(aot.B())
  testInterf(C())
  0
}
