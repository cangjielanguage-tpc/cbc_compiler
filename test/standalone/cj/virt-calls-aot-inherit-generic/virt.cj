import aot

public open class C <: aot.A<Int64> & aot.I<String> {
  public open func foo(x: Int64): Int64 {
    println("C.foo")
    x
  }

  public open func foo3(x: Int64): Int64 {
    println("C.foo3")
    x
  }

  public open func bar(x: String): String {
    println("C.bar")
    x
  }

  public open func bar3(x: String): String {
    println("C.bar3")
    x
  }
}

func testVirt(x: aot.A<Int64>): Unit {
  println(x.foo(42))
  println(x.foo2(37))
  println(x.foo3(123))
}

func testInterf(x: aot.I<String>): Unit {
  println(x.bar("abc"))
  println(x.bar2("def"))
  println(x.bar3("ghi"))
}

main() {
  testVirt(aot.B())
  testVirt(C())

  testInterf(aot.B())
  testInterf(C())
  0
}
