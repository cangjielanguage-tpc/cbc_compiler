import aot.*


class CBCInheritor <: Super {
  public func foo():String {
    println("CBCInheritor.foo")
    "C2I returned string"
  }

  public func bar(): Unit {
    println("CBCInheritor.bar")
  }
}

func test1(): Unit {
  let obj = CBCInheritor()
  applyFoo(obj)
  applyBar(obj)
}

main() {
  test1()
  0
}
