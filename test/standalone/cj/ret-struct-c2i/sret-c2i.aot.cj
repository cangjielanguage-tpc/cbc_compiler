package aot

public open class Super {
  public open func foo(): String {
    println("Super.foo")
    "AOT string"
  }

  public open func bar(): Unit {
    println("Super.bar")
    "AOT string"
  }
}

public func applyFoo(x: Super) {
  let sret = x.foo()
  println(sret)
}

public func applyBar(x: Super) {
  let sret = x.bar()
  println(sret)
}

