package c

import a
import b

public func testA(x: a.A) {
  x.foo()
}

public func testB(x: b.B) {
  x.foo()
}

public func testCA(x: CA) {
  testA(x)
}

public func testCB(x: CB) {
  testB(x)
}

public open class CA <: a.A {
  public open func bar() {
    println("CA.bar")
  }
}

public open class CB <: b.B {
  public open func bar() {
    println("CB.bar")
  }
}

