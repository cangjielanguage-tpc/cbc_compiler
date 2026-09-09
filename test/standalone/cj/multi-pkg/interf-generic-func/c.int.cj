package c

import a
import b

public func testA<T>(x: a.A<T>) {
  x.foo<T>()
}

public func testB<T>(x: b.B<T>) {
  x.foo<T>()
}

public func testCA(x: CA) {
  testA(x)
}

public func testCB(x: CB) {
  testB(x)
}

public open class CA <: a.A<String> {
  public open func bar<S>() {
    println("CA.bar")
  }
}

public open class CB <: b.B<Int64> {
  public open func bar<S>() {
    println("CB.bar")
  }
}

