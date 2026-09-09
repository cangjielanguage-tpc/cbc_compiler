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

public struct CA <: a.A {
}

public struct CB <: b.B {
}

