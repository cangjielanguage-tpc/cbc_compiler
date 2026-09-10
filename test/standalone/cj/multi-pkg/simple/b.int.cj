package b

import a

public func bar(x: Int64, y: Int64): Int64 {
  println("bar(${x},${y})")
  a.foo(x) + a.foo(y)
}

