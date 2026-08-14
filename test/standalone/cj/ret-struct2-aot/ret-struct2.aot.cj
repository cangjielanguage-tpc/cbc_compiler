package aot

public func foo(x: Int64): S {
  S(42, x)
}

public struct S {
  public S(public let i: Int64, public let j: Int64) {}
}
