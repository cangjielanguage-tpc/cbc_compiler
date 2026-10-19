public open class A {
  protected open func foo() {
    1
  }
  protected static func foo1() {
    1
  }
}

public type TA = A
extend TA {
  public func me() {
    foo() + foo1()
  }
}

class B <: TA {
  func foo_b() {
    foo() + foo1() + me()
  }
}

main(): Int64 {
  B().me() + B().foo_b() - 6
}
