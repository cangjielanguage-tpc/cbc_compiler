open class C1 {
  public func foo() {
    println("C1.foo")
  }
}

interface I1 {
  func foo() {
    println("I1.foo")
  }
}

class C2 <: C1 & I1 {
  public func bar() {
    super.foo()
  }
}

main() {
  C2().bar()
}
