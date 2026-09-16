import a
import b
import c

main() {
  c.testCA(c.CA())
  c.testCB(c.CB())
  c.testA(MA())
  c.testB(MB())
}

public struct MA <: a.A {
  public override func foo() {
    println("MA.foo")
  }
}

public struct MB <: b.B {
  public override func foo() {
    println("MB.foo")
  }
}
