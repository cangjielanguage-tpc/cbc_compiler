import a
import b
import c

main(): Int64 {
  c.testCA(c.CA())
  c.testCB(c.CB())
  c.testA(MA())
  c.testB(MB())
  0
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
