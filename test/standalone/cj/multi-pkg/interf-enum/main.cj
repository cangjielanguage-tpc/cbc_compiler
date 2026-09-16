import a
import b
import c

main() {
  c.testCA(c.CA.A)
  c.testCB(c.CB.D(None))
  c.testA(MAA)
  c.testB(MBB(None))
}

public enum MA <: a.A {
  MAA | MAB

  public override func foo() {
    println("MA.foo")
  }
}

public enum MB <: b.B {
  MBA(MB) | MBB(?MB)

  public override func foo() {
    println("MB.foo")
  }
}
