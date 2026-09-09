import a
import b
import c

main(): Int64 {
  c.testA(a.A())
  c.testB(b.B())
  c.testCA(c.CA())
  c.testCB(c.CB())
  c.testA(MA())
  c.testB(MB())
  c.testCA(MCA())
  c.testCB(MCB())
  0
}

public open class MA <: a.A {
  public override open func foo() {
    println("MA.foo")
  }
}

public open class MB <: b.B {
  public override open func foo() {
    println("MB.foo")
  }
}

public open class MCA <: c.CA {
  public override open func foo() {
    println("MCA.foo")
  }
}

public open class MCB <: c.CB {
  public override open func foo() {
    println("MCB.foo")
  }
}
