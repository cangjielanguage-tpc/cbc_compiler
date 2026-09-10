import a
import b
import c

main(): Int64 {
  c.testCA(c.CA())
  c.testCB(c.CB())
  c.testA(MA())
  c.testB(MB())
  c.testCA(MCA())
  c.testCB(MCB())
  testAString(c.CA())
  testAString(MA())
  0
}

func testAString(x: a.A<String>) {
  x.foo<Int64>()
}

public open class MA <: a.A<String> {
  public override open func foo<S>() {
    println("MA.foo")
  }
}

public open class MB <: b.B<Int64> {
  public override open func foo<S>() {
    println("MB.foo")
  }
}

public open class MCA <: c.CA {
  public override open func foo<S>() {
    println("MCA.foo")
  }
}

public open class MCB <: c.CB {
  public override open func foo<S>() {
    println("MCB.foo")
  }
}
