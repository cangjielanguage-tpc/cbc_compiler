import aot.*

public open class Child <: I<Int64, Int32> & I<Int32, UInt64> {
  public open func foo(x: Int64): Int64 {
    println("i64 ${x}")
    0
  }
  public open func foo(x: Int32): Int64 {
    println("i32 ${x}")
    0
  }
  public open func foo(x: UInt64): Int64 {
    println("u64 ${x}")
    0
  }
}

public class TestInterface <: Test {
  TestInterface(let x: I<Int64, Int32>, let y: I<Int32, UInt64>) {}

  public func test(): Int64 {
    x.foo(Int64(0))
    x.foo(Int32(1))
    y.foo(Int32(2))
    y.foo(UInt64(3))
    0
  }
}

public class TestChild <: Test {
  TestChild(let x: Child) {}

  public func test(): Int64 {
    x.foo(Int64(4))
    x.foo(Int32(5))
    x.foo(UInt64(6))
    0
  }
}

main() {
  let c = Child()
  test(TestInterface(c, c))
  test(TestChild(c))
  0
}
