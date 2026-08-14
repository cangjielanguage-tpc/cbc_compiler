package aot

public interface I<T, K> {
  func foo(x: T): Int64
  func foo(x: K): Int64
}

public interface Test {
  func test(): Int64
}

public func test(t: Test): Int64 {
  t.test()
}
