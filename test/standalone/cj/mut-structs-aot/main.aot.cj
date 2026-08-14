package aot

public var one = S(Object(), 1)

public struct S {
  public S(public let x: Object, public var id: Int64) {}

  public mut func foo(id: Int64) {
    this.id = id
  }

  public mut func bar(id: Int64) {
    foo(id)
  }
}
