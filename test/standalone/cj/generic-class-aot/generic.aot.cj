package aot

public open class C<T, S> {
  public C(public let x: T, public let y: Int8, public let z: S) {}

  public func getX(): T { x }
  public func getY(): Int8 { y }
  public func getZ(): S { z }
}
