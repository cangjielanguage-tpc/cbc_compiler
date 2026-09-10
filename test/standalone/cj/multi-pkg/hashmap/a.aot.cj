package a

public class A<T> <: Hashable & Equatable<A<T>> where T <: Hashable & Equatable<T> {
  public A(public let v: T) {}

  public operator func ==(that: A<T>) {
    this.v == that.v
  }

  public func hashCode() {
    this.v.hashCode()
  }
}
