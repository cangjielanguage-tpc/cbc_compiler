package b

public class B<T> <: Hashable & Equatable<B<T>> where T <: Hashable & Equatable<T> {
  public B(public let v: T) {}

  public operator func ==(that: B<T>) {
    this.v == that.v
  }

  public func hashCode() {
    this.v.hashCode()
  }
}


