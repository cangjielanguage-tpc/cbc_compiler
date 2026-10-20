package pkgA

public struct Key <: Hashable & Equatable<Key> {
  public let x: Int64
  public init(x: Int64) { this.x = x }
  public func hashCode(): Int64 { x }
  public operator func ==(o: Key): Bool { x == o.x }
  public operator func !=(o: Key): Bool { x != o.x }
}
