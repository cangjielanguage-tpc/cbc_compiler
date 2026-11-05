package aot

public struct Pair {
  public Pair(public let first: Int64, public let second: Int64) {}
}

public interface Operation {
  static func calculate(value: Int64): Pair
}

public func invoke<M>(value: Int64): Pair where M <: Operation {
  M.calculate(value)
}
