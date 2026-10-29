package aot

public interface Operation<T> {
  static func calculate<U>(value: T, extra: U): (T, U)
}

public func invoke<M, T, U>(value: T, extra: U): (T, U) where M <: Operation<T> {
  M.calculate<U>(value, extra)
}
