package aot

public interface Operation {
  static func calculate(value: Int64): Int64
  static func constant(): Int64
}

// Compile the interface dispatch into the native library; implementations live in CBC.
public func invoke<M>(value: Int64): Int64 where M <: Operation {
  M.calculate(value)
}

public func invokeConstant<M>(): Int64 where M <: Operation {
  M.constant()
}
