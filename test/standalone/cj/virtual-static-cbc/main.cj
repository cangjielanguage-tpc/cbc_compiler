// Non-generic InvokeStatic, including inherited static calls forwarding this TI.
struct Result {
  public Result(public let value: Int64) {}
}

open class Base {
  public init() {}

  public static func value(): Int64 { 11 }

  public static func inherited(): Int64 { value() }

  public static func result(): Result { Result(value()) }

  public func read(): Int64 { inherited() }

  public func readResult(): Result { result() }
}

class Derived <: Base {
  public init() { super() }

  public static func value(): Int64 { 22 }
}

class OtherDerived <: Base {
  public init() { super() }

  public static func value(): Int64 { 33 }
}

func check(base: Base, expected: Int64): Bool {
  base.read() == expected && base.readResult().value == expected
}

main(): Int64 {
  if (!check(Base(), 11)) { return 1 }
  if (!check(Derived(), 22)) { return 2 }
  if (!check(OtherDerived(), 33)) { return 3 }
  0
}
