class RepeatedField<T> {
  public var value: ?T = None

  public init() {}

  public func appendDirty(item: T): Unit {
    value = Some(item)
  }
}

abstract class MessageLite {
  protected static func s_add<T>(fields: RepeatedField<T>, value: T): Unit {
    fields.appendDirty(value)
  }

  protected static func identity<T>(value: T): T {
    value
  }
}

class MetaDataManager <: MessageLite {
  public init() {}

  public func forward<T>(fields: RepeatedField<T>, value: T): Unit {
    s_add(fields, value)
  }

  public func unpack(): Int64 {
    let numbers = RepeatedField<Int64>()
    let strings = RepeatedField<String>()

    // Both inferred and explicit method type arguments must retain MessageLite.
    s_add(numbers, 42)
    s_add<String>(strings, "first")
    if (numbers.value.getOrThrow() != 42 || strings.value.getOrThrow() != "first") {
      return 1
    }

    // Forward a caller's generic argument to the inherited helper.
    forward(numbers, 73)
    forward(strings, "second")
    if (numbers.value.getOrThrow() != 73 || strings.value.getOrThrow() != "second") {
      return 2
    }

    // Also exercise the generic return convention.
    if (identity(91) != 91 || identity<String>("result") != "result") {
      return 3
    }
    0
  }
}

main(): Int64 {
  MetaDataManager().unpack()
}
