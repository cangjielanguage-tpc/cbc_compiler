import aot.*

class Add <: Operation {
  public static func calculate(value: Int64): Int64 {
    if (value < 0) { throw IllegalArgumentException("static virtual callback") }
    value + 7
  }

  public static func constant(): Int64 { 7 }
}

class Multiply {}

extend Multiply <: Operation {
  public static func calculate(value: Int64): Int64 { value * 3 }
  public static func constant(): Int64 { 3 }
}

func invokeInterpreted<M>(value: Int64): Int64 where M <: Operation {
  M.calculate(value) + M.constant()
}

main(): Int64 {
  // Both implementations occupy the same interface slot and have no instance receiver.
  if (invoke<Add>(11) != 18) { return 1 }
  if (invoke<Multiply>(11) != 33) { return 2 }
  if (invoke<Add>(23) != 30) { return 3 }
  if (invokeConstant<Add>() != 7) { return 4 }
  if (invokeConstant<Multiply>() != 3) { return 5 }
  if (invokeInterpreted<Add>(11) != 25) { return 6 }
  if (invokeInterpreted<Multiply>(11) != 36) { return 7 }

  var caught = false
  try {
    invoke<Add>(-1)
  } catch (e: IllegalArgumentException) {
    if (e.message != "static virtual callback") { return 8 }
    caught = true
  }
  if (!caught) { return 9 }
  0
}
