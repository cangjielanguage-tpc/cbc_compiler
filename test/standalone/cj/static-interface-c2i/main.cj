import aot.*

class Add <: Operation {
  public static func calculate(value: Int64): Int64 { value + 7 }
}

class Multiply {}

extend Multiply <: Operation {
  public static func calculate(value: Int64): Int64 { value * 3 }
}

main(): Int64 {
  // Both implementations occupy the same interface slot and have no instance receiver.
  if (invoke<Add>(11) != 18) { return 1 }
  if (invoke<Multiply>(11) != 33) { return 2 }
  if (invoke<Add>(23) != 30) { return 3 }
  0
}
