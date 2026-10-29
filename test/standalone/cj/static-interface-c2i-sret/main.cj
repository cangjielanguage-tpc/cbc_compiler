import aot.*

class Forward <: Operation {
  public static func calculate(value: Int64): Pair { Pair(value, value + 1) }
}

class Reverse <: Operation {
  public static func calculate(value: Int64): Pair { Pair(value + 1, value) }
}

main(): Int64 {
  // The hidden result buffer is separate from the static dispatch type information.
  let forward = invoke<Forward>(41)
  if (forward.first != 41 || forward.second != 42) { return 1 }
  let reverse = invoke<Reverse>(51)
  if (reverse.first != 52 || reverse.second != 51) { return 2 }
  0
}
