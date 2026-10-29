import aot.*

class GenericOperation<T> <: Operation<T> {
  public static func calculate<U>(value: T, extra: U): (T, U) {
    // Returning both values requires the implementation's outer TI and method TI.
    (value, extra)
  }
}

main(): Int64 {
  let first = invoke<GenericOperation<Int64>, Int64, String>(42, "generic")
  if (first[0] != 42 || first[1] != "generic") { return 1 }
  let second = invoke<GenericOperation<String>, String, Float64>("outer", 3.5)
  if (second[0] != "outer" || second[1] != 3.5) { return 2 }
  let third = invoke<GenericOperation<Int64>, Int64, Bool>(17, true)
  if (third[0] != 17 || !third[1]) { return 3 }
  0
}
