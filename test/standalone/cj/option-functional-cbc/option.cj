import std.runtime.*

class Bias {
  let value: Int64

  init(value: Int64) {
    this.value = value
  }
}

class OptionalFunctionHolder {
  let prefix: Int64
  var callback: Option<(Int64) -> Int64>
  let suffix: Int64

  init(prefix: Int64, callback: Option<(Int64) -> Int64>, suffix: Int64) {
    this.prefix = prefix
    this.callback = callback
    this.suffix = suffix
  }

  func apply(value: Int64): Int64 {
    match (callback) {
      case Some(f) => f(value)
      case None => value
    }
  }

  func setCallback(callback: Option<(Int64) -> Int64>): Unit {
    this.callback = callback
  }

  func sentinelSum(): Int64 {
    prefix + suffix
  }
}

func makeHolder(): OptionalFunctionHolder {
  let bias = Bias(40)
  OptionalFunctionHolder(10, Some({ value => value + bias.value }), 20)
}

main() {
  let holder = makeHolder()

  gc(heavy: true)
  println(holder.sentinelSum())
  println(holder.apply(2))

  holder.setCallback(None)
  gc(heavy: true)
  println(holder.apply(7))

  holder.setCallback(Some({ value => value * 6 }))
  gc(heavy: true)
  println(holder.apply(7))
  println(holder.sentinelSum())
  0
}
