package aot

import std.sync.SyncCounter

var arr: Array<Int64> = [0]

public func startGarbageGenerator(): Int64 {
  let counter = SyncCounter(1)
  spawn { =>
    counter.dec()
    var i: Int64 = 2048
    while (true) {
      let a = Array<Int64>(i, repeat: 0)
      arr = a
    }
  }
  counter.waitUntilZero()
  return 0
}

public var bh: Int64 = 0

public func gcpoint0(): Int64 {
    bh = 0
    return bh
}

public var foo: ?(() -> Int64) = gcpoint0

public func gcpoint(): Int64 {
    return foo.getOrThrow()()
}

public class Cons {
    public var x: Int = 0
    public var t: ?Cons = None

    public init(arg: ?Cons) {
        t = arg
    }

    public init() {
        t = None
    }
}
