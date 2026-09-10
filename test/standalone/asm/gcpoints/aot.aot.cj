package aot

import std.sync.SyncCounter

var arr: Array<Int64> = [0]

public func testGcPoint(): Int64 {
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
