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

public class TestClass {
    public var x: Int64 = 123
    public var y: Int64 = 321
}

public struct TestContainer {
    public var p1: Int64 = 0
    public var r1: TestClass = TestClass()
}

public func checkObj(test: TestClass): Int64 {
    if (test.x != 123 || test.y != 321) {
        println("Test error: found corrupted object")
    }
    return 0
}
