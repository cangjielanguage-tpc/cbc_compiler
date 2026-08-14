package aot
import std.sync.SyncCounter

var m = SyncCounter(1)

public func wait(): Int32 {
    m.waitUntilZero()
    println("Wait!")
    0
}

public func notify(): Int32 {
    println("Notify!")
    m.dec()
    0
}
