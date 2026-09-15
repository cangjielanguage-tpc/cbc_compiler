package aot

import std.runtime.*

public func collect(): Int64 {
    gc(heavy: true)
    gc(heavy: true)
    return 0
}
