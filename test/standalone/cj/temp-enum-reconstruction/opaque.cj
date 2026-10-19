import enum_support.*
import std.runtime.gc

// The AOT caller supplies T. Creating Holder<T> emits load.type.info.g,
// which reconstructs that native TypeInfo before building the holder.
class Holder<T> {
    var value: T
    init(value: T) { this.value = value }
}

class CollectingProbe <: Probe {
    public func apply<T>(value: T): T {
        let holder = Holder<T>(value)
        gc(heavy: true)
        // Native code checks the generic container's TypeInfo after GC.
        unwrap<T>(NativeBox<T>(holder.value))
    }
}

main(): Int64 {
    if (!runOpaque(CollectingProbe())) { return 1 }
    println("opaque-ok")
    0
}
