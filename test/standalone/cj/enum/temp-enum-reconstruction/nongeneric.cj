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
        // AOT code checks the generic container's TypeInfo after GC.
        unwrap<T>(NativeBox<T>(holder.value))
    }
}

// reference the enum in CBC metadata without executing a TypeInfo lookup
func metadata(value: NativeEnum): Unit {}

main(): Int64 {
    if (!runNongeneric(CollectingProbe())) { return 1 }
    println("nongeneric-ok")
    0
}
