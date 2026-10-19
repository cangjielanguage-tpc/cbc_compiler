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

// Refer to public definitions without acquiring their TypeInfos before the callback.
func metadata(value: NativeEnum): Unit {}

main(): Int64 {
    if (!runNongeneric(CollectingProbe())) { return 1 }
    println("nongeneric-ok")
    0
}
