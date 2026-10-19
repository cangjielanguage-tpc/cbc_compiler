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
    // Warm the control TypeInfos: these test existing enum/option representations,
    // while the other entrypoints deliberately start with cold TEMP_ENUM TypeInfos.
    let probe = CollectingProbe()
    probe.apply<Payload>(Payload("warm"))
    probe.apply<Scalar>(scalar())
    probe.apply<Option<Payload>>(Some(Payload("warm")))
    probe.apply<Option<Int64>>(Some(1))
    if (!runControls(probe)) { return 1 }
    println("controls-ok")
    0
}
