import std.runtime.*

public struct OwnerProbe {
    private var value: String

    public init(value: String) {
        this.value = value
    }

    public func readValue(): String {
        value
    }

    public mut func readAfterGC(): String {
        value = value + "!"
        // The receiver address remains live across this call, even when the
        // owner has no further explicit uses. readValue is deliberately non-mut.
        gc(heavy: true)
        readValue()
    }
}

class OwnerBox {
    public var probe = OwnerProbe("heap")
}

main() {
    var local = OwnerProbe("local")
    println(local.readAfterGC())
    let box = OwnerBox()
    println(box.probe.readAfterGC())
}
