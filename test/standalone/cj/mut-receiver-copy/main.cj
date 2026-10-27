import std.runtime.*

public struct CopyProbe {
    private var value: String

    public init(value: String) {
        this.value = value
    }

    public mut func set(value: String): Unit {
        this.value = value
    }

    public func get(): String {
        value
    }

    public func readAfter(box: CopyBox): String {
        box.probe.set("after")
        gc(heavy: true)
        value
    }

    public mut func readSnapshot(box: CopyBox): String {
        // The non-mut callee must receive a value snapshot of this receiver.
        readAfter(box)
    }
}

public class CopyBox {
    public var probe = CopyProbe("before")
}

main() {
    let box = CopyBox()
    println(box.probe.readSnapshot(box))
    println(box.probe.get())
}
