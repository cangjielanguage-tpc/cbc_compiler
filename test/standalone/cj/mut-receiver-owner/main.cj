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
        gc(heavy: true)
        readValue() // uses stuct and implicitly uses containing object to copy record
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
