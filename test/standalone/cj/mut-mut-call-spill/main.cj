interface StringReader {
    func read(): String
}

class Reader <: StringReader {
    public func read(): String { "updated" }
}

struct Value {
    public var text = "initial"

    public mut func set(text: String) {
        this.text = text
    }

    public mut func unpack(reader: StringReader) {
        // A virtual call supplies the argument to another mut method.
        set(reader.read())
    }
}

main() {
    var value = Value()
    value.unpack(Reader())
    println(value.text)
}
