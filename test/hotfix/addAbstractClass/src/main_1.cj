// Allocating Derived also builds the new abstract Base's method table.
abstract class Base {
    public open func value(): Int64
}

class Derived <: Base {
    public func value(): Int64 {
        42
    }
}

func value(): Int64 {
    let instance: Base = Derived()
    instance.value()
}

main() {
    println(value())
}
