public struct A<T> {
    let a: T
    let b: T
    public init(a: T, b: T) {
        this.a = a
        this.b = b
    }
}

public open class B<T> {
    public var a: A<T>
    public var b: A<T>

    public init(a: A<T>, b: A<T>) {
        this.a = a
        this.b = b
    }

    // TODO: support generation copy generic instruction
    public func copyToFirst(): Unit {
        a = b
    }
}
