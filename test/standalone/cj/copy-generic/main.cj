public struct A<T> {
    let a: T
    let b: T
    public inti(a: T, b: T) {
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

    public func copyToFirst(): Unit {
        a = b
    }
}