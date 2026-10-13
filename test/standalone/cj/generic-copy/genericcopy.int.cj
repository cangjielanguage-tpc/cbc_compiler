package genericcopy

public struct Copyable<T> {
    public let value: Int64

    public init(value: Int64) {
        this.value = value
    }

    public func copy(): Copyable<T> {
        this
    }
}

public open class CopyableHolder<T> {
    private var first: Copyable<T>
    private var second: Copyable<T>

    public init(first: Copyable<T>, second: Copyable<T>) {
        this.first = first
        this.second = second
    }

    public func copySecondToFirst(): Unit {
        first = second
    }

    public func getFirst(): Copyable<T> {
        first
    }
}
