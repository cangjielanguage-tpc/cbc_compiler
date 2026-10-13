package genericbox

public struct Pair<T> {
    public let first: T
    public let second: T

    public init(first: T, second: T) {
        this.first = first
        this.second = second
    }

    public func copy(): Pair<T> {
        this
    }
}

public open class PairHolder<T> {
    private var first: Pair<T>
    private var second: Pair<T>

    public init(first: Pair<T>, second: Pair<T>) {
        this.first = first
        this.second = second
    }

    public func copySecondToFirst(): Unit {
        first = second
    }

    public func getFirst(): Pair<T> {
        first
    }
}
