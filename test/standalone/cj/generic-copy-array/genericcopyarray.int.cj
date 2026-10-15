package genericcopyarray

public struct Copyable<T> {
    public let value: Int64

    public init(value: Int64) {
        this.value = value
    }
}

public func copyArrayElement<T>(values: Array<Copyable<T>>, sourceIndex: Int64, targetIndex: Int64): Copyable<T> {
    values[targetIndex] = values[sourceIndex]
    values[targetIndex]
}
