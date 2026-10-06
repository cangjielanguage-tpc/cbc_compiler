package genericarray

public func getElement<T>(array: Array<T>, idx: Int64): T {
    array[idx]
}

public func setElement<T>(array: Array<T>, idx: Int64, value: T): Unit {
    array[idx] = value
}
