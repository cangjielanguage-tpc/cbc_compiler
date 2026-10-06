package genericarray

public func getElement<T>(array: Array<T>, idx: Int64): T {
    array[idx]
}

public func setElement<T>(array: Array<T>, idx: Int64, value: T): Unit {
    array[idx] = value
}

public func allocateEmptyArray<T>(): Unit {
    let array: Array<T> = []
    println(array.size)
}

public func repeatedArrayElement<T>(value: T): T {
    let array = Array<T>(3, repeat: value)
    array[1]
}

public func arrayOf<T>(value: T): Array<T> {
    [value]
}
