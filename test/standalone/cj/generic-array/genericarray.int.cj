package genericarray

extend <T> Array<T> {
    func foo(x: T): Int64 {
        this.size
    }
}

public func baz<T>(value: T): Int64 {
    let array: Array<T> = [value, value]
    array.foo(value)
}
