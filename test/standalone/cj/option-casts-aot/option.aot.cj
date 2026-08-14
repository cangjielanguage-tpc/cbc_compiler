package aot

public func checkOptionalIs<T, U>(optional: Option<U>): Bool {
    return match (optional) {
        case Some(_: T) => true
        case _          => false
    }
}
