package pkg
public enum A<T> {
    A1
}
extend<T> A<T> {
    public func foo<X>(a: X, b!: A<T> = A1<T>): X {
        return a
    }
}
