extend<T> CPointer<T> {
    func coo() {}
}
public func coo<T>() where T <: CType {
    let a = CPointer<T>()
    a.coo()
}
main(): Unit {
    coo<Int64>()
}
