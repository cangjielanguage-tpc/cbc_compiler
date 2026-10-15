package genericboxinstanceof

public struct A<T> {}

public func test<T>(value: A<T>): Int64 {
    match (value) {
        case _: A<Int32> => 0
        case _ => 1
    }
}
