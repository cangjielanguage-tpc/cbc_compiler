struct Data<T> {
    public var a: T
    public init(x: T) {
        a = x
    }

    // Static fields of generic struct with preinitialization
    public static var intV: Int32 = 2
    public static var longV: Int64 = 300
    public static var floatV: Float64 = 1.5
    public static var boolV: Bool = true
    public static var strV: String = "data"
    public static var optV: ?Int64 = Some(42)
}

class Box<T> {
    public var v: T
    public init(y: T) {
        v = y
    }

    // Static fields of generic class with preinitialization
    public static var cnt: Int32 = 7
    public static let name: String = "box"
}

main(): Int64 {
    if (Data<Int32>.intV != 2 || Data<String>.longV != 300 || Data<Rune>.floatV != 1.5 ||
            !Data<Bool>.boolV || Data<Int8>.strV != "data" || Data<Int8>.optV.getOrThrow() != 42) {
        return 1
    }

    if (Box<Bool>.cnt != 7 || Box<Int8>.name != "box") {
        return 1
    }

    return 0
}
