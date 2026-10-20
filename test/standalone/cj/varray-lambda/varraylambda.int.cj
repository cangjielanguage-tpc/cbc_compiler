package varraylambda

public struct Point {
    public let x: Int64
    public let y: Int64

    public init(x: Int64, y: Int64) {
        this.x = x
        this.y = y
    }
}

public func test_library_primitive_lambda(): Bool {
    let arr = VArray<Int64, $10000>({ i => i * 3 })
    return arr[0] == 0 && arr[1234] == 3702 && arr[9999] == 29997
}

public func test_library_captured_lambda(): Bool {
    let off = 23
    let arr = VArray<Int64, $16>({ i => i + off })
    return arr[0] == 23 && arr[7] == 30 && arr[15] == 38
}

public func test_library_short_integral_lambda(): Bool {
    let arr = VArray<UInt8, $4>({ i => UInt8(i + 5) })
    return arr[0] == 5 && arr[1] == 6 && arr[2] == 7 && arr[3] == 8
}

public func test_library_record_lambda(): Bool {
    let arr = VArray<Point, $10000>({ i =>
        Point(i, i * 3)
    })
    return arr[0].x == 0 && arr[1234].y == 3702 && arr[9999].x == 9999
}

public func test_library_generic_captured_lambda<T>(x: T): Bool {
    let arr = VArray<Int64, $4>({ i =>
        let _ = x
        i + 50
    })
    return arr[0] == 50 && arr[3] == 53
}
