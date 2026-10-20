import varraylambda.*

struct A {
    let x: Int64
    let y: Int64

    init(x: Int64, y: Int64) {
        this.x = x
        this.y = y
    }
}

class UnitLambdaCounter {
    var value: Int64 = 0

    func add(i: Int64): Unit {
        value += i
    }
}

func test_primitive_lambda(): Bool {
    let arr = VArray<Int64, $10000>({ i => i * 2 })
    return arr[0] == 0 && arr[1234] == 2468 && arr[9999] == 19998
}

func test_captured_lambda(): Bool {
    let off = 17
    let arr = VArray<Int64, $16>({ i => i + off })
    return arr[0] == 17 && arr[7] == 24 && arr[15] == 32
}

func test_short_integral_lambda(): Bool {
    let arr = VArray<UInt8, $4>({ i => UInt8(i + 1) })
    return arr[0] == 1 && arr[1] == 2 && arr[2] == 3 && arr[3] == 4
}

func test_record_lambda(): Bool {
    let arr = VArray<A, $10000>({ i => A(i, i * 2) })
    return arr[0].x == 0 && arr[1234].y == 2468 && arr[9999].x == 9999
}

func test_generic_captured_lambda<T>(x: T): Bool {
    let arr = VArray<Int64, $4>({ i =>
        let _ = x
        i + 40
    })
    return arr[0] == 40 && arr[3] == 43
}

func test_unit_lambda(): Bool {
    let c = UnitLambdaCounter()
    let _ = VArray<Unit, $4>({ i => c.add(i) })
    return c.value == 6
}

main(): Int64 {
    println(test_primitive_lambda())
    println(test_captured_lambda())
    println(test_short_integral_lambda())
    println(test_record_lambda())
    println(test_generic_captured_lambda<String>("captured"))
    println(test_unit_lambda())
    println(test_library_primitive_lambda())
    println(test_library_captured_lambda())
    println(test_library_short_integral_lambda())
    println(test_library_record_lambda())
    println(test_library_generic_captured_lambda<String>("captured in library"))
    return 0
}
