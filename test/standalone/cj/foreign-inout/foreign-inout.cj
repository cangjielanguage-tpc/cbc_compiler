foreign func PlusOne(ptr: CPointer<Int32>): Unit
foreign func PlusOneEachField(ptr: CPointer<Data>): Unit
foreign func PlusHalf(ptr: CPointer<Float32>): Unit

let base: Int32 = 100

func printValue(v: Int32) {
    println(v)
}

@C
struct Data {
    var a: Int8 = 0
    var b: Int16 = 0
    var c: Int32 = 0
    var d: Int64 = 0
}

struct Counter {
    var n: Int32 = 0
    mut func bump() {
        unsafe { PlusOne(inout this.n) }
    }
}

struct Holder {
    var data: Data = Data()
}

var total: Int32 = 7
public var cnt: Int64 = 5

func testCase_1() {
    var a: Int32 = 0
    unsafe { PlusOne(inout a) }
    printValue(a)
    println(a + base)
}

func testCase_2() {
    var c = Counter()
    unsafe { PlusOne(inout c.n) }
    c.bump()
    printValue(c.n)
}

func testCase_3() {
    var d = Data()
    unsafe { PlusOneEachField(inout d) }
    println("d = {${d.a}, ${d.b}, ${d.c}, ${d.d}}")
    unsafe { PlusOneEachField(inout d) }
    println("d = {${d.a}, ${d.b}, ${d.c}, ${d.d}}")
}

func testCase_4() {
    var h = Holder()
    unsafe { PlusOneEachField(inout h.data) }
    println("h = {${h.data.a}, ${h.data.b}, ${h.data.c}, ${h.data.d}}")
}

func testCase_5() {
    var c: Int32 = 0
    unsafe {
        PlusOne(inout c)
        PlusOne(inout c)
        PlusOne(inout c)
    }
    println(c)
}

func testCase_6() {
    var f: Float32 = 0.0
    for (i in 0..cnt) {
        unsafe { PlusHalf(inout f) }
        println(f)
    }
}

func testCase_7() {
    unsafe { PlusOne(inout total) }
    println(total + base)
}

var gData: Data = Data()

func testCase_8() {
    unsafe { PlusOneEachField(inout gData) }
    println("g = {${gData.a}, ${gData.b}, ${gData.c}, ${gData.d}}")
}

main(): Int64 {
    testCase_1()
    testCase_2()
    testCase_3()
    testCase_4()
    testCase_5()
    testCase_6()
    testCase_7()
    testCase_8()
    return 0
}
