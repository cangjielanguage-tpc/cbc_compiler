foreign func PlusOne(ptr: CPointer<Int32>): Unit

let base: Int32 = 100

func printValue(v: Int32) {
    println(v)
}

struct Counter {
    var n: Int32 = 0
    mut func bump() {
        unsafe { PlusOne(inout this.n) }
    }
}

var total: Int32 = 7

main(): Int64 {
    var a: Int32 = 0
    unsafe { PlusOne(inout a) }
    printValue(a)
    println(a + base)

    var c = Counter()
    unsafe { PlusOne(inout c.n) }
    c.bump()
    printValue(c.n)

    unsafe { PlusOne(inout total) }
    println(total + base)
    return 0
}
