func test_get(): Unit {
    let arr: VArray<Int64, $3> = [1, 2, 3]
    println(arr[0])
}

func get_index(): Int64 {
   1
}

func test_getI(): Unit {
    let idx = get_index()
    let arr: VArray<Int64, $3> = [1, 2, 3]
    println(arr[idx])
}

func test_set(): Unit {
    var arr: VArray<Int64, $3> = [1, 2, 3]
    arr[1] = 42
    println(arr[1])
}

func test_lambda(): Unit {
    var arr = VArray<Int64, $3>({ i => i * 2 })
    println(arr[2])
}

func generator(idx: Int64): Int64 {
   idx + 2
}

func test_function(): Unit {
    var arr = VArray<Int64, $3>(generator)
    println(arr[2])
}

func test_item(): Unit {
    var arr = VArray<Int64, $3>(repeat: 42)
    println(arr[1])
}

func test_elemTypeVarray(): Unit {
    var arr: VArray<VArray<Int64, $3>, $2> = [[1, 2, 3],
                                              [4, 5, 6]]
    println(arr[1][2])
}

func test_size(): Unit {
    let arr: VArray<Int64, $5> = [1, 2, 3, 4, 5]
    println(arr.size)
}

func get_first(a: VArray<Int64,$2>): Int64 {
    return a[0]
}

func test_arg(): Unit {
    let arr: VArray<Int64, $2> = [1, 2]
    println(get_first(arr))
}

func get_varray(): VArray<Int64, $2> {
    let arr: VArray<Int64, $2> = [1, 2]
    arr
}

func test_ret(): Unit {
    let arr = get_varray()
    println(arr[1])
}

struct Point {
    let x: Int32
    let y: Int32

    init (x: Int32, y: Int32) {
        this.x = x
        this.y = y
    }
}

func test_structTypeSimple() {
    var arr = VArray<Point, $2>(repeat: Point(0, 0))
    arr[0] = Point(1, 2)
    arr[1] = Point(3, 2)
    println(arr[1].x)
}

struct S {
    let arr: VArray<Point, $3>

    init (arr: VArray<Point, $3>) {
        this.arr = arr
    }
}

func test_structType() {
    var arr = VArray<S, $2>(repeat: S(VArray<Point, $3>(repeat: Point(10, 20))))
    let n = get_index()
    println(arr[1].arr[n].x)
}

func merge_varray(a: VArray<Int64,$2>, b: VArray<Int64, $3>): VArray<Int64, $5> {
    var ret = VArray<Int64, $5>(repeat: 0)
    for(i in 0..2) {
        ret[i] = a[i]
    }
    for (i in 0..3) {
        ret[a.size + i] = b[i]
    }
    return ret
}

func test_argOfFunction1(): Unit {
    let arr1: VArray<Int64, $2> = [1, 2]
    let arr2: VArray<Int64, $3> = [3, 4, 5]
    let arr_result = merge_varray(arr1, arr2)
     for (i in 0..5) {
         println(arr_result[i])
     }
    println(arr_result.size)
}

func get_first_varray(a: VArray<Int64, $3>, b: VArray<Int64, $1>, c: VArray<Int64, $1>): VArray<Int64, $3> {
    var ret = VArray<Int64, $3>(repeat: 0)
    for(i in 0..3) {
       ret[i] = a[i]
    }
    return ret
}

func test_argOfFunction2(): Unit {
    let arr1: VArray<Int64, $3> = [1, 2, 3]
    let arr2: VArray<Int64, $1> = [4]
    let arr3: VArray<Int64, $1> = [5]
    let arr_result = get_first_varray(arr1, arr2, arr3)
     for (i in 0..3) {
         println(arr_result[i])
     }
    println(arr_result.size)
}

class Foo {
    let x: Int64
    let flat: Bar
    let arr: VArray<Int32, $2>

    init(x: Int64, flat: Bar, arr: VArray<Int32, $2>) {
        this.x = x
        this.flat = flat
        this.arr = arr
    }
}

struct Bar {
    let x: Int64
    let arr: VArray<Int32, $2>

    init(x: Int64, arr: VArray<Int32, $2>) {
        this.x = x
        this.arr = arr
    }
}

func fromObj1(x: Foo): Int32 {
    return x.arr[1]
}

func fromObj2(x: Foo): Int32 {
    return x.flat.arr[1]
}

func test_fromObj() {
    var foo = Foo(1, Bar(2, [3, 4]), VArray<Int32, $2>(repeat: 5))
    println(fromObj1(foo))
    println(fromObj2(foo))
}

struct Baz {
    let x: Int64
    let flat: Bar
    let arr: VArray<Int32, $2>

    init(x: Int64, flat: Bar, arr: VArray<Int32, $2>) {
        this.x = x
        this.flat = flat
        this.arr = arr
    }
}

func fromRec1(x: Baz): Int32 {
    return x.arr[1]
}

func fromRec2(x: Baz): Int32 {
    return x.flat.arr[1]
}

func test_fromRec() {
    var baz = Baz(1, Bar(2, [3, 4]), VArray<Int32, $2>(repeat: 5))
    println(fromRec1(baz))
    println(fromRec2(baz))
}

let x = Baz(1, Bar(2, [3, 4]), VArray<Int32, $2>(repeat: 5))

func fromGlobal1(): Int32 {
    return x.arr[1]
}

func fromGlobal2(): Int32 {
    return x.flat.arr[1]
}

func test_fromGlobal() {
    println(fromGlobal1())
    println(fromGlobal2())
}

main(): Int64 {
    test_get()
    test_getI()
    test_set()
    test_lambda()
    // TODO failed to get type info of @$CfN7default3generatorHl
    // test_function()
    test_item()
    test_elemTypeVarray()
    test_size()
    test_structTypeSimple()
    test_structType()
    test_arg()
    test_ret()
    test_argOfFunction1()
    test_argOfFunction2()
    test_fromObj()
    test_fromRec()
    test_fromGlobal()
    0
}
