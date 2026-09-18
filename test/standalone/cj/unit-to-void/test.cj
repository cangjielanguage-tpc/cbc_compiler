func test1() {
    let sum = {a: Unit => 0}
    println(42)
}

class d {
    var i = 4
    var b: Unit = ()
}

func f() {
    [d().i++, d().i = 0]
}

func test2() {
    println(f()[0])
    println(f()[1])
}

main() {
    test1()
    test2()
}
