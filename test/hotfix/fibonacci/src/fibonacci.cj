let TARGET_NUM: Int64 = 12

public func fib(): Int64 {
    var a: Int64 = 0
    var b: Int64 = 1
    let n = TARGET_NUM
    for (i in 1..n) {
        let tmp = b
        b = a + b
        a = tmp
    }
    return a
}

main() {
    println(fib())
}
