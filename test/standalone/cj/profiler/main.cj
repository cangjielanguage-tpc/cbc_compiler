func leafA(i: Int64): Int64 {
    i + 1
}

func leafB(i: Int64): Int64 {
    i * 2
}

func dispatch(i: Int64): Int64 {
    if (i % 2 == 0) {
        leafA(i)
    } else {
        leafB(i)
    }
}

func middle(i: Int64): Int64 {
    dispatch(i) + leafA(i)
}

func outer(i: Int64): Int64 {
    middle(i) + leafB(i)
}

main() {
    let n = 20000
    var count: Int64 = 0
    for (i in 1..n) {
        count += outer(i)
    }
    println(count)
}
