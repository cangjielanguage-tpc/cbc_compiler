main(): Int64 {
    if (case02()(1) != ()) { // <--- PROBLEM HERE (program exits with code 2, expected 0)
        return 2
    }
    return 0
}

func case02() {
    var c: (Int32) -> Unit = {x: Int32 => x}
    return c
}
