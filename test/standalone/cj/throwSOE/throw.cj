import std.env.*

var g: Int64 = 0

func f(x: Int64): Int64 {
    let result = f(x + 1)
    g = g + x
    return result + 1
}

main() {
    atExit {
        println("Hello CJC")
    }
    println(f(1))
}
