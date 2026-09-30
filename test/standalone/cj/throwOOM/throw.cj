import std.env.*
import std.collection.*

main(): Unit {
    atExit {
        println("Hello CJC")
    }
    let arr = ArrayList<Array<Int64>>()
    while (true) {
        arr.add(Array<Int64>(100000000000) { i => 0 })
    }
}
