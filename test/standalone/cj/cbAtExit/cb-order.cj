import std.env.*

func register(value: Int64) {
    atExit { println(value) }
}

main() {
    register(1)
    register(2)
    let callback = { => println("last") }
    let registerCallback = atExit
    registerCallback(callback)
    println("main")
    return 0
}
