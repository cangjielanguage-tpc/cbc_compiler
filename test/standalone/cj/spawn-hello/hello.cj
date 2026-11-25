main(): Int64 {
    let future = spawn {
        println("Hello, world!")
    }
    future.get()
    return 0
}
