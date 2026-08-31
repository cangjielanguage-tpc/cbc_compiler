main(): Int64 {
    let future: Future<Int64> = spawn {
        => 42
    }

    println(future.get())
    return 0
}
