func spawnGeneric<T>(value: T): Future<T> {
    spawn {
        => value
    }
}

main(): Int64 {
    let future: Future<Int64> = spawn {
        => 42
    }

    println(future.get())
    println(spawnGeneric<String>("generic").get())
    return 0
}
