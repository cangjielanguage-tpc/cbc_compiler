public struct GenericReceiver<T> {
    private var value: T

    public init(value: T) {
        this.value = value
    }

    public func get(): T {
        value
    }

    public mut func replace(value: T): T {
        let previous = get()
        this.value = value
        previous
    }
}

main(): Int64 {
    var receiver = GenericReceiver<Int64>(17)
    println(receiver.replace(29))
    println(receiver.get())
    return 0
}
