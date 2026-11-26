public interface BenchInputProvider<T> {
    mut func reset(max: T) {
        update(max)
    }

    mut func update(max: T): Unit
}

public struct ImmutableInputProvider<T> <: BenchInputProvider<T> {
    public ImmutableInputProvider(var data: T) {}

    public mut func update(max: T): Unit {
        data = max
    }
}

main() {
    var provider = ImmutableInputProvider<Int64>(1)
    provider.reset(42)
    println(provider.data)
}
