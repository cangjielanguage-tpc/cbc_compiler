package pkg

// Mimics std.unittest: generic interface with a default (non-overridden)
// @Frozen method, a generic struct implementing the interface without
// overriding that method, and a generic wrapper storing a closure that
// calls the default method.

public interface Marker {}

public interface Provider<T> <: Marker {
    @Frozen
    mut func reset(max: Int64) {
        let _ = max
    }

    @Frozen
    mut func get(idx: Int64): T
}

public struct ImmutableProvider<T> <: Provider<T> {
    @Frozen
    public ImmutableProvider(let data: T) {}

    @Frozen
    public mut func get(_: Int64): T { data }
}

public interface Runnable {
    func run(times: Int64, max: Int64): Unit
}

public struct Wrapper<T> <: Runnable {
    let input: T
    let f: (T, Int64, Int64) -> Unit

    public Wrapper(input: T, f: (T, Int64, Int64) -> Unit) {
        this.input = input
        this.f = f
    }

    public func run(times: Int64, max: Int64): Unit {
        f(input, times, max)
    }
}
