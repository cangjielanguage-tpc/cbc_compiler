import aot.*
import aot

public class B<T> <: I<T> {
    public var f: T
    public var iters: Int64 = 100000

    public init(f: T) {
        this.f = f
    }

    public func fact(n: Int64): Int64 {
        if (n == 0) {
            return 0
        }
        return n * fact(n - 1)
    }

    public func foo(): Option<T> {
        println("Start")
        return f
    }
}

main() {
    g_I = B<S>(S())
    aot.invokeI()
    return 0
}
