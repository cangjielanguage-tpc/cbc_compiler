package aot

public func identity(x: Float16): Float16 { x }

public func mixed(a: Int64, b: Float16, c: Int64, d: Float16): Float16 {
    Float16(a + c) + b + d
}

public func many(a: Float16, b: Float16, c: Float16, d: Float16, e: Float16,
                 f: Float16, g: Float16, h: Float16, i: Float16, j: Float16): Float16 {
    a + b + c + d + e + f + g + h + i + j
}

public func callback(f: () -> Float16): Float16 { f() }

public interface HalfCast {
    func run(x: Float16): Int64
}

public func checked(i: HalfCast, x: Float16): Int64 {
    try { i.run(x) } catch (e: Exception) { 777 }
}

public class Holder<T> {
    public var value: T
    public init(value: T) { this.value = value }
}

public func wrap<T>(value: T): Holder<T> { Holder(value) }

public func genericCallback<T>(f: () -> T): Holder<T> { Holder(f()) }

public func reportFailure(code: Int64): Int64 {
    println("Test failed with code: ${code}")
    return code
}
