package aot

@C
public func plusTwo(a: Int64): Int64 {
    a + 2
}

public func getPlusTwo(): CFunc<(Int64) -> Int64> {
    plusTwo
}