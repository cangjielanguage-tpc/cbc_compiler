import aot

foreign func applyCb(cb: CFunc<(Int64) -> Int64>): Int64

main(): Int64 {
    let f = aot.getPlusTwo()
    return unsafe { applyCb(f) }
}