foreign func ff(a: Int64): Int64

let g: CFunc<(Int64) -> Int64> = ff

main(): Int64 {
    return unsafe { g(40) }
}
