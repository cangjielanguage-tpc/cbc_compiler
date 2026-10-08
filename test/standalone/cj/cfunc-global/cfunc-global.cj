foreign func ff(a: Int64): Int64

let g: CFunc<(Int64) -> Int64> = ff

foreign func fs(a1: Int64, a2: Int64, a3: Int64, a4: Int64, a5: Int64, a6: Int64, a7: Int64, a8: Int64, a9: Int64, a10: Int64): Int64

let gs: CFunc<(Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64, Int64) -> Int64> = fs

main(): Int64 {
    if (unsafe { gs(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) } != 385) {
        println("fs failed")
        return 1
    }
    return unsafe { g(40) }
}
