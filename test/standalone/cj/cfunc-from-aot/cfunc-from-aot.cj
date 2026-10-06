import aot

main(): Int64 {
    let f = aot.getPlusTwo()
    return unsafe { f(40) }
}