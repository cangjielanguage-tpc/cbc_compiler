import aot.*

foreign func sleepInC(n: Int64): Int64

class Box {
    var x: Int64 = 0

    init(v: Int64) {
        x = v
    }
}

main(): Int64 {
    startGarbageGenerator()

    let rounds = 20000
    var total: Int64 = 0
    var r: Int64 = 0

    while (r < rounds) {
        let l0 = Box(0)
        let l1 = Box(1)
        let l2 = Box(2)
        let l3 = Box(3)
        let l4 = Box(4)
        let l5 = Box(5)
        let l6 = Box(6)
        let l7 = 7
        let l8 = 8
        let l9 = 9
        let l10 = Box(10)
        let l11 = Box(11)
        let l12 = Box(12)
        let l13 = Box(13)
        let l14 = Box(14)
        let l15 = Box(15)

        let res = unsafe { sleepInC(r) }

        total += res + l0.x + l1.x + l2.x + l3.x + l4.x + l5.x + l6.x + l7 + l8 + l9 + l10.x + l11.x + l12.x + l13.x + l14.x + l15.x
        r += 1
    }

    println("${total}")
    return 0
}
