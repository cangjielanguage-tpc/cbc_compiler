import aot.*

public var end: Int64 = 0
public var selector: Int64 = 0

public var bh: Cons = Cons()
public var bh2: ConsContainer = ConsContainer()

public func consume(c1: ConsContainer, c2: ConsContainer, c3: ConsContainer): Int64 {
    bh = c1.r1
    bh = c1.r2
    bh = c2.r1
    bh = c2.r2
    bh = c3.r1
    bh = c3.r2
    bh2 = c1
    bh2 = c2
    bh2 = c3
    return gcpoint()
}

main() {
  startGarbageGenerator()

  var v1  = Cons()
  var v2  = Cons(v1)
  var v3  = Cons(v2)
  var v4  = Cons(v3)
  var v5  = Cons(v4)
  var v6  = Cons(v5)
  var v7  = Cons(v6)
  var v8  = Cons(v7)
  var v9  = Cons(v8)
  var v10 = Cons(v9)
  var v11 = Cons(v10)
  var v12 = Cons(v11)

  var c1 = ConsContainer(v1, v2)
  var c2 = ConsContainer(v3, v4)
  var c3 = ConsContainer(v5, v6)
  var c4 = ConsContainer(v7, v8)
  var c5 = ConsContainer(v9, v10)
  var c6 = ConsContainer(v11, v12)

  var counter = 0x30000
  while (counter != end) {
    if (selector == 0) {
        consume(c1, c2, c3)
        consume(c3, c2, c1)
        selector += 1
    } else if (selector == 1) {
        consume(c4, c5, c6)
        consume(c6, c5, c4)
        selector += 1
    } else {
        v12 = Cons(v12)
        v1  = v12
        c6.r2 = v12
        selector = 0
    }
    counter -= 1
  }

  bh  = v1
  bh2 = c1
  bh2 = c2
  bh2 = c3
  counter + (c1.p1 + c2.p2 + c3.p1) * 10 + c3.p2
}