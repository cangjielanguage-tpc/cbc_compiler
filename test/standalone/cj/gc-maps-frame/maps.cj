import aot.*

public var end: Int64 = 0
public var selector: Int64 = 0

public var bh: Cons = Cons()

public func consume(v1: Cons, v2: Cons, v3: Cons, v4: Cons, v5: Cons, v6: Cons): Int64 {
    bh = v1
    bh = v2
    bh = v3
    bh = v4
    bh = v5
    bh = v6
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

  var a1 = Cons()
  var a2 = Cons(a1)
  var a3 = Cons(a2)

  var counter = 0x30000
  while (counter != end) {
    if (selector == 0) {
        consume(v1, v2, v3, v4, v5, v6)
        consume(v6, v5, v4, v3, v2, v1)
        selector += 1
    } else if (selector == 1) {
        consume(v7, v8, v9, v10, v11, v12)
        consume(v12, v11, v10, v9, v8, v7)
        selector += 1
    } else {
        v12 = Cons(v12)
        v1  = v12
        a3  = Cons(a3)
        selector = 0
    }
    counter -= 1
  }
  bh = v1
  bh = a3
  counter + a1.x + a2.x + a3.x
}