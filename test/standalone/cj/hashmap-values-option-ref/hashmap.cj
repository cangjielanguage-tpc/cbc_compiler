import std.collection.*

class C {
  C(let str: String) {}
}

func foo(y: C): Unit {
  println(y.str)
}

func bar(hm: HashMap<?String, ?C>): Unit {
    for (v in hm.values()) {
        foo(v.getOrThrow())
    }
}

main() {
    let hm = HashMap<?String, ?C>()
    hm["k1"] = C("v1")
    hm["k2"] = C("v2")
    bar(hm)
    0
}
