import std.collection.*

func foo(x: String, y: String): Unit {
  println(x)
  println(y)
}

func bar(hm: HashMap<?String, ?String>): Unit {
    for ((k, v) in hm) {
        foo(k.getOrThrow(), v.getOrThrow())
    }
}

main() {
    let hm = HashMap<?String, ?String>()
    hm["k1"] = "v1"
    hm["k2"] = "v2"
    bar(hm)
    0
}
