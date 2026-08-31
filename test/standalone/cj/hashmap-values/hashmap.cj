import std.collection.*

func foo(y: String): Unit {
  println(y)
}

func bar(hm: HashMap<String, String>): Unit {
    for (v in hm.values()) {
        foo(v)
    }
}

main() {
    let hm = HashMap<String, String>()
    hm["k1"] = "v1"
    hm["k2"] = "v2"
    bar(hm)
    0
}
