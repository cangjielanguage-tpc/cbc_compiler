import a
import b

import std.collection.*

var hm = HashMap<a.A<String>, b.B<Int64>>()

main(): Int64 {
  hm[a.A("abc")] = b.B(123)
  hm[a.A("def")] = b.B(456)
  hm[a.A("abc")] = b.B(789)

  for ((k, v) in hm) {
    println(k.v)
    println(v.v)
  }

  println(hm.size)
  0
}
