package pkgA

import std.collection.*

public func useStringKeys(): Int64 {
  let m = HashMap<String, Int64>()
  m["a"] = 1
  m["bb"] = 2
  var s = 0
  for (k in m.keys()) {
    s += k.size
  }
  s
}
