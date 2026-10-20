package pkgB

import std.collection.*

public func useIntKeys(): Int64 {
  let m = HashMap<Int64, Int64>()
  m[10] = 1
  m[20] = 2
  var s = 0
  for (k in m.keys()) {
    s += k
  }
  s
}
