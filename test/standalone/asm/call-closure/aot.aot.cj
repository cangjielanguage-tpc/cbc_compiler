package aot

public func getstr(): String {
  return "Hello from aot"
}

var _sret: Int64 = 0

public func guessSRet(a1: Int64, a2: Int64): Unit {
  // pass 0, 1 on IR1, IR2
  // on x64 -> a1 == 1
  // on aarch64 -> a1 == 0
  _sret = a1
}

public func sret(): Int64 {
  _sret
}

public func print(s: String): Int64 {
  println(s)
  0
}
