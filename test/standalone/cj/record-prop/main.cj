struct R {
  var a: Int64 = 0
  mut prop p: Int64 {
    get()  { a     }
    set(v) { a = v }
  }
  mut func inc() {
    p++
  }
}

main() {
  var x = R()
  println(x.p)
  x.inc()
  println(x.p)
  x.p++
  println(x.p)
}
