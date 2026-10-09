interface A {
  mut func c(b: Int64): Int64

  mut func cc(b: Int64): Int64 {
    println("A.cc(${b})")
    c(b)
  }
}

struct R <: A {
  public var a = 3
  public mut func c(b: Int64): Int64 {
    println("R.c(${b})")
    this.a = b
    return this.a
  }
}

main() {
  var a = R()
  println("a.a = ${a.a} before")
  a.cc(9)
  println("a.a = ${a.a} after")
}

