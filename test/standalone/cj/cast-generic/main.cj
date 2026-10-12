interface I {
  prop p1: Int64 {
    get() {
      println("I.p1")
      1
    }
  }
}

interface I1 {}

enum E1<M> <: I1 where M <: I {
  E01(M)
}

interface I2<M> where M <: I {
  prop p1: Int64 {
    get() {
      println("I2.p1")
      0
    }
  }
}

extend<M> E1<M> <: I2<M> {}

struct Wrapper {
  Wrapper(let value: String) {}
}

extend Wrapper <: I {
  public prop p1: Int64 {
    get() {
      println("Wrapper.p1")
      42
    }
  }
}

main() {
  var E01(a) = E01<Wrapper>(Wrapper("foo"))
  println(a.p1)
}
