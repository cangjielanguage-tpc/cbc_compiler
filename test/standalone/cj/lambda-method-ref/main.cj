interface I<T> {
  func f(x: T): Unit {
    println("I.f")
  }
}

open class Base <: I<Int64> & I<Int32> {
  public open func f(x: Int64): Unit {
    println("Base.f(${x})")
  }
}

func g1<T>(a: T) where T <: Base {
  let ff: (Int32) -> Unit = a.f
  ff(42)
}

func g2<T>(a: T) where T <: Base {
  let ff: (Int32) -> Unit = { x => a.f(x) }
  ff(42)
}

main() {
  g1(Base())
  g2(Base())
}
