interface I<T> {
  static func f(x: T): Unit {
    println("I.f")
  }
}

open class Base <: I<Int64> & I<Int32> {
  public static func f(x: Int64): Unit {
    println("Base.f(${x})")
  }
}

func g1<T>(a: T) where T <: Base {
  let ff: (Int32) -> Unit = T.f
  ff(42)
}

func g2<T>(a: T) where T <: Base {
  let ff: (Int32) -> Unit = { x => T.f(x) }
  ff(42)
}

main() {
  g1(Base())
  g2(Base())
}
