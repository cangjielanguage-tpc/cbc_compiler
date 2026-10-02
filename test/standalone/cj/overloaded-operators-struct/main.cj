interface Integer<T> {
  operator func +(x: T): T
  operator func -(): T
}

func foo<T>(x: T, y: T, n: Int64): T where T <: Integer<T> {
  if (n == 0) {
    x + y
  } else {
    -x
  }
}

// Cannot extend AOT type (Int64)
// extend Int64 <: Integer<Int64> {}

struct Wrapper <: Integer<Wrapper> {
  Wrapper(let value: Int64) {}

  public operator func +(x: Wrapper): Wrapper {
    Wrapper(this.value + x.value)
  }

  public operator func -(): Wrapper {
    Wrapper(-this.value)
  }
}

main() {
  println(foo<Wrapper>(Wrapper(2), Wrapper(3), 0).value)
  println(foo<Wrapper>(Wrapper(2), Wrapper(3), 1).value)
}
