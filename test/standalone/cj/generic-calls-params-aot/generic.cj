import aot

func foo(a: Int64, b: Float64, c: String, d: Object): Unit {
  aot.globalFunc(a, b)
  aot.globalFunc(a, c)
  aot.globalFunc(c, d)
  aot.globalFunc(d, a)

  aot.C<Int64, Float64>.staticFunc(a, b)
  aot.C<Int64, String>.staticFunc(a, c)
  aot.C<String, Object>.staticFunc(c, d)
  aot.C<Object, Int64>.staticFunc(d, a)

  aot.C<Int64, Float64>().instanceFunc(a, b)
  aot.C<Int64, String>().instanceFunc(a, c)
  aot.C<String, Object>().instanceFunc(c, d)
  aot.C<Object, Int64>().instanceFunc(d, a)

  aot.C<Int64, Float64>.staticFuncG(a, b, d, c)
  aot.C<Int64, String>.staticFuncG(a, c, c, b)
  aot.C<String, Object>.staticFuncG(c, d, b, c)
  aot.C<Object, Int64>.staticFuncG(d, a, d, b)

  aot.C<Int64, Float64>().instanceFuncG(a, b, d, c)
  aot.C<Int64, String>().instanceFuncG(a, c, c, b)
  aot.C<String, Object>().instanceFuncG(c, d, b, c)
  aot.C<Object, Int64>().instanceFuncG(d, a, d, b)
}

main() {
  foo(42, 3.5, "foo", Object())
  0
}

