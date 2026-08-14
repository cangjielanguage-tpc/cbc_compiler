import aot

main() {
  aot.globalFunc(42, 3.5)
  aot.globalFunc(42, "foo")
  aot.globalFunc("bar", Object())
  aot.globalFunc(Object(), 42)

  aot.C<Int64, Float64>.staticFunc(42, 3.5)
  aot.C<Int64, String>.staticFunc(42, "foo")
  aot.C<String, Object>.staticFunc("bar", Object())
  aot.C<Object, Int64>.staticFunc(Object(), 42)

  aot.C<Int64, Float64>().instanceFunc(42, 3.5)
  aot.C<Int64, String>().instanceFunc(42, "foo")
  aot.C<String, Object>().instanceFunc("bar", Object())
  aot.C<Object, Int64>().instanceFunc(Object(), 42)

  aot.C<Int64, Float64>.staticFuncG(42, 3.5, Object(), "foo")
  aot.C<Int64, String>.staticFuncG(42, "foo", "bar", 3.5)
  aot.C<String, Object>.staticFuncG("bar", Object(), 3.5, "foo")
  aot.C<Object, Int64>.staticFuncG(Object(), 42, Object(), 3.5)

  aot.C<Int64, Float64>().instanceFuncG(42, 3.5, Object(), "foo")
  aot.C<Int64, String>().instanceFuncG(42, "foo", "bar", 3.5)
  aot.C<String, Object>().instanceFuncG("bar", Object(), 3.5, "foo")
  aot.C<Object, Int64>().instanceFuncG(Object(), 42, Object(), 3.5)

  0
}

