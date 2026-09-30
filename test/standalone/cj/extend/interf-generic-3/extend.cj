import aot.Box as Box2
import aot.*

let x1 = ComplexType<S, Int64>(1)
let x2 = ComplexType<S, Int16>(2)
let x3 = ComplexType<S, Int8>(3)
let x4 = ComplexType<S, Int32>(4)

var g: Array<J> = [A<S, ComplexType<S, Int64>>(x1), A<S, ComplexType<S, Int16>>(x2), A<S, ComplexType<S, Int8>>(x3), A<S, ComplexType<S, Int32>>(x4)]

main() {
  for (o in g) {
    o.foo()
    match (obj) {
      case b: Box2<Int64> => println("i64 ${b.value}")
      case b: Box2<Int32> => println("i32 ${b.value}")
      case b: Box2<Int8>  => println("i8 ${b.value}")
      case b: Box2<Int16> => println("i16 ${b.value}")
      case _             => println("unknown")
    }
  }
  0
}

