interface J {
  func foo(): Unit
}

interface I<T> <: J {
  func foo(): Unit
}

class ComplexType<K, T> {
  var value: T
  init(value: T) { this.value = value }
}

class Box<T> {
  var value: T
  init(value: T) { this.value = value }
}

class A<K, T> {
  var value: T
  init(value: T) { this.value = value }
}

class S {}

var obj = Object()

// CJNative tries to access type `T` using typeinfo of extended class.
// So, creation of Box<T> would look like a following pseudo-asm:
// ```
// outerTI == TI[A<S, ComplexType<S, T>>]
// tmp1 = outerTI.typeargs[1] // get TI[ComplexType<S, T>]
// tmp2 = outerTI.typeargs[1] // get TI[T]
// boxTT = lea[TT[Box]]
// boxTI = GetOrCreateTypeInfo(boxTT, 1, {tmp2})
// boxObj = NewObject(boxTI)
// ... object construction
// ```
//
// But cbc compiler can't (and don't actually want) to replicate such complex logic.
// It accesses `T` as 0-th type variable in `extend` type context:
// ```
// outerTI == TI[GenericContext<T>] // special synthetic TypeInfo
// IR1 := outerTI.typeargs[0] // get TI[T]
// boxTI = load.typeinfo.g(Term[Box<%0>]) // uses IR1 implicitly
// boxObj = NewObject(boxTI)
// ... object construction
// ```
extend<T> A<S, ComplexType<S, T>> <: I<ComplexType<Int64, ComplexType<T, Int64>>> {
  public func foo(): Unit {
    obj = Box<T>(this.value.value)
  }
}

let x1 = ComplexType<S, Int64>(1)
let x2 = ComplexType<S, Int16>(2)
let x3 = ComplexType<S, Int8>(3)
let x4 = ComplexType<S, Int32>(4)

var g: Array<J> = [A<S, ComplexType<S, Int64>>(x1), A<S, ComplexType<S, Int16>>(x2), A<S, ComplexType<S, Int8>>(x3), A<S, ComplexType<S, Int32>>(x4)]

main() {
  for (o in g) {
    o.foo()
    match (obj) {
      case b: Box<Int64> => println("i64 ${b.value}")
      case b: Box<Int32> => println("i32 ${b.value}")
      case b: Box<Int8>  => println("i8 ${b.value}")
      case b: Box<Int16> => println("i16 ${b.value}")
      case _             => println("unknown")
    }
  }
  0
}

