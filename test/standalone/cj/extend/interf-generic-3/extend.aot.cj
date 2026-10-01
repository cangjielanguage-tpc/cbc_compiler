package aot

public interface J {
  func foo(): Unit
}

public interface I<T> <: J {
  func foo(): Unit
}

public class ComplexType<K, T> {
  public var value: T
  public init(value: T) { this.value = value }
}

public class Box<T> {
  public var value: T
  public init(value: T) { this.value = value }
}

public class A<K, T> {
  public var value: T
  public init(value: T) { this.value = value }
}

public class S {}

public var obj = Object()

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
