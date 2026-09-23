// Regression test for GetRTTIStatic on a class generic parameter.

interface Factory<M> {
  static func create(): M
}

var creations: Int64 = 0

class Product <: Factory<Product> {
  Product(public let id: Int64) {}

  public static func create(): Product {
    creations += 1
    Product(11)
  }
}

class OtherProduct <: Factory<OtherProduct> {
  OtherProduct(public let id: Int64) {}

  public static func create(): OtherProduct {
    creations += 1
    OtherProduct(22)
  }
}

class GenericProduct<T> <: Factory<GenericProduct<T>> {
  public let value: ?T

  GenericProduct() { value = None }

  public static func create(): GenericProduct<T> {
    creations += 1
    GenericProduct<T>()
  }
}

struct Holder<M> where M <: Factory<M> {
  public Holder(public var value: ?M) {}

  public mut func getOrCreate(): M {
    if (let Some(value) <- value) {
      value
    } else {
      M.create()
    }
  }
}

main(): Int64 {
  var holder = Holder<Product>(None)
  if (holder.getOrCreate().id != 11 || creations != 1) { return 1 }

  var cached = Holder<Product>(Some(Product(33)))
  if (cached.getOrCreate().id != 33 || creations != 1) { return 2 }

  var other = Holder<OtherProduct>(None)
  if (other.getOrCreate().id != 22 || creations != 2) { return 3 }

  // The implementation needs GenericProduct<Int64>'s outer TI, not Factory's TI.
  var generic = Holder<GenericProduct<Int64>>(None)
  if (generic.getOrCreate().value.isSome() || creations != 3) { return 4 }
  0
}
