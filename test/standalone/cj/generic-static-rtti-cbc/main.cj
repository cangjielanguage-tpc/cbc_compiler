// Regression test for GetRTTIStatic on a class generic parameter.

interface Factory<M> {
  static func create(): M
}

class Product <: Factory<Product> {
  Product() {}

  public static func create(): Product {
    Product()
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
  holder.getOrCreate()
  0
}
