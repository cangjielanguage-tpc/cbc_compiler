unsafe main() {
  let ptr = CPointer<UInt16>()
  let ptr1 = CPointer<UInt8>(ptr)
  if (ptr1.isNull()) {
    println("Hoho!")
  }
  let live = LibC.malloc<UInt64>(count: 1)
  let alias = CPointer<UInt64>(live)
  alias.write(7)
  if (alias.read() == 7) {
    println("Haha!")
  }
  0
}
