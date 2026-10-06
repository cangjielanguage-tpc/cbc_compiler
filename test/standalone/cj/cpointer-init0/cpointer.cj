main(): Int64 {
  let ptr = CPointer<CPointer<CPointer<UInt16>>>()
  if (ptr.isNull()) {
    println("hoho")
  } else {
    println("hihi")
  }
  return 0
}
