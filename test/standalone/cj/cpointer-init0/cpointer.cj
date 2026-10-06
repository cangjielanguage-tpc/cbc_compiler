main(): Int64 {
  let ptr = CPointer<CPointer<CPointer<Uint16>>>()
  if (ptr.isNull()) {
    println("hoho")
  } else {
    println("hihi")
  }
  return 0
}
