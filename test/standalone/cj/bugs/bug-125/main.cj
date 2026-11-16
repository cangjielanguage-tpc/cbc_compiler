public func goo<M>(y:M):(M, VArray<Int64, $3>) {
  var aaa:VArray<Int64, $3> = [111, 222, 333]
  (y, aaa)  // <--- CRASH HERE
}
main() {
  println(goo<Int32>(444)[1][1])
}
