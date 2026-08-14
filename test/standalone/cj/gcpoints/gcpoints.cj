import aot

var end = 0

main() {
  aot.testGcPoint()

  var counter = 0x10000
  while (counter != end) {
    counter = counter - 1
    // gcpoint will be here
  }
  counter
}
