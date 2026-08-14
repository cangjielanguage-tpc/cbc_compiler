import aot

var field32: Int32 = 100
var field64: Int64 = 155
var refField: aot.Point = aot.Point()

main() {
  refField.x = 123
  refField.y = 321

  aot.checkObj(refField)

  Int64(field32) + field64
}
