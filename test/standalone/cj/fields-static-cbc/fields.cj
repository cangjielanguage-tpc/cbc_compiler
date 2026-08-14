import aot

var field32: Int32 = 0
var field64: Int64 = 0
var refField: aot.Point = aot.Point()

main() {
  refField = aot.Point()
  refField.x = 123
  refField.y = 321

  field32 = 100
  field64 = 155

  aot.checkObj(refField)

  Int64(field32) + field64
}
