import hotfix.patchable

@patchable
func foo(x: Int64, y: Int64) {
  try {
    x / y
  } catch (e: ArithmeticException) {
    return x
  }
}