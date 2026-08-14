import aot

main() {
  aot.testStaticVar = 40

  aot.testRefVar = Some(Object())
  aot.testAssign(2)
  aot.testStaticVar
}
