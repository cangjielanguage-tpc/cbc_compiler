var i = 42
var j = jvalue()
func jvalue() {
  37
}
class Once {
  public func foo(): Unit {
    println("ONCE!")
    println(i)
    println(j)
  }
}
var res = Once().foo()
main() {
  0
}

