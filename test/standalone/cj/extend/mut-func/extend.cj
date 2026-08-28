struct S {
  S(var x: Int64, var y: Int64) {}

  mut func setX(x: Int64) { this.x = x }
  mut func setY(y: Int64) { this.y = y }
}

extend S {

  mut func setBoth(x: Int64, y: Int64) {
    setX(x)
    setY(y)
  }

  mut func setFields(x: Int64, y: Int64) {
    this.x = x
    this.y = y
  }
}

func test1() {
  var s = S(1, 2)
  prints(s)
  s.setBoth(3, 4)
  prints(s)
}

func test2() {
  var s = S(1, 2)
  prints(s)
  s.setFields(5, 6)
  prints(s)
}

var s: S = S(1, 2)

func test3() {
  prints(s)
  s.setFields(7, 8)
  prints(s)
}

func prints(s: S) {
  println(s.x)
  println(s.y)
}


main() {
  test1()
  test2()
  test3()
  0
}
