func sfoo(
    a0: String,
    a1: String,
    a2: String,
    a3: String,
    a4: String,
    a5: String,
    a6: String,
    a7: String,
    a8: String,
    a9: String) {
  println(a0)
  println(a1)
  println(a2)
  println(a3)
  println(a4)
  println(a5)
  println(a6)
  println(a7)
  println(a8)
  println(a9)
}

func ifoo(
    a0: Int64,
    a1: Int64,
    a2: Int64,
    a3: Int64,
    a4: Int64,
    a5: Int64,
    a6: Int64,
    a7: Int64,
    a8: Int64,
    a9: Int64) {
  println(a0)
  println(a1)
  println(a2)
  println(a3)
  println(a4)
  println(a5)
  println(a6)
  println(a7)
  println(a8)
  println(a9)
}

func ffoo(
    a0: Float64,
    a1: Float64,
    a2: Float64,
    a3: Float64,
    a4: Float64,
    a5: Float64,
    a6: Float64,
    a7: Float64,
    a8: Float64,
    a9: Float64) {
  println(a0)
  println(a1)
  println(a2)
  println(a3)
  println(a4)
  println(a5)
  println(a6)
  println(a7)
  println(a8)
  println(a9)
}

public func mfoo(
    a0: String,
    a1: Int64,
    a2: Float64,
    a3: String,
    a4: Int64,
    a5: Float64,
    a6: String,
    a7: Int64,
    a8: Float64,
    a9: String,
    a10: Int64,
    a11: Float64,
    a12: String) {
  println(a0)
  println(a1)
  println(a2)
  println(a3)
  println(a4)
  println(a5)
  println(a6)
  println(a7)
  println(a8)
  println(a9)
  println(a10)
  println(a11)
  println(a12)
}

main() {
  sfoo("a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9")
  ifoo(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  ffoo(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0)
  mfoo("a0", 1, 2.0, "a3", 4, 5.0, "a6", 7, 8.0, "a9", 10, 11.0, "a12")
  0
}
