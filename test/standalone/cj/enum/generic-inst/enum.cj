enum Time<T> {
  Hour(Bool, T)
}

func bar(e: Time<Int64>) : Int64 {
  match (e) {
    case Hour(x, y) =>
      println(x)
      println(y)
      y
  }
}

main(): Unit {
  bar(Time<Int64>.Hour(true, 9))
}
