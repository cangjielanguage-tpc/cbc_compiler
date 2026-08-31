public enum Direction {
  North | East | South | West
}

func name(direction: Direction): String {
  match (direction) {
    case North => "North"
    case East  => "East"
    case South => "South"
    case West  => "West"
  }
}

main() {
  let boxedDirections: Array<Any> = [North, East, South, West]

  for (boxedDirection in boxedDirections) {
    let direction = (boxedDirection as Direction).getOrThrow()
    println(name(direction))
  }

  0
}
