class Wrapper {
  Wrapper(let value: Int64) {}
}

func createSquares(n: Int64): Array<Wrapper> {
  let arr = Array(n, repeat: Wrapper(0))
  for (i in 0..n) {
    arr[i] = Wrapper(i * i);
  }
  arr
}

func printSquares(arr: Array<Wrapper>): Unit {
  for (x in arr) {
    println(x.value)
  }
}

main() {
  let arr = createSquares(10)
  printSquares(arr)
  0
}
