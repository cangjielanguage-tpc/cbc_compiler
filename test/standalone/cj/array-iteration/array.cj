
func createSquares(n: Int64): Array<Int64> {
  let arr = Array(n, repeat: 0)
  for (i in 0..n) {
    arr[i] = i * i;
  }
  arr
}

func printSquares(arr: Array<Int64>): Unit {
  for (x in arr) {
    println(x)
  }
}

main() {
  let arr = createSquares(10)
  printSquares(arr)
  0
}
