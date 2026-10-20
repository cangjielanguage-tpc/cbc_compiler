import pkgA.*

main() {
  let k = Key(42)
  if (k == Key(42)) {
    println(k.hashCode())
  }
}
