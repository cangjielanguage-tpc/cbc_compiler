enum E {
  | A
  | B(E)
}

func bar(x: (E, E)) {
  match (x) {
    case (B(_), B(_)) => println("B B")
    case _ => println("other")
  }
}

main() {
  bar((A, A))
  bar((B(A), A))
  bar((B(A), B(A)))
  bar((B(B(A)), B(A)))
}
