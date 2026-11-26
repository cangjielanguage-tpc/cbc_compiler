func incompatible(value: Option<(Int64, Int64)>): Option<Option<Int16>> {
  value as Option<Int16>
}

func compatible(value: Any): Option<Option<Int16>> {
  value as Option<Int16>
}

main() {
  match (incompatible(Some((12, 34)))) {
    case None => println("rejected")
    case Some(_) => println("unexpected success")
  }

  match (compatible(Option<Int16>.Some(42))) {
    case Some(Some(item)) => println(item)
    case _ => println("unexpected rejection")
  }
}
