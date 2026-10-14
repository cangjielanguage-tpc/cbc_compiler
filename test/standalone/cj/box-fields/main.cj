struct Foo {
	var f: Option<Foo> = None
}

var f = Foo()

main() {
  f.f = f
	match (f.f) {
		case Some(x) => println("some")
		case _       => println("none")
	}
}

