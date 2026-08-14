import aot.*

public open class C <: I<C> {

}

main() {
  C().foo()
  0
}
