import hotfix.patchable

main() { run() }

@patchable
func run(): Unit {
  let base = Base()
  let derived = Derived()
  base.printMember()
  derived.printMember()
}

open class Base {
  @patchable
  protected open func printVirtual(): Unit {
    println("patched base")
  }

  @patchable
  protected static func printStatic(): Unit {
    println("patched static")
  }

  @patchable
  public func printMember(): Unit {
    println("patched member")
    printVirtual()
    printStatic()
  }
}

class Derived <: Base {
  @patchable
  protected override func printVirtual(): Unit {
    println("patched derived")
  }
}
