let g : h = i()
interface h {}
class i <: h  {}
enum s<n> {
    t | u
}

interface Item {
    func value(): Int64
}

class Number <: Item {
    public func value(): Int64 { 42 }
}

func choose(flag: Bool, item: Item): (Item, Bool) {
    match (flag) {
        case true => (item, false)
        case _ => (Number(), true)
    }
}

func testMatch(): Int64 {
    match (match (s<Int16>.u) {
        case t =>
            ( g,true)
        case _ =>
            ( i(), true)
    }) {
        case (_ ,ai) =>
            5
    }
}

open class Base {}
class Derived <: Base {
    public func value(): Int64 { 42 }
}

func identity<T>(value: T): T { value }

func checkTupleInstanceOf(args: Array<String>) {
    let yes: Base = if (args.size == 0) { Derived() } else { Base() }
    let no: Base = if (args.size == 0) { Base() } else { Derived() }
    let positive = identity<(Base, Base)>((Derived(), yes))
    let negative = identity<(Base, Base)>((Derived(), no))
    println(positive is (Base, Derived))
    println(negative is (Base, Derived))
    match (positive as (Base, Derived)) {
        case Some((_, derived)) => println(derived.value())
        case None => println("unexpected cast failure")
    }
    println((negative as (Base, Derived)).isNone())
}

main(args: Array<String>) {
    println(testMatch())
    let (item, flag) = choose(false, Number())
    println(item.value())
    println(flag)
    match ((7, (Number(), false)) as (Int64, (Item, Bool))) {
        case Some((tag, (nested, flag))) =>
            println(tag)
            println(nested.value())
            println(flag)
        case None => println("unexpected cast failure")
    }
    checkTupleInstanceOf(args)
    return 0
}
