import std.collection.HashMap

// A non-option enum with a reference-bearing payload must use Cangjie's
// class-based representation.  It is deliberately used as a HashMap value so
// the runtime has to materialize its type information as a generic argument.
enum Value {
    | Text(String)
    | IntValue(Int32)
    | UIntValue(UInt32)
}

func getText(values: HashMap<String, Value>): String {
    match (values.get("text")) {
        case Some(Value.Text(value)) => value
        case _ => "missing"
    }
}

main(): Int64 {
    let values = HashMap<String, Value>()
    values.add("text", Value.Text("reference payload"))
    values.add("int", Value.IntValue(42))
    values.add("uint", Value.UIntValue(7))

    println(getText(values))
    0
}
