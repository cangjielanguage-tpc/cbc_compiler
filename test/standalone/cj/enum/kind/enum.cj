import std.collection.HashMap

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

main() {
    let values = HashMap<String, Value>()
    values.add("text", Value.Text("reference payload"))
    values.add("int", Value.IntValue(42))
    values.add("uint", Value.UIntValue(7))
    println(getText(values))
}
