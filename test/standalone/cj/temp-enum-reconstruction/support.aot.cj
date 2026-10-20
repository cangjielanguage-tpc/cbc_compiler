package enum_support

public interface Probe {
    func apply<T>(value: T): T
}

// An AOT generic container also checks that reconstructed T is compatible
// with the native runtime when CBC builds another generic TypeInfo using it.
public class NativeBox<T> {
    public let value: T
    public init(value: T) { this.value = value }
}

public func unwrap<T>(box: Object): T {
    match (box as NativeBox<T>) {
        case Some(typed) => typed.value
        case None => throw Exception("incompatible NativeBox TypeInfo")
    }
}

public class Payload {
    public let text: String
    public init(text: String) { this.text = text }
}

public enum NativeEnum {
    Text(String) | Number(Int64) | Empty
}

public enum NativeGeneric<T> {
    Value(T) | Number(Int64) | Empty
}

private enum HiddenEnum {
    Value(Payload) | Number(Int64) | Empty
}

private enum HiddenGeneric<T> {
    Value(T) | Number(Int64) | Empty
}

public func runNongeneric(probe: Probe): Bool {
    for (_ in 0..2) {
        let text = "native-" + Int64(123).toString()
        match (probe.apply<NativeEnum>(NativeEnum.Text(text))) {
            case Text(actual) => if (actual != text) { return false }
            case _ => return false
        }
    }
    true
}

public func runGeneric(probe: Probe): Bool {
    for (_ in 0..2) {
        let payload = Payload("generic-" + Int64(456).toString())
        match (probe.apply<NativeGeneric<Payload>>(NativeGeneric<Payload>.Value(payload))) {
            case Value(actual) => if (!refEq(actual, payload)) { return false }
            case _ => return false
        }
        match (probe.apply<NativeGeneric<Int64>>(NativeGeneric<Int64>.Value(77))) {
            case Value(77) => ()
            case _ => return false
        }
        let nested = NativeGeneric<NativeGeneric<Payload>>.Value(NativeGeneric<Payload>.Value(payload))
        match (probe.apply<NativeGeneric<NativeGeneric<Payload>>>(nested)) {
            case Value(Value(actual)) => if (!refEq(actual, payload)) { return false }
            case _ => return false
        }
    }
    true
}

public func runOpaque(probe: Probe): Bool {
    for (_ in 0..2) {
        let payload = Payload("opaque-" + Int64(789).toString())
        match (probe.apply<HiddenEnum>(HiddenEnum.Value(payload))) {
            case Value(actual) => if (!refEq(actual, payload)) { return false }
            case _ => return false
        }
        match (probe.apply<HiddenGeneric<Payload>>(HiddenGeneric<Payload>.Value(payload))) {
            case Value(actual) => if (!refEq(actual, payload)) { return false }
            case _ => return false
        }
        match (probe.apply<HiddenGeneric<Int64>>(HiddenGeneric<Int64>.Value(99))) {
            case Value(99) => ()
            case _ => return false
        }
    }
    true
}
