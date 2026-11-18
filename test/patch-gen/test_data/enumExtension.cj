import hotfix.patchable

enum Existing {
    Value(Int64)
}

extend Existing {
    func number(): Int64 {
        match (this) {
            case Value(n) => n
        }
    }
}

@patchable
func value(n: Int64): Int64 {
    Existing.Value(n).number()
}
