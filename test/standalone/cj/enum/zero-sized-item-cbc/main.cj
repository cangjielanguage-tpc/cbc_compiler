// A one-case, payload-free enum is represented as a zero-sized enum.
enum DnsSource {
    | System

    func toInt64(): Int64 {
        match (this) {
            case System => 1
        }
    }

    protected prop item: Int64 {
        get() {
            toInt64()
        }
    }
}

func itemOf(source: DnsSource): Int64 {
    source.item
}

func intOf(source: DnsSource): Int64 {
    source.toInt64()
}

main(): Int64 {
    println(intOf(DnsSource.System))
    println(itemOf(DnsSource.System))
    0
}
