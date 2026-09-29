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

func forwardTag(tag: Int64): Int64 {
    tag
}

func tagOf(source: DnsSource): Int64 {
    forwardTag(source.item)
}

main(): Int64 {
    println(tagOf(DnsSource.System))
    0
}
