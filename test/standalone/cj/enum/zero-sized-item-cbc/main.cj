// A one-case, payload-free enum is represented as a zero-sized enum.
enum DnsSource {
    | System
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
