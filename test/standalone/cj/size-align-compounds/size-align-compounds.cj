package default

@C
struct Mixed {
    var first: Int16 = 0
    var repeated: Int16 = 0
    var last: UInt8 = 0
}

@C
struct NarrowWide {
    var first: UInt8 = 0
    var repeated: UInt8 = 0
    var last: Int64 = 0
}

@C
struct WideNarrow {
    var first: Int64 = 0
    var repeated: Int64 = 0
    var last: UInt8 = 0
}

@C
struct Repeated {
    var first: UInt8 = 0
    var second: UInt8 = 0
    var third: UInt8 = 0
}

@C
struct Nested {
    var value: NarrowWide = NarrowWide()
}

@C
struct ArrayRecord {
    var prefix: UInt8 = 0
    var values: VArray<Int64, $3> = [0, 0, 0]
}

func queryCompounds<T, U>() where T <: CType, U <: CType {
    println(sizeOf<T>())
    println(alignOf<T>())
    println(sizeOf<U>())
    println(alignOf<U>())
}

main() {
    // Public layout queries require CType; @C records cannot be generic.
    println(sizeOf<Mixed>())
    println(alignOf<Mixed>())
    println(sizeOf<NarrowWide>())
    println(alignOf<NarrowWide>())
    println(sizeOf<WideNarrow>())
    println(alignOf<WideNarrow>())
    println(sizeOf<Repeated>())
    println(alignOf<Repeated>())
    println(sizeOf<Nested>())
    println(alignOf<Nested>())
    println(sizeOf<ArrayRecord>())
    println(alignOf<ArrayRecord>())
    // Runtime TypeInfo queries must observe these different compound layouts.
    queryCompounds<Mixed, NarrowWide>()
    queryCompounds<NarrowWide, Mixed>()
    queryCompounds<Repeated, Repeated>()
    queryCompounds<WideNarrow, Nested>()
    queryCompounds<ArrayRecord, Repeated>()
    0
}
