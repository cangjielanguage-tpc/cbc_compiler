enum InnerEnum {
    InnerEnum
}

enum OuterEnum {
    OuterEnum(InnerEnum)
}

func foo(e: OuterEnum): Int32 {
    match (e) {
        case OuterEnum(InnerEnum) => 2
    }
}

main() {
    println(foo(OuterEnum.OuterEnum(InnerEnum.InnerEnum)))
    return 0
}
