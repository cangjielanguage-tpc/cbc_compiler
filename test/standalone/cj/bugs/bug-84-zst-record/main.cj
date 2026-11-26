struct SSSS {
    public var xxxx: Unit = ()
}

enum EXHAUSTIVE_ZERO_SIZE {
    A(SSSS)
}

func foo(e: EXHAUSTIVE_ZERO_SIZE) {
    match (e) {
        case A(ssss) =>
            print("AAA");
            println(ssss.xxxx)
    }
}

main() {
    foo(EXHAUSTIVE_ZERO_SIZE.A(SSSS()))
    return 0
}
