class Empty {}

func test_repeat_initializer() {
    let _ = VArray<Int64, $10000>(repeat: 2)
    let _ = Empty()
}

func test_lambda_initializer() {
    let _ = VArray<Int64, $10000>({ index => index * 2 })
    let _ = Empty()
}

main() {
    test_repeat_initializer()
    test_lambda_initializer()
}
