/*
gcc -shared -fPIC cpointer.c -o libcpointer.so
cjc cpointer.cj -L ~/Projects/cpointer -l cpointer --emit-chir -o cpointer.chir -O2
cbc-compiler -cbcaotdeps=cpointer +genlibrary -outputname=cpointer cpointer.chir -decor=rhtp -silentcompilation +debugirlogs -logonlyproc=main
launcher --dasm cpointer.cbc
*/

@C
struct TestStruct {
    public var a: Int64 = 1
    public var b: Int64 = 2

    public init(a!: Int64, b!: Int64) {
        this.a = a
        this.b = b
    }
}

foreign {
    func createStruct(i: Int64, j: Int64): CPointer<TestStruct>
    func createPrim(i: Int64): CPointer<Int64>
}

unsafe main():Int64 {
    let sPtr = createStruct(43, 44)
    var s = sPtr.read()
    println(s.a)
    println(s.b)

    sPtr.write(TestStruct(a: 10, b: 11))
    s = sPtr.read()
    println(s.a)
    println(s.b)


    let primPtr = createPrim(42)
    var prim = primPtr.read()
    println(prim)
    prim = primPtr.read(1)
    println(prim)

    primPtr.write(45)

    prim = primPtr.read()
    println(prim)

    prim = primPtr.read(1)
    println(prim)

    LibC.free<TestStruct>(sPtr)
    LibC.free<Int64>(primPtr)

    return 0
}



