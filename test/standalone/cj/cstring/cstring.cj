
main() {
    let cjString: String = "Hello"
    let cString: CString = unsafe { LibC.mallocCString(cjString) }
    println("C-String: " + cString.toString())
    unsafe {
        let chars = cString.getChars()
        println(chars.read())
        println(chars.read(4))
        println(chars.read(5))
        let alias = CString(chars)
        println(alias.getChars().toUIntNative() == chars.toUIntNative())
        chars.write(1, 97u8)
        println(alias.toString())
        println(CString(chars + 1).toString())
    }
    unsafe { LibC.free(cString) }
    0
}
