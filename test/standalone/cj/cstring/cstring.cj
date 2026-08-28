
main() {
    let cjString: String = "Hello"
    let cString: CString = unsafe { LibC.mallocCString(cjString) }
    println("C-String: " + cString.toString())
    unsafe { LibC.free(cString) }
    0
}
