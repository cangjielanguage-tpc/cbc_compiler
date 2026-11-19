// Exercises the std.core 32-byte block string helpers (vectorCompare32 /
// vectorIndexByte32).
//
// All strings are longer than 32 bytes so that the vectorized block paths in
// std.core are taken; differences are placed inside a block, at the block
// boundary (index 31/32) and at the very end to cover the tail handling.

func showEq(label: String, a: String, b: String): Unit {
    println("eq ${label} ${a == b}")
}

func showCmp(label: String, a: String, b: String): Unit {
    println("cmp ${label} ${a.compare(b)}")
}

func showIdx(label: String, s: String, sub: String): Unit {
    match (s.indexOf(sub)) {
        case Some(i) => println("idx ${label} ${i}")
        case None => println("idx ${label} -1")
    }
}

main(): Int64 {
    // 64 'a' characters.
    let base = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    // Same content, distinct literal.
    let same = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    // Differ inside the first block (index 16).
    let diff16 = "aaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    // Differ in the last byte of the first block (index 31).
    let diff31 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    // Differ in the first byte of the second block (index 32).
    let diff32 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    // Differ in the last byte (index 63).
    let diff63 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"
    // Shorter prefix of base (48 bytes).
    let shorter = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    // Longer than base, same prefix (68 bytes).
    let longer = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

    showEq("same", base, same)
    showEq("diff16", base, diff16)
    showEq("diff31", base, diff31)
    showEq("diff32", base, diff32)
    showEq("diff63", base, diff63)
    showEq("shorter", base, shorter)
    showEq("longer", base, longer)
    showEq("bothEmpty", "", "")

    showCmp("same", base, same)
    showCmp("diff16", base, diff16)
    showCmp("diff31", base, diff31)
    showCmp("diff32", base, diff32)
    showCmp("diff63", base, diff63)
    showCmp("shorter", base, shorter)
    showCmp("longer", base, longer)

    // 31 'a', then "bcde", then 29 'a' (64 bytes).
    //   marked[31..34] = "bcde", so "abcde" starts at 30.
    let marked = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabcdeaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    // 40 'a', then "bcd", then 21 'a' (64 bytes).
    //   The needle crosses the block boundary between bytes 31 and 32.
    let crossed = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabcdaaaaaaaaaaaaaaaaaaaaa"

    showIdx("start", base, "aaaa")
    showIdx("whole", base, base)
    showIdx("absent", base, "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb")
    showIdx("tooLong", shorter, base)
    showIdx("marker", marked, "bcde")
    showIdx("marker2", marked, "bc")
    showIdx("marker3", marked, "cde")
    showIdx("markerCross", marked, "abcde")
    showIdx("markerAbsent", marked, "bcdf")
    showIdx("crossBlock", crossed, "bcd")
    showIdx("crossBlock2", crossed, "cd")
    showIdx("crossEmpty", crossed, "")
    return 0
}
