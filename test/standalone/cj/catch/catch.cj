import aot

// --- Test 1 ---
// One try-catch with several handlers

func test1(x: Int64, y: Int64): Int64 {
  try {
    y / x * x - x / y
  } catch(e: IllegalStateException) {
    return 42
  } catch(e: IllegalArgumentException) {
    return 43
  } catch(e: ArithmeticException) {
    return 44
  }
}

// --- Test 2 ---
// Try-catch inside try-catch

func test2(x: Int64, y: Int64): Int64 {
  try {
    try {
      x / y
    } catch(e: ArithmeticException) {
      x / y
      return 42
    }
  } catch(e: IllegalArgumentException) {
    return 43
  } catch(_: Exception) {
    return 44
  }
}

// --- Test 3 ---
// Throw in an interpreted method, catch in another interpreted method

func test3_helper2(x: Int64, y: Int64): Int64 {
    if (x > y + 1) {
        throw IllegalArgumentException()
    }
    return 42
}

func test3_helper1(x: Int64, y: Int64): Int64 {
    if (x > y) {
        test3_helper2(x, y)
    }
    return 42
}

func test3(x: Int64, y: Int64): Int64 {
    try {
        return test3_helper1(x, y)
    } catch(e: ArithmeticException) {
        return 43
    } catch(e: IllegalArgumentException) {
        return 44
    }
}

// --- Test 4 ---
// Throw in AOT method, catch in an interpreted method

class TestClass {
    func test4_helper1(x: Int64, y: Int64): Int64 {
        if (x > y) {
            aot.test4_helper2(x, y)
        }
        return 42
    }
}

func test4(x: Int64, y: Int64): Int64 {
    try {
        return TestClass().test4_helper1(x, y)
    } catch(e: ArithmeticException) {
        e.printStackTrace()
        return 43
    } catch(e: IllegalArgumentException) {
        e.printStackTrace()
        return 44
    }
}

// --------------

main(): Int64 {
  test1(0, 0) +
  test2(0, 0) +
  test3(2, 0) +
  test4(2, 0)
}
