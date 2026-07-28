/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.common.io

import org.scalatest.funsuite.AnyFunSuite
import xscala.io.Path
import xscala.io.Path.*

import java.io.IOException

/** Tests for Path class.
  *
  * @author ikireev
  */
class PathSuite extends AnyFunSuite {

  test("absolute") {
    assert(Path("/path/to/smth") == abs("/path/to/smth"))
    assert(Path("/path/to/smth/") == abs("/path/to/smth"))
    assert(Path("//path//to//smth//") == abs("/path/to/smth"))
    assert(Path("/") == abs("/"))
    assert(Path("////") == abs("/"))
    assert(Path("""C:\path\to\smth""") == abs("""C:\path\to\smth"""))
    assert(Path("""C:\path\to\smth\""") == abs("""C:\path\to\smth"""))
    assert(Path("""C:\\path\\to\\smth\\""") == abs("""C:\path\to\smth"""))
    assert(Path("""C:\""") == abs("""C:\"""))
    assert(Path("""C:\\\\\""") == abs("""C:\"""))
    assert(Path("""\\server\path\to\smth""") == abs("""\\server\path\to\smth"""))
    assert(root("/") == abs("/"))
    assert(root("/path/to/smth/") == root("/"))

    assert(Path("/path/to/smth/..") == abs("/path/to"))
    assert(Path("/path/to/../smth") == abs("/path/smth"))
    assert(Path("/path/../to/smth") == abs("/to/smth"))
    assert(Path("/path/to/smth/../..") == abs("/path"))
    assert(Path("/path/to/../../smth") == abs("/smth"))
    assert(Path("/path/to/smth/../../..") == abs("/"))
    assert(Path("///path/.///to/./././../../smth/../smth////./") == abs("/smth"))

    assert(Path("/path/to/smth").parent == abs("/path/to"))
    assert(Path("/path/to/smth").up(3) == abs("/"))
    assert(Path("/path/to/smth")/".." == abs("/path/to"))
    assert(Path("/path/to/smth")/"../../.." == abs("/"))
  }

  test("relative") {
    assert(Path(".") == dot)
    assert(Path("./") == dot)
    assert(Path(".////") == dot)
    assert(Path("..") == up(1))
    assert(Path("../") == up(1))
    assert(Path("..////") == up(1))
    assert(Path("./..") == up(1))
    assert(Path("../.") == up(1))
    assert(Path("../../..") == up(3))
    assert(Path("..//.//..//..") == up(3))
    assert(Path("./././") == dot)
    assert(Path(".//.//.//") == dot)

    assert(Path("./path/to/smth") == dot/"path/to/smth")
    assert(Path("../path/to/smth") == up(1)/"path/to/smth")
    assert(Path("../../../path/to/smth") == up(3)/"path/to/smth")
    assert(Path("../../path/../to/../smth") == up(2)/"smth")
    assert(Path("../../path/../to/../..") == up(3))

    assert(Path("./")/".." == up(1))
    assert(Path("./").parent == up(1))
    assert(dot.parent == up(1))
    assert(Path("../..").parent == up(3))
    assert(Path("../..").up(5) == up(7))
  }

  test("invalid") {
    assertThrows[IllegalArgumentException](Path("/.."))
    assertThrows[IllegalArgumentException](Path("/path/to/smth/../../../.."))

    assertThrows[ClassCastException](rel("/"))
    assertThrows[ClassCastException](abs("."))
    assertThrows[ClassCastException](abs(".."))
    assertThrows[ClassCastException](dot/"/")

    assertThrows[IOException](abs("/")/"..")
    assertThrows[IOException](abs("/")/"./path/../..")
    assertThrows[IOException](abs("/").parent)
    assertThrows[IOException](abs("/path/to/smth").up(4))
  }
}
