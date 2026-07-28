/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.jprof.blame.MarkedRegions.Hotness
import com.huawei.excelsior.jet.compiler.types.Guards.PointGuard
import com.huawei.excelsior.jet.compiler.opt.middle.devirtualization.TauInfo
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeMethodReference
import com.huawei.excelsior.jet.compiler.opt.middle.inline.scales.Scales

import scala.collection.mutable

class WarmPGOAnalysisSuite extends CompilerSuite with GlobalNodesBuilder with CFGAnalysis with Scales {

  val temperatures = mutable.Map.empty[Position, Hotness] withDefaultValue Hotness.Unknown

  private var nextBCPos = 0
  private def nextPos() = {
    val p = rootMethodPos.copy(offset = nextBCPos)
    nextBCPos += 1
    p
  }
  override def nodeWeight(n: Node): Double = 0

  override def currentPhase = CompilerPhase.PreLowering

  override def beforeEach(): Unit = {
    super.beforeEach()

    temperatures.clear()
  }

  override def isPGOHost = true

  private def setHotness(markerPos: Position, hotness: Hotness) =
    temperatures.put(markerPos, hotness) ensuring (_.isEmpty)

  override def getHotness(n: Node) =
    temperatures(n.pos)

  override def parsableAttributes() = {
    def makeCall(h: Hotness) = {
      val pos = nextPos()
      setHotness(pos, h)
      withPos(pos) { Invoke(new FakeMethodReference())() }
    }

    Seq(
      new SimpleAttribute("coldcall")({ case Seq() =>
        makeCall(Hotness.Cold)
      }),

      new SimpleAttribute("hotcall")({ case Seq() =>
        makeCall(Hotness.Hot)
      }),

    ) ++ super.parsableAttributes()
  }

  def check(g: SubGraph, warmBlocks: Block*): Unit = {
    makeCFG(g)
    markWarmBlocks()
    // Sort for ease of failure investigation.
    (all[WarmCodeMarker] map (_.block)).toList.sortBy(_.id) shouldBe warmBlocks.toList.sortBy(_.id)
  }

  test("do forward and backward propagation from warm blocks 4 and 5") {
    check(0 -> 1 -> (2 || (3 -> (4@@"coldcall()" || 5@@"coldcall()") -> 6)) -> 7,
      3, 4, 5, 6
    )
  }

  test("no forward and backward propagation from warm block 5") {
    check(0 -> 1 -> (2 || (3 -> (4@@"hotcall()" || 5@@"coldcall()") -> 6)) -> 7,
      5
    )
  }

  test("forward propagation of hot path") {
    check(0 -> 1 -> (2 || (3@@"hotcall()" -> (4@@"coldcall()" || 5@@"coldcall()") -> 6)) -> 7,
    )
  }

  test("forward propagation of hot path: warm block 5 becomes hot; 4 remains cold") {
    check(0 -> 1 -> (2 || (3@@"hotcall()" -> (4@@"coldcode()" || 5@@"coldcall()") -> 6)) -> 7,
    )
  }

  test("propagation of warm property is impossible; " +
    "propagation of hot path is needless as there is a not warm path containing blocks without calls 3 -> 5 -> 6 -> 7 ") {
    check(0 -> 1 -> (2 || (3@@"hotcall()" -> (4@@"coldcall()" || 5) -> 6)) -> 7,
      4
    )
  }

  test("backward propagation of hot path") {
    check(0 -> 1 -> (2 || (3 -> (4@@"coldcall()" || 5@@"coldcall()") -> 6@@"hotcall()")) -> 7,
    )
  }

  test("backward propagation of hot path: warm block 5 becomes hot; 4 remains cold") {
    check(0 -> 1 -> (2 || (3 -> (4@@"coldcode()" || 5@@"coldcall()") -> 6@@"hotcall()")) -> 7,
    )
  }

  test("propagation of warm property is impossible; " +
    "propagation of hot path is needless as there is a not warm path containing blocks without calls 3 -> 4 -> 6 -> 7 ") {
    check(0 -> 1 -> (2 || (3 -> (4 || 5@@"coldcall()") -> 6@@"hotcall()")) -> 7,
      5
    )
  }

  test("hot path is found through a loop that doesn't contain hot calls but the loop contains warm bocks and blocks without calls" +
    "block 4 and block 5 become hot - they are found to be on a hot path" +
    "warm paths are marked inside a loop" +
    "this test is created from the sample PGO") {
    check(1 -> wd(2 -> 4@@"coldcall()" ->
      wd(6 -> 7 -> (8 ||
        (9@@"coldcall()") -> (10 || (11 -> (12 || 13)))) //cold code
        -> 14)
      ->5@@"coldcall()")
      ->3@@"coldcall()",
      9, 10, 11, 13, 12
    )
  }

  test("extended version of the previous test; there are more warm blocks inside a loop;" +
    "forward and backward propagation of the warm property is required") {
    check(1 -> 2 -> wd(3 -> 4@@"coldcall()" ->
      wd(5 -> (6@@"coldcall()" ||
        (7 -> ((8@@"coldcall()" -> (10@@"coldcall()" || (11 -> (12@@"coldcall()" || 13))))
          || (9 -> ((15 -> ((17@@"coldcall()" -> (19 || 20 ) -> 23) || (18@@"coldcall()") -> (21 || 22)) -> 24) || 16)))
          -> 14)))
    ) -> 25@@"coldcall()" ,
      8, 10, 11, 13, 12, 15, 17, 18, 19, 20 ,21, 22, 23, 24
    )
  }

  test("another version of the previous test; do-while loops instead of while-do") {
    check(1 -> 2 -> dw(3 -> 4@@"coldcall()" ->
      dw(5 -> (6@@"coldcall()" ||
        (7 -> ((8@@"coldcall()" -> (10@@"coldcall()" || (11 -> (12@@"coldcall()" || 13))))
          || (9 -> ((15 -> ((17@@"coldcall()" -> (19 || 20 ) -> 23) || (18@@"coldcall()") -> (21 || 22)) -> 24) || 16)))
          -> 14)))
    ) -> 25@@"coldcall()" ,
      8, 10, 11, 13, 12, 15, 17, 18, 19, 20 ,21, 22, 23, 24
    )
  }

  test("PGO.Test.justDoIt(Ljava/lang/StringBuffer;[Ljava/lang/String;)Ljava/lang/Object;") {
    check(seq(0 -> 3, 0 -> 2, 3 -> 4, 4 -> 1, 4 -> 7, 5 -> 4, 6 -> 5, 6 -> 9, 7 -> 6, 8 -> 6, 9 -> 11, 9 -> 14, 10 -> 8, 11 -> 12, 11 -> 10, 12 -> 15, 12 -> 13, 13 -> 8, 14 -> 8, 15 -> 8,
      1@@("coldcall()"), 2@@("coldcode()", "halt()"), 10@@("coldcall()"), 11@@("coldcall()"), 12@@("coldcall()"), 13@@("coldcall()")),
      10, 11, 12, 13, 15
    )
  }

  test("PGO_Krol.Test.justDoIt(Ljava/lang/StringBuffer;[Ljava/lang/String;)Ljava/lang/Object;") {
    check(seq(0 -> 3, 0 -> 2, 3 -> 4, 4 -> 1, 4 -> 7, 5 -> 4, 6 -> 5, 6 -> 10, 7 -> 6, 8 -> 6, 9 -> 22, 9 -> 14, 10 -> 19, 10 -> 9, 11 -> 8, 12 -> 11, 13 -> 25, 13 -> 15, 14 -> 16, 14 -> 13, 15 -> 12, 16 -> 24, 16 -> 17, 17 -> 11, 18 -> 8, 19 -> 20, 19 -> 18, 20 -> 23, 20 -> 21, 21 -> 8, 22 -> 8, 23 -> 8, 24 -> 11, 25 -> 12,
      1@@("coldcall()"), 2@@("coldcode()", "hotcall()", "halt()"), 5@@("hotcall()"), 7@@("hotcall()"), 13@@("coldcall()"), 16@@("coldcall()"), 18@@("coldcall()"), 19@@("coldcall()"), 20@@("coldcall()"), 21@@("coldcall()")),
      11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25
    )
  }

  test("CharsetEncoder.encode(Ljava/nio/CharBuffer;Ljava/nio/ByteBuffer;Z)Ljava/nio/charset/CoderResult;") {
    check(seq(0 -> 55, 0 -> 54, 1 -> 58, 1 -> 2, 2 -> 59, 2 -> 3, 3 -> 56, 3 -> 4, 4 -> 60, 4 -> 57, 5 -> 6, 6 -> 7, 7 -> 37, 7 -> 35, 8 -> 74, 8 -> 44, 9 -> 61, 9 -> 10, 10 -> 62, 10 -> 12, 11 -> 28, 12 -> 13, 13 -> 75, 13 -> 46, 14 -> 76, 14 -> 48, 15 -> 66, 15 -> 16, 17 -> 18, 17 -> 72, 18 -> 67, 18 -> 19, 19 -> 20, 19 -> 27, 20 -> 21, 21 -> 68, 21 -> 22, 22 -> 23, 22 -> 69, 23 -> 70, 23 -> 24, 25 -> 26, 26 -> 7, 27 -> 28, 29 -> 30, 30 -> 31, 30 -> 32, 32 -> 33, 32 -> 34, 35 -> 36, 36 -> 73, 36 -> 42, 37 -> 77, 37 -> 50, 38 -> 79, 38 -> 52, 39 -> 41, 40 -> 41, 41 -> 36, 42 -> 43, 43 -> 8, 43 -> 71, 44 -> 45, 45 -> 63, 45 -> 9, 46 -> 47, 47 -> 14, 47 -> 64, 48 -> 49, 49 -> 15, 49 -> 65, 50 -> 78, 50 -> 38, 51 -> 39, 52 -> 80, 52 -> 40, 53 -> 39, 54 -> 1, 55 -> 1, 56 -> 5, 57 -> 5, 58 -> 6, 59 -> 6, 60 -> 6, 61 -> 11, 62 -> 11, 63 -> 13, 64 -> 17, 65 -> 17, 66 -> 17, 67 -> 21, 68 -> 25, 69 -> 25, 70 -> 26, 71 -> 28, 72 -> 28, 73 -> 43, 74 -> 45, 75 -> 47, 76 -> 49, 77 -> 51, 78 -> 51, 79 -> 53, 80 -> 53,
      5@@("coldcall()"), 12@@("coldcall()"), 16@@("coldcode()", "coldcall()", "halt()"), 20@@("coldcall()"), 24@@("coldcode()", "coldcall()", "halt()"), 25@@("coldcall()"), 29@@("coldcode()"), 31@@("coldcode()", "coldcall()", "halt()"), 33@@("coldcode()", "coldcall()", "halt()"), 34@@("coldcode()", "halt()"), 35@@("coldcode()", "hotcall()"), 39@@("coldcall()"), 40@@("hotcall()"), 42@@("coldcode()", "coldcall()"), 44@@("coldcode()", "coldcall()"), 46@@("coldcode()", "coldcall()"), 48@@("coldcode()", "coldcall()")),
      5, 12, 20, 25, 39, 51, 53, 56, 57, 68, 69, 77, 78, 79, 80
    )
  }


  test("Tuple2.equals(Ljava/lang/Object;)Z") {
    check(seq(0 -> 1, 0 -> 105, 1 -> 2, 1 -> 106, 2 -> 114, 2 -> 18, 3 -> 96, 3 -> 4, 4 -> 5, 4 -> 22, 5 -> 6, 5 -> 7, 6 -> 99, 6 -> 109, 7 -> 98, 7 -> 108, 8 -> 116, 8 -> 27, 9 -> 101, 9 -> 10, 10 -> 11, 10 -> 15, 11 -> 12, 11 -> 14, 12 -> 32, 12 -> 31, 13 -> 104, 13 -> 113, 14 -> 103, 14 -> 111, 15 -> 102, 15 -> 110, 16 -> 17, 18 -> 19, 19 -> 115, 19 -> 20, 20 -> 21, 21 -> 95, 21 -> 3, 22 -> 23, 22 -> 72, 23 -> 24, 23 -> 25, 24 -> 26, 25 -> 26, 26 -> 97, 26 -> 107, 27 -> 28, 28 -> 117, 28 -> 29, 29 -> 30, 30 -> 100, 30 -> 9, 31 -> 33, 32 -> 34, 32 -> 128, 33 -> 13, 33 -> 112, 34 -> 118, 34 -> 35, 35 -> 119, 35 -> 37, 36 -> 64, 37 -> 120, 37 -> 38, 38 -> 121, 38 -> 40, 40 -> 41, 40 -> 63, 41 -> 122, 41 -> 42, 42 -> 43, 42 -> 62, 43 -> 123, 43 -> 61, 44 -> 45, 44 -> 60, 45 -> 124, 45 -> 46, 46 -> 47, 46 -> 59, 47 -> 125, 47 -> 58, 48 -> 49, 48 -> 57, 49 -> 50, 49 -> 56, 50 -> 126, 50 -> 51, 51 -> 127, 51 -> 53, 53 -> 54, 53 -> 55, 54 -> 64, 64 -> 33, 65 -> 66, 65 -> 67, 65 -> 68, 65 -> 69, 65 -> 70, 66 -> 71, 67 -> 91, 67 -> 89, 68 -> 71, 69 -> 71, 70 -> 71, 71 -> 26, 72 -> 73, 72 -> 134, 73 -> 74, 73 -> 135, 74 -> 75, 74 -> 136, 75 -> 76, 75 -> 137, 76 -> 131, 76 -> 77, 77 -> 132, 77 -> 133, 78 -> 79, 79 -> 80, 80 -> 81, 80 -> 141, 81 -> 82, 81 -> 142, 82 -> 83, 82 -> 143, 83 -> 84, 83 -> 144, 84 -> 138, 84 -> 85, 85 -> 139, 85 -> 140, 86 -> 87, 87 -> 88, 88 -> 130, 88 -> 129, 89 -> 90, 90 -> 94, 90 -> 92, 91 -> 90, 92 -> 93, 93 -> 71, 94 -> 93, 95 -> 8, 96 -> 8, 97 -> 8, 98 -> 8, 99 -> 8, 100 -> 16, 101 -> 16, 102 -> 16, 103 -> 16, 104 -> 16, 105 -> 17, 106 -> 17, 107 -> 17, 108 -> 17, 109 -> 17, 110 -> 17, 111 -> 17, 112 -> 17, 113 -> 17, 114 -> 19, 115 -> 21, 116 -> 28, 117 -> 30, 118 -> 36, 119 -> 36, 120 -> 39, 121 -> 39, 122 -> 44, 123 -> 44, 124 -> 48, 125 -> 48, 126 -> 52, 127 -> 52, 128 -> 64, 129 -> 65, 130 -> 65, 131 -> 78, 132 -> 78, 133 -> 79, 134 -> 80, 135 -> 80, 136 -> 80, 137 -> 80, 138 -> 86, 139 -> 86, 140 -> 87, 141 -> 88, 142 -> 88, 143 -> 88, 144 -> 88,
      6@@("coldcall()"), 7@@("coldcall()"), 13@@("coldcall()"), 14@@("coldcall()"), 15@@("coldcall()"), 18@@("coldcode()", "coldcall()"), 20@@("coldcode()", "coldcall()"), 24@@("coldcall()"), 25@@("coldcall()"), 27@@("coldcode()", "coldcall()"), 29@@("coldcode()", "coldcall()"), 31@@("coldcode()", "hotcall()"), 39@@("coldcode()", "halt()"), 52@@("coldcode()", "halt()"), 55@@("coldcode()", "halt()"), 56@@("coldcode()", "halt()"), 57@@("coldcode()", "halt()"), 58@@("coldcode()", "halt()"), 59@@("coldcode()", "halt()"), 60@@("coldcode()", "halt()"), 61@@("coldcode()", "halt()"), 62@@("coldcode()", "halt()"), 63@@("coldcode()", "halt()"), 66@@("coldcall()"), 68@@("coldcall()"), 69@@("coldcall()"), 70@@("coldcall()"), 89@@("coldcode()", "hotcall()"), 92@@("coldcode()", "hotcall()")),
      5, 6, 7, 13, 14, 15, 23, 24, 25, 66, 68, 69, 70, 98, 99, 102, 103, 104, 108, 109, 110, 111, 113
    )
  }

  test("grinder.org/kxml/parser/XmlParser.readText(Ljava/lang/StringBuffer;C)I") {
    check(seq(0 -> 1, 1 -> 35, 1 -> 33, 2 -> 77, 2 -> 3, 3 -> 70, 3 -> 4, 4 -> 78, 4 -> 5, 5 -> 71, 5 -> 79, 6 -> 52, 6 -> 46, 7 -> 72, 7 -> 73, 8 -> 63, 9 -> 68, 9 -> 67, 10 -> 11, 10 -> 23, 11 -> 12, 11 -> 21, 12 -> 13, 12 -> 20, 13 -> 14, 13 -> 19, 14 -> 15, 14 -> 18, 15 -> 16, 15 -> 17, 16 -> 22, 17 -> 22, 18 -> 22, 19 -> 22, 20 -> 22, 21 -> 22, 22 -> 29, 23 -> 24, 23 -> 30, 24 -> 25, 24 -> 26, 25 -> 27, 26 -> 27, 27 -> 74, 27 -> 75, 28 -> 29, 29 -> 1, 33 -> 34, 34 -> 76, 34 -> 2, 35 -> 36, 35 -> 82, 36 -> 81, 36 -> 37, 37 -> 38, 37 -> 40, 38 -> 80, 38 -> 39, 39 -> 45, 40 -> 41, 40 -> 44, 41 -> 42, 42 -> 43, 43 -> 45, 44 -> 45, 45 -> 34, 46 -> 47, 47 -> 7, 47 -> 9, 48 -> 49, 49 -> 47, 50 -> 51, 52 -> 53, 52 -> 86, 53 -> 85, 53 -> 54, 54 -> 55, 54 -> 57, 55 -> 84, 55 -> 56, 56 -> 62, 57 -> 58, 57 -> 61, 58 -> 59, 59 -> 60, 60 -> 62, 61 -> 62, 62 -> 83, 62 -> 48, 63 -> 89, 63 -> 64, 64 -> 88, 64 -> 87, 65 -> 66, 66 -> 29, 67 -> 69, 68 -> 69, 69 -> 10, 69 -> 31, 70 -> 6, 71 -> 6, 72 -> 8, 73 -> 8, 74 -> 28, 75 -> 28, 76 -> 32, 77 -> 32, 78 -> 32, 79 -> 32, 80 -> 42, 81 -> 43, 82 -> 45, 83 -> 49, 84 -> 59, 85 -> 60, 86 -> 62, 87 -> 65, 88 -> 65, 89 -> 66,
      1@@("hotcall()"), 6@@("hotcall()"), 8@@("hotcall()"), 11@@("coldcall()"), 12@@("coldcall()"), 13@@("coldcall()"), 14@@("coldcall()"), 15@@("coldcall()"), 16@@("coldcall()"), 17@@("coldcall()"), 18@@("coldcall()"), 19@@("coldcall()"), 20@@("coldcall()"), 21@@("coldcall()"), 25@@("coldcall()"), 26@@("coldcall()"), 28@@("coldcall()"), 30@@("coldcode()", "halt()"), 31@@("coldcode()", "halt()"), 33@@("coldcode()", "hotcall()"), 34@@("hotcall()"), 38@@("coldcall()"), 40@@("coldcall()"), 45@@("hotcall()"), 46@@("coldcode()", "hotcall()"), 47@@("hotcall()"), 49@@("hotcall()"), 50@@("coldcode()"), 51@@("coldcode()", "halt()"), 55@@("coldcall()"), 57@@("coldcall()"), 64@@("coldcall()"), 65@@("coldcall()"), 66@@("hotcall()"), 67@@("coldcode()"), 69@@("coldcall()")),
      9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 37, 38, 39, 40, 41, 42, 44, 54, 55, 56, 57, 58, 59, 61, 64, 65, 68, 69, 74, 75, 80, 84, 87, 88
    )
  }

  test("hp/kXML/KxmlBenchUtil.saxTraverseGatherStats(Lorg/kxml/parser/XmlParser;)V") {
    check(seq(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 3 -> 21, 4 -> 8, 5 -> 6, 6 -> 7, 6 -> 20, 7 -> 8, 8 -> 3, 8 -> 9, 8 -> 10, 8 -> 11, 8 -> 12, 8 -> 13, 8 -> 14, 8 -> 17, 8 -> 18, 9 -> 19, 10 -> 19, 11 -> 19, 12 -> 19, 13 -> 19, 14 -> 16, 14 -> 15, 15 -> 16, 16 -> 19, 17 -> 19, 18 -> 19, 19 -> 2,
      4 @@ ("coldcall()"), 5 @@ ("coldcode()"), 8 @@ ("coldcall()"), 14 @@ ("coldcall()"), 16 @@ ("coldcall()"), 20 @@ ("coldcode()", "halt()")),
      14, 15, 16
    )
  }

  test("sun/mep/bench/Chess/GameState.archive(Ljava/lang/StringBuffer;)V") {
    check(seq(0 -> 1, 1 -> 2, 2 -> 3, 2 -> 10, 3 -> 5, 3 -> 4, 4 -> 5, 5 -> 6, 5 -> 9,  6 -> 7, 6 -> 8, 7 -> 9, 9 -> 2,
      4@@("coldcall()"), 8@@("coldcode()", "halt()"), 9@@("coldcall()"), 10@@("coldcall()")),
      4
    )
  }


  test("grinder.org/kxml/parser/XmlParser.readName()Ljava/lang/String;") {
    check(seq(0 -> 43, 0 -> 22, 1 -> 73, 1 -> 2, 2 -> 74, 2 -> 3, 3 -> 70, 3 -> 4, 4 -> 75, 4 -> 71, 5 -> 92, 5 -> 6, 6 -> 76, 6 -> 93, 7 -> 107, 7 -> 69, 8 -> 27, 8 -> 91, 9 -> 82, 9 -> 10, 10 -> 83, 10 -> 11, 11 -> 84, 11 -> 12, 12 -> 85, 12 -> 13, 13 -> 77, 13 -> 14, 14 -> 86, 14 -> 78, 15 -> 79, 15 -> 16, 16 -> 87, 16 -> 80, 17 -> 89, 17 -> 18, 18 -> 88, 18 -> 90, 19 -> 54, 19 -> 39, 22 -> 23, 23 -> 72, 23 -> 1, 24 -> 25, 25 -> 23, 26 -> 28, 27 -> 29, 27 -> 26, 28 -> 81, 28 -> 9, 29 -> 96, 29 -> 30, 30 -> 31, 30 -> 33, 31 -> 95, 31 -> 32, 32 -> 38, 33 -> 34, 33 -> 37, 34 -> 35, 35 -> 36, 36 -> 38, 37 -> 38, 38 -> 28, 39 -> 40, 40 -> 106, 40 -> 65, 41 -> 42, 42 -> 40, 43 -> 44, 43 -> 100, 44 -> 99, 44 -> 45, 45 -> 46, 45 -> 48, 46 -> 98, 46 -> 47, 47 -> 53, 48 -> 49, 48 -> 52, 49 -> 50, 50 -> 51, 51 -> 53, 52 -> 53, 53 -> 94, 53 -> 24, 54 -> 55, 54 -> 103, 55 -> 102, 55 -> 56, 56 -> 57, 56 -> 59, 57 -> 101, 57 -> 58, 58 -> 64, 59 -> 60, 59 -> 63, 60 -> 61, 61 -> 62, 62 -> 64, 63 -> 64, 64 -> 97, 64 -> 41, 65 -> 105, 65 -> 104, 66 -> 67, 67 -> 8, 68 -> 8, 69 -> 68, 70 -> 5, 71 -> 5, 72 -> 7, 73 -> 7, 74 -> 7, 75 -> 7, 76 -> 7, 77 -> 15, 78 -> 15, 79 -> 17, 80 -> 17, 81 -> 19, 82 -> 19, 83 -> 19, 84 -> 19, 85 -> 19, 86 -> 19, 87 -> 19, 88 -> 19, 89 -> 20, 90 -> 20, 91 -> 20, 92 -> 21, 93 -> 21, 94 -> 25, 95 -> 35, 96 -> 36, 97 -> 42, 98 -> 50, 99 -> 51, 100 -> 53, 101 -> 61, 102 -> 62, 103 -> 64, 104 -> 66, 105 -> 66, 106 -> 67, 107 -> 68,
      0@@("hotcall()"), 19@@("hotcall()"), 21@@("coldcode()", "halt()"), 22@@("coldcode()", "hotcall()"), 23@@("hotcall()"), 25@@("hotcall()"), 26@@("coldcode()", "hotcall()"), 27@@("hotcall()"), 28@@("hotcall()"), 31@@("coldcall()"), 33@@("coldcall()"), 38@@("hotcall()"), 39@@("coldcode()", "hotcall()"), 40@@("hotcall()"), 42@@("hotcall()"), 46@@("coldcall()"), 48@@("coldcall()"), 57@@("coldcall()"), 59@@("coldcall()"), 65@@("coldcall()"), 66@@("coldcall()"), 67@@("hotcall()"), 68@@("hotcall()"), 69@@("coldcode()")),
      30, 31, 32, 33, 34, 35, 37, 45, 46, 47, 48, 49, 50, 52, 56, 57, 58, 59, 60, 61, 63, 65, 66, 95, 98, 101, 104, 105
    )
  }

  test("Test.searchForConcreteKey(Ljava/lang/Integer;Ljava/util/HashMap;I)J") {
    check(seq(0 -> 6, 0 -> 1, 1 -> 38, 1 -> 5, 2 -> 3, 2 -> 40, 3 -> 4, 6 -> 7, 7 -> 39, 7 -> 10, 8 -> 7, 9 -> 8, 10 -> 19, 10 -> 9, 11 -> 8, 12 -> 36, 12 -> 41, 13 -> 12, 14 -> 42, 14 -> 23, 15 -> 45, 15 -> 14, 16 -> 59, 16 -> 15, 17 -> 58, 17 -> 16, 18 -> 57, 18 -> 17, 19 -> 20, 19 -> 44, 20 -> 18, 21 -> 43, 21 -> 47, 22 -> 48, 22 -> 21, 23 -> 46, 23 -> 22, 24 -> 12, 25 -> 51, 25 -> 24, 26 -> 60, 26 -> 25, 27 -> 26, 28 -> 12, 29 -> 49, 29 -> 32, 30 -> 53, 30 -> 29, 31 -> 52, 31 -> 61, 32 -> 54, 32 -> 35, 33 -> 31, 34 -> 50, 34 -> 56, 35 -> 55, 35 -> 34, 36 -> 11, 37 -> 11, 38 -> 2, 39 -> 2, 40 -> 4, 41 -> 11, 42 -> 13, 43 -> 13, 44 -> 18, 45 -> 26, 46 -> 26, 47 -> 27, 48 -> 27, 49 -> 28, 50 -> 28, 51 -> 30, 52 -> 30, 53 -> 31, 54 -> 31, 55 -> 33, 56 -> 33, 57 -> 37, 58 -> 37, 59 -> 37, 60 -> 37, 61 -> 37,
      0@@("coldcall()"), 2@@("coldcall()"), 5@@("coldcode()", "halt()"), 9@@("coldcode()", "hotcall()"), 24@@("coldcall()")),
      24
    )
  }

  test("cold return (JET-12268)") {
    check(0 -> wd(1 -> 2) -> 3@@"coldcode()")
  }

  test("arraycopy_krol.Test.benchmark([I)V") {
    check(seq(0 -> 1, 1 -> 2, 1 -> 3, 3 -> 4, 4 -> 11, 4 -> 5, 5 -> 6, 5 -> 12, 6 -> 7, 6 -> 8, 7 -> 9, 8 -> 9, 9 -> 10, 10 -> 4, 11 -> 1, 12 -> 10,
      7@@("coldcall()"), 8@@("coldcall()")),
      6, 7, 8, 9
    )
  }
  test("arraycopy_krol.Test.execute([II)V") {
    check(seq(0 -> 1, 1 -> 2, 1 -> 3, 3 -> 8, 3 -> 4, 4 -> 9, 4 -> 6, 5 -> 7, 6 -> 7, 7 -> 1, 8 -> 5, 9 -> 5,
      5@@("hotcall()"), 6@@("coldcall()")),
      6
    )
  }


  test("backup path 0%") {
    testWeightedBackupPath(0, treatAsCold = true)
  }

  test("backup path 10%") {
    testWeightedBackupPath(10, treatAsCold = true)
  }

  test("backup path 25%") {
    testWeightedBackupPath(25, treatAsCold = false)
  }

  test("backup path 100%") {
    testWeightedBackupPath(100, treatAsCold = false)
  }

  def testWeightedBackupPath(fastPathHitsPercent: Int, treatAsCold: Boolean): Unit = {
    makeCFG(0 -> 1 -> (2 || 3) -> 4)
    makeNodes { at =>
      at(1)
      b(1).blockEnd.asInstanceOf[If].selector = TypeTest(PointGuard(tA), TauInfo.PGO(100 - fastPathHitsPercent, fastPathHitsPercent))(Null())
    }

    if (treatAsCold) {
      findColdBlocks() shouldBe Set(b(3))
    } else {
      findColdBlocks() shouldBe Set()
    }
  }

  test("block with two preds: cold exit from TauTest and non-cold block") {
    makeCFG(0 -> 1 -> (2 || 3) -> 4 -> 3)
    makeNodes { at =>
      at(1)
      b(1).blockEnd.asInstanceOf[If].selector = TypeTest(PointGuard(tA), TauInfo.PGO(100, 0))(Null())
    }

    findColdBlocks() shouldBe Set()
  }


}
