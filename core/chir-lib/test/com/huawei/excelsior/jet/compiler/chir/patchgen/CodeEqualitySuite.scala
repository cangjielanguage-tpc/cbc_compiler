package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.{CHIR, EditableCHIR}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class CodeEqualitySuite extends AnyFunSuite with Matchers {
  private def body(expression: CHIR.Expression): CHIR.BlockGroup = new CHIR.BlockGroup {
    val entryBlock: CHIR.Block = new CHIR.Block {
      def nonTerminatorExpressions: Seq[CHIR.Expression] = Seq(expression)
      val terminator: CHIR.Terminator = new CHIR.Exit {
        def debugLoc: Option[CHIR.DebugLocation] = None
      }
      def expressions: Seq[CHIR.Expression] = nonTerminatorExpressions :+ terminator
      def isLandingPadBlock: Boolean = false
    }
    def blocks: Seq[CHIR.Block] = Seq(entryBlock)
  }

  for (dynamic <- Seq(false, true)) {
    test(s"static call equality does not read a receiver: dynamic=$dynamic") {
      val pkg = EditableCHIR.empty("fixture")
      val function = pkg.newFunction("@static", Seq.empty, CHIR.BuiltinType.Unit)
      function.replaceAttributes(Seq(CHIR.Attribute.Static))
      def call(): CHIR.Expression = if (dynamic) {
        new CHIR.Invoke {
          def callee: CHIR.FuncSig = function
          def thisType: CHIR.Type = CHIR.BuiltinType.This
          def thisArg: CHIR.Value = fail("static call has no receiver")
          def args: Seq[CHIR.Value] = Seq.empty
          def instantiatedTypeArgs: Seq[CHIR.Type] = Seq.empty
          def overflowStrategy: CHIR.OverflowStrategy = CHIR.OverflowStrategy.Na
          def resultTpe: CHIR.Type = CHIR.BuiltinType.Unit
          def resultVar: CHIR.LocalVar = fail("unexpected result query")
          def debugLoc: Option[CHIR.DebugLocation] = None
        }
      } else {
        new CHIR.Apply {
          def callee: CHIR.Value = function
          def thisType: Option[CHIR.Type] = Some(CHIR.BuiltinType.This)
          def thisArg: CHIR.Value = fail("static call has no receiver")
          def args: Seq[CHIR.Value] = Seq.empty
          def instantiatedTypeArgs: Seq[CHIR.Type] = Seq.empty
          def resultTpe: CHIR.Type = CHIR.BuiltinType.Unit
          def resultVar: CHIR.LocalVar = fail("unexpected result query")
          def debugLoc: Option[CHIR.DebugLocation] = None
        }
      }
      Equality.sameCode(body(call()), body(call())) shouldBe true
    }
  }
}
