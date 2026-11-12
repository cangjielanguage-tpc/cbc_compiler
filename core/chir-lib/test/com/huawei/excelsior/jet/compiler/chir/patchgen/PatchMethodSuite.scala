package com.huawei.excelsior.jet.compiler.chir.patchgen

import com.huawei.excelsior.jet.compiler.chir.{CHIR, EditableCHIR}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PatchMethodSuite extends AnyFunSuite with Matchers {
  test("method selection rejects ambiguous candidates instead of choosing the first") {
    val pkg = EditableCHIR.empty("fixture")
    val first = pkg.newFunction("@patch", Seq.empty, CHIR.BuiltinType.Unit)
    val second = pkg.newFunction("@patch", Seq(CHIR.BuiltinType.Int64), CHIR.BuiltinType.Unit)
    PatchGen.uniquePatchMethod(Seq.empty) shouldBe None
    PatchGen.uniquePatchMethod(Seq(first)) shouldBe Some(first)
    val failure = intercept[PatchGenException](PatchGen.uniquePatchMethod(Seq(first, second)))
    failure.getMessage should include ("multiple patch methods")
  }
}
