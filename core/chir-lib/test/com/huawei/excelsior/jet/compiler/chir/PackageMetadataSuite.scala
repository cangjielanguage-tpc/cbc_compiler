package com.huawei.excelsior.jet.compiler.chir

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class PackageMetadataSuite extends AnyFunSuite with Matchers {
  for (version <- TestData.versions; (phase, name) <- TestData.phases) {
    test(s"$version editing and saving preserves input phase $name") {
      val pkg = TestData.phaseFixture(version, phase)
      pkg.function(1).moveBody(pkg.function(8))
      TestData.storedPhase(pkg.toBytes) shouldBe phase
      TestData.storedPhase(pkg.toBytes) shouldBe phase
    }
  }
}
