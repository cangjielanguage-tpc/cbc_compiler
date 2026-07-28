/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.FakeType
import org.scalatest.matchers.{MatchResult, Matcher}

class SignatureSuite extends CompilerSuite {

  def resolve(name: String): Type = {
    val tkind = if (name.head == 'R') TypeKind.RECORD else TypeKind.CLASS
    FakeType(name, tkind).markAsCangjieType()
  }

  for (((x, y, shouldEqual), pos) <- Seq(
    tp(Primitive(INT), Primitive(INT), true),
    tp(Primitive(INT), Primitive(LONG), false),

    tp(JavaArray(Primitive(INT), 1), JavaArray(Primitive(INT), 1), true),
    tp(JavaArray(Primitive(INT), 1), JavaArray(Primitive(INT), 2), false),
    tp(JavaArray(Primitive(INT), 1), JavaArray(Primitive(LONG), 1), false),

    tp(Record("Roo"), Record("Rar"), false),
    tp(Record("Roo"), Record(asClassType(resolve("Roo"))), true),
    tp(Record(asClassType(resolve("Roo"))), Record(asClassType(resolve("Roo"))), true),

    tp(NullableWrapper(CangjieReference("foo")), NullableWrapper(CangjieReference("bar")), false),
    tp(NullableWrapper(CangjieReference("foo")), CangjieReference("foo"), false),
    tp(NullableWrapper(CangjieReference("foo")), NullableWrapper(CangjieReference("foo")), true),
    tp(NullableWrapper(CangjieReference("foo")), NullableWrapper(CangjieReference(asClassType(resolve("foo")))), true),
  ) ++ {
    val x = asClassType(resolve("foo"))
    val y = asClassType(resolve("foo"))
    Seq(
      tp(NullableWrapper(CangjieReference(x)), NullableWrapper(CangjieReference(x)), true),
      tp(NullableWrapper(CangjieReference(x)), CangjieReference(x), false),
      tp(NullableWrapper(CangjieReference(x)), NullableWrapper(CangjieReference(y)), true), // symtype is compared by name!
    )
  } ++ {
    Set(
      Primitive(INT),
      JavaArray(Primitive(INT), 1),
      CangjieReference("foo"),
      Record("Roo")
    ) subsets 2 map (_.toSeq) map { case Seq(x, y) =>
      tp(x, y, false)
    }
  }) {
    test(s"$x equals $y at ${pos.lineNumber}") {
      if (shouldEqual) {
        x shouldBe y
        y shouldBe x
        x.## shouldBe y.##
      } else {
        x should not be y
        y should not be x
      }
    }
  }

  for (((x, y, inst, shouldEqual), pos) <- Seq(
    tp(Primitive(INT), Primitive(INT), Seq(Void), true),
    tp(Primitive(INT), Primitive(LONG), Seq(), false),

    tp(LocalTypeVariable(0), LocalTypeVariable(0), Seq(Int32, Int32), true),
    tp(LocalTypeVariable(0), LocalTypeVariable(1), Seq(Int32, Int32), true),
    tp(LocalTypeVariable(0), LocalTypeVariable(1), Seq(Int32, Int64), false),

  ) ++ {
    val classI32 = InstantiatedReference("foo", Seq(Int32))
    val classT1 = InstantiatedReference("foo", Seq(LocalTypeVariable(1)))
    val recordI32 = InstantiatedRecord("foo", Seq(Int32))
    val recordT1 = InstantiatedRecord("foo", Seq(LocalTypeVariable(1)))
    Seq(
      tp(LocalTypeVariable(1), classI32, Seq(Void, classI32), true),
      tp(LocalTypeVariable(1), classI32, Seq(Void, NullableWrapper(classI32)), false),
      tp(LocalTypeVariable(1), NullableWrapper(classI32), Seq(Void, NullableWrapper(classI32)), true),

      tp(LocalTypeVariable(0), classT1, Seq(classT1, Int32), true),
      tp(LocalTypeVariable(0), classT1, Seq(NullableWrapper(classT1), Int32), false),
      tp(LocalTypeVariable(0), NullableWrapper(classT1), Seq(NullableWrapper(classT1), Int32), true),

      tp(classT1, classI32, Seq(classT1, Int32), true),
      tp(NullableWrapper(classT1), classI32, Seq(classT1, Int32), false),
      tp(NullableWrapper(classT1), NullableWrapper(classI32), Seq(classT1, Int32), true),

      tp(LocalTypeVariable(1), recordI32, Seq(Void, recordI32), true),
      tp(LocalTypeVariable(0), recordT1, Seq(recordT1, Int32), true),
      tp(recordT1, recordI32, Seq(recordT1, Int32), true),

      tp(ArraySlice(LocalTypeVariable(1)), ArraySlice(recordI32), Seq(Void, recordI32), true),
      tp(ArraySlice(LocalTypeVariable(0)), ArraySlice(recordT1), Seq(recordT1, Int32), true),
      tp(ArraySlice(recordT1), ArraySlice(recordI32), Seq(recordT1, Int32), true),

      tp(CangjieArray(LocalTypeVariable(1)), CangjieArray(recordI32), Seq(Void, recordI32), true),
      tp(CangjieArray(LocalTypeVariable(0)), CangjieArray(recordT1), Seq(recordT1, Int32), true),
      tp(CangjieArray(recordT1), CangjieArray(recordI32), Seq(recordT1, Int32), true),
    )
  }) {
    test(s"$x equals $y with instantiations [${inst.mkString(",")}]") {
      if (shouldEqual) {
        x should beEqualInstantiated (inst, y)
        y should beEqualInstantiated (inst, x)
      } else {
        x shouldNot beEqualInstantiated (inst, y)
        y shouldNot beEqualInstantiated (inst, x)
      }
    }
  }

  for (((x, y, inst, shouldEqual), pos) <- Seq(
    tp(MethodSignature()(LocalTypeVariable(0)), MethodSignature()(LocalTypeVariable(0)), Seq(Int32, Int32), true),
    tp(MethodSignature()(LocalTypeVariable(0)), MethodSignature()(LocalTypeVariable(1)), Seq(Int32, Int32), true),
    tp(MethodSignature()(LocalTypeVariable(0)), MethodSignature()(LocalTypeVariable(1)), Seq(Int32, Int64), false),
    tp(MethodSignature(LocalTypeVariable(0))(Void), MethodSignature(LocalTypeVariable(0))(Void), Seq(Int32, Int32), true),
    tp(MethodSignature(LocalTypeVariable(0))(Void), MethodSignature(LocalTypeVariable(1))(Void), Seq(Int32, Int32), true),
    tp(MethodSignature(LocalTypeVariable(0))(Void), MethodSignature(LocalTypeVariable(1))(Void), Seq(Int32, Int64), false),
  )) {
    test(s"$x equals $y with instantiations [${inst.mkString(",")}]") {
      if (shouldEqual) {
        x should beEqualInstantiated (inst, y)
        y should beEqualInstantiated (inst, x)
      } else {
        x shouldNot beEqualInstantiated (inst, y)
        y shouldNot beEqualInstantiated (inst, x)
      }
    }
  }

  def beEqualInstantiated(instantiatedTypes: Seq[SignatureType], expected: SignatureType) = new Matcher[SignatureType] {
    def apply(actual: SignatureType) = {
      MatchResult(SignatureType.equalInstantiatedLegacy(instantiatedTypes)(actual, expected),
        "{0} was not equal to {1} with {2}", "{0} was equal to {1} with {2}",
        Vector(actual, expected, instantiatedTypes.mkString("[", ",", "]")))
    }
  }

  def beEqualInstantiated(instantiatedTypes: Seq[SignatureType], expected: MethodSignature) = new Matcher[MethodSignature] {
    def apply(actual: MethodSignature) = {
      MatchResult(MethodSignature.equalInstantiatedLegacy(instantiatedTypes)(actual, expected),
        "{0} was not equal to {1} with {2}", "{0} was equal to {1} with {2}",
        Vector(actual, expected, instantiatedTypes.mkString("[", ",", "]")))
    }
  }

  for (((str, sig), pos) <- Seq(
    tp("V", Some(Void)),
    tp("U", Some(Unit)),
    tp("N", Some(Nothing)),
    tp("b", Some(Boolean)),

    tp("ia",  Some(AddrInt)),
    tp("ua",  Some(AddrUInt)),
    tp("i8",  Some(Int8)),
    tp("u8",  Some(UInt8)),
    tp("i16", Some(Int16)),
    tp("u16", Some(UInt16)),
    tp("i32", Some(Int32)),
    tp("u32", Some(UInt32)),
    tp("i64", Some(Int64)),
    tp("u64", Some(UInt64)),

    tp("c32", Some(UnicodeChar32)),

    tp("f16", Some(Float16)),
    tp("f32", Some(Float32)),
    tp("f64", Some(Float64)),

    tp("BS", Some(BString)),

    tp("Pi32", Some(CPointer(Int32))),
    tp("P()V", Some(CPointer(MethodSignature()(Void)))),

    tp("Sfoo;", Some(Record("foo"))),
    tp("Sfoo bar;", Some(Record("foo bar"))), // weird case with space in name

    tp("ISfoo;<i32>", Some(InstantiatedRecord("foo", Seq(Int32)))),
    tp("ISfoo;<TL0_TC1>", Some(InstantiatedRecord("foo", Seq(LocalTypeVariable(0), ClassTypeVariable(1))))),
    tp("ISfoo;<i32_ISbar;<i32>>", Some(InstantiatedRecord("foo", Seq(Int32, InstantiatedRecord("bar", Seq(Int32)))))),

    tp("Rfoo;", Some(CangjieReference("foo"))),
    tp("?Rfoo;", Some(NullableWrapper(CangjieReference("foo")))),
    tp("Rfoo bar;", Some(CangjieReference("foo bar"))), // weird case with space in name
    tp("?Rfoo bar;", Some(NullableWrapper(CangjieReference("foo bar")))), // weird case with space in name

    tp("IRfoo;<i32>", Some(InstantiatedReference("foo", Seq(Int32)))),
    tp("?IRfoo;<i32>", Some(NullableWrapper(InstantiatedReference("foo", Seq(Int32))))),
    tp("IRfoo;<TL0_TC1>", Some(InstantiatedReference("foo", Seq(LocalTypeVariable(0), ClassTypeVariable(1))))),
    tp("?IRfoo;<TL0_TC1>", Some(NullableWrapper(InstantiatedReference("foo", Seq(LocalTypeVariable(0), ClassTypeVariable(1)))))),
    tp("IRfoo;<i32_ISbar;<i32>>", Some(InstantiatedReference("foo", Seq(Int32, InstantiatedRecord("bar", Seq(Int32)))))),
    tp("?IRfoo;<i32_ISbar;<i32>>", Some(NullableWrapper(InstantiatedReference("foo", Seq(Int32, InstantiatedRecord("bar", Seq(Int32))))))),

    tp("ASi32", Some(ArraySlice(Int32))),

    tp("ARi32", Some(CangjieArray(Int32))),
    tp("?ARi32", Some(NullableWrapper(CangjieArray(Int32)))),

    tp("AJ0i32", Some(JavaArray(Int32, 0))), // weird case with zero dimensions
    tp("AJ3i32", Some(JavaArray(Int32, 3))),
    tp("!AJ3i32", Some(NonNullableWrapper(JavaArray(Int32, 3)))),

    tp("EWi32MyEnum;", Some(CangjieEnumWrapper(Int32, "MyEnum"))),
    tp("EWVMyEnum;", Some(CangjieEnumWrapper(Void, "MyEnum"))), // weird case with Void base type
    tp("EWRfoo;MyEnum;", Some(CangjieEnumWrapper(CangjieReference("foo"), "MyEnum"))),
    tp("EW?Rfoo;MyEnum;", Some(CangjieEnumWrapper(NullableWrapper(CangjieReference("foo")), "MyEnum"))),

    tp("AV2i8", Some(VArray(Int8, 2))),
    tp("AV0i32", Some(VArray(Int32, 0))),
    tp("AV13AV5Sfoo;", Some(VArray(VArray(Record("foo"), 5), 13))),
    tp(s"AV${Long.MaxValue}V", Some(VArray(Void, Long.MaxValue))),

    tp("()V", Some(MethodSignature()(Void))),
    tp("(i32_i64)V", Some(MethodSignature(Int32, Int64)(Void))),
    tp("(i32_i64)P(Sfoo;)P(i32)i32", Some(
      MethodSignature(
        Int32, Int64
      )(
        CPointer(
          MethodSignature(
            Record("foo")
          )(
            CPointer(
              MethodSignature(Int32)(Int32)
            )
          )
        )
      )
    )),

    // Negative cases
    tp("", None),
    tp("AR", None),
    tp("VV", None),
    tp("(()V", None),
    tp("())V", None),
    tp("!i32", None),
    tp("!!ARi32", None), // multiple non-nullable prefixes are not allowed
    tp("i33", None),
    tp("EWSfoo;MyEnum;", None), // only primitive and reference types are allowed as enum base type
    tp("AJ-2i32", None), // negative dimensions are prohibited
    tp(s"AJ${Long.MaxValue}", None),
    tp("AV-3i16", None),
    tp("IRfoo;<>", None),
    tp("IRfoo;<<>", None),
    tp("IRfoo;<>>", None),
    tp(s"TL${Long.MaxValue}", None),
    tp("TL-3", None),

  )) {
    test(s"JET signature $str") {
      sig match {
        case Some(sig) =>
          JETSignatureParser.parse(str) shouldBe sig
          sig.toJETSignature shouldBe str
        case None =>
          a[JETSignatureParser.Error] should be thrownBy {
            JETSignatureParser.parse(str)
          }
      }
    }
  }

  for ((sig, res) <- Seq(
    (NullableWrapper(InstantiatedReference("C", Seq(InstantiatedReference("C", Seq(LocalTypeVariable(0)))))), true),
    (NullableWrapper(InstantiatedReference("C", Seq(InstantiatedReference("C", Seq(CangjieReference("D")))))), false),
    (LocalTypeVariable(1) , true),
    (ClassTypeVariable(1) , true),
    (InstantiatedRecord("S", Seq(LocalTypeVariable(0))), true),
    (InstantiatedRecord("S", Seq(ClassTypeVariable(0))), true),
  ))
  test(s"$sig contains type variables") {
    sig.containsTypeVariables shouldBe res
  }
}
