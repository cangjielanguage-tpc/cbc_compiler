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
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SignatureSuite extends CompilerSuite with ScalaCheckPropertyChecks {

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

  for ((x, cparams, lparams, expected) <- Seq[(SignatureType, Seq[SignatureType], Seq[SignatureType], SignatureType)](
    // No type variables: returned unchanged
    (Int32, Seq(Record("Roo")), Seq(Int32), Int32),
    (BString, Seq(Record("Roo")), Seq(Int32), BString),
    (Record("Roo"), Seq(Record("Rar")), Seq(Int32), Record("Roo")),
    (CangjieReference("C"), Seq(Record("Roo")), Seq(Int32), CangjieReference("C")),
    (ThisTypeInfo, Seq(Record("Roo")), Seq(Int32), ThisTypeInfo),

    // No parameters: signature is returned as is
    (LocalTypeVariable(0), Seq.empty, Seq.empty, LocalTypeVariable(0)),
    (InstantiatedReference("C", Seq(LocalTypeVariable(0))), Seq.empty, Seq.empty,
      InstantiatedReference("C", Seq(LocalTypeVariable(0)))),

    // Type variables are substituted by the corresponding parameters
    (LocalTypeVariable(0), Seq.empty, Seq(Int32), Int32),
    (LocalTypeVariable(1), Seq.empty, Seq(Void, Int64), Int64),
    (ClassTypeVariable(0), Seq(Record("Roo")), Seq.empty, Record("Roo")),

    // Out of bounds type variables are preserved
    (LocalTypeVariable(0), Seq(Record("Roo")), Seq.empty, LocalTypeVariable(0)),
    (ClassTypeVariable(0), Seq.empty, Seq(Int32), ClassTypeVariable(0)),

    // Composite types are instantiated recursively
    (InstantiatedReference("C", Seq(LocalTypeVariable(0), ClassTypeVariable(0))), Seq(Record("Roo")), Seq(Int32),
      InstantiatedReference("C", Seq(Int32, Record("Roo")))),
    (InstantiatedRecord("S", Seq(LocalTypeVariable(0))), Seq.empty, Seq(Record("Roo")), InstantiatedRecord("S", Seq(Record("Roo")))),
    (CangjieArray(LocalTypeVariable(0)), Seq.empty, Seq(Int32), CangjieArray(Int32)),
    (VArray(LocalTypeVariable(0), 5), Seq.empty, Seq(Int32), VArray(Int32, 5)),
    (Tuple(Seq(LocalTypeVariable(0), ClassTypeVariable(0))), Seq(Record("Roo")), Seq(Int32), Tuple(Seq(Int32, Record("Roo")))),
    (PackedTuple(Seq(LocalTypeVariable(0))), Seq.empty, Seq(Int32), PackedTuple(Seq(Int32))),
    (ZeroSizedEnum("EZ", Seq(LocalTypeVariable(0))), Seq.empty, Seq(Int32), ZeroSizedEnum("EZ", Seq(Int32))),
    (PrimitiveBasedEnum("EP", Seq(LocalTypeVariable(0))), Seq.empty, Seq(Int32), PrimitiveBasedEnum("EP", Seq(Int32))),
    (ClassBasedEnum("EC", Seq(ClassTypeVariable(0))), Seq(Record("Roo")), Seq.empty, ClassBasedEnum("EC", Seq(Record("Roo")))),
    (UnionBasedEnum("EU", Seq(LocalTypeVariable(0))), Seq.empty, Seq(Int32), UnionBasedEnum("EU", Seq(Int32))),
    (OptionLikeEnum("EO", Seq(LocalTypeVariable(0)), LocalTypeVariable(1)), Seq.empty, Seq(Int32, CangjieReference("C")),
      OptionLikeEnum("EO", Seq(Int32), CangjieReference("C"))),

    // Box is removed when the instantiated type is a reference, kept otherwise
    (Box(LocalTypeVariable(0)), Seq.empty, Seq(Int32), Box(Int32)),
    (Box(LocalTypeVariable(0)), Seq.empty, Seq(CangjieReference("C")), CangjieReference("C")),
    (Box(InstantiatedReference("C", Seq(LocalTypeVariable(0)))), Seq.empty, Seq(Int32), InstantiatedReference("C", Seq(Int32))),

    (CPointer(LocalTypeVariable(0)), Seq.empty, Seq(Int32), CPointer(Int32)),
    (CPointer(InstantiatedRecord("S", Seq(LocalTypeVariable(0)))), Seq.empty, Seq(Int32),
      CPointer(InstantiatedRecord("S", Seq(Int32)))),
    (CPointer(InstantiatedRecord("S", Seq(ClassTypeVariable(0)))), Seq(Record("Roo")), Seq.empty,
      CPointer(InstantiatedRecord("S", Seq(Record("Roo"))))),
  )) {
    test(s"instantiate $x with cparams=$cparams lparams=$lparams") {
      x.instantiate(cparams, lparams) shouldBe expected
    }
  }

  for ((sig, cparams, lparams, expected) <- Seq[(MethodSignature, Seq[SignatureType], Seq[SignatureType], MethodSignature)](
    // Top-level type variables instantiated to non-reference types are boxed
    (MethodSignature()(LocalTypeVariable(0)), Seq.empty, Seq(Int32), MethodSignature()(Box(Int32))),
    (MethodSignature()(LocalTypeVariable(0)), Seq.empty, Seq(Record("Roo")), MethodSignature()(Box(Record("Roo")))),
    (MethodSignature(LocalTypeVariable(0), Int64)(ClassTypeVariable(0)), Seq(CangjieReference("C")), Seq(Int32),
      MethodSignature(Box(Int32), Int64)(CangjieReference("C"))),

    // Top-level type variables instantiated to references are not boxed
    (MethodSignature()(LocalTypeVariable(0)), Seq.empty, Seq(CangjieReference("C")), MethodSignature()(CangjieReference("C"))),
    (MethodSignature()(LocalTypeVariable(0)), Seq.empty, Seq(InstantiatedReference("C", Seq(Int32))),
      MethodSignature()(InstantiatedReference("C", Seq(Int32)))),

    // Nullable options are boxed even though they are references
    (MethodSignature()(LocalTypeVariable(0)), Seq.empty, Seq(OptionLikeEnum("EO", Seq.empty, CangjieReference("C"))),
      MethodSignature()(Box(OptionLikeEnum("EO", Seq.empty, CangjieReference("C"))))),

    // Type variable substituted by another type variable is not boxed
    (MethodSignature()(LocalTypeVariable(0)), Seq.empty, Seq(LocalTypeVariable(1)), MethodSignature()(LocalTypeVariable(1))),

    // Nested type variables are substituted without boxing
    (MethodSignature()(InstantiatedReference("C", Seq(LocalTypeVariable(0)))), Seq.empty, Seq(Int32),
      MethodSignature()(InstantiatedReference("C", Seq(Int32)))),

    // No parameters: signature is returned as is
    (MethodSignature(LocalTypeVariable(0), Int64)(Void), Seq.empty, Seq.empty, MethodSignature(LocalTypeVariable(0), Int64)(Void)),
  )) {
    test(s"instantiate $sig with cparams=$cparams lparams=$lparams") {
      sig.instantiate(cparams, lparams) shouldBe expected
    }
  }

  for ((x, y, expected) <- Seq[(SignatureType, SignatureType, Option[(Seq[SignatureType], Seq[SignatureType])])](
    // Single variables bind any signature, including another variable
    (LocalTypeVariable(0), Int64, Some((Seq.empty, Seq(Int64)))),
    (ClassTypeVariable(0), Record("Roo"), Some((Seq(Record("Roo")), Seq.empty))),
    (ClassTypeVariable(0), ClassTypeVariable(0), Some((Seq(ClassTypeVariable(0)), Seq.empty))),
    (LocalTypeVariable(1), ClassTypeVariable(0), Some((Seq.empty, Seq(LocalTypeVariable(0), ClassTypeVariable(0))))),

    (Record("Roo"), Record("Roo"), Some((Seq.empty, Seq.empty))),
    (Record("Roo"), Record("Rar"), None),
    (Int64, Int32, None),

    (InstantiatedReference("A", Seq(
      InstantiatedReference("B", Seq(ClassTypeVariable(0), LocalTypeVariable(1))),
      InstantiatedReference("C", Seq(InstantiatedReference("D", Seq(ClassTypeVariable(2))))),
    )),
      InstantiatedReference("A", Seq(
        InstantiatedReference("B", Seq(Int64, ClassTypeVariable(0))),
        InstantiatedReference("C", Seq(InstantiatedReference("D", Seq(CangjieArray(ClassTypeVariable(0)))))),
      )),
      Some((Seq(Int64, ClassTypeVariable(1), CangjieArray(ClassTypeVariable(0))), Seq(LocalTypeVariable(0), ClassTypeVariable(0))))),

    // A repeated variable must be bound consistently
    (InstantiatedReference("C", Seq(ClassTypeVariable(0), ClassTypeVariable(0))), InstantiatedReference("C", Seq(Int64, Int64)),
      Some((Seq(Int64), Seq.empty))),
    (InstantiatedReference("C", Seq(ClassTypeVariable(0), ClassTypeVariable(0))), InstantiatedReference("C", Seq(Int64, Int32)), None),
    (InstantiatedReference("C", Seq(ClassTypeVariable(0), InstantiatedReference("D", Seq(ClassTypeVariable(0))))),
      InstantiatedReference("C", Seq(Int64, InstantiatedReference("D", Seq(Int32)))), None),
    (Tuple(Seq(ClassTypeVariable(0), ClassTypeVariable(0))), Tuple(Seq(Int64, Int32)), None),

    (InstantiatedReference("C", Seq(ClassTypeVariable(0))), InstantiatedReference("D", Seq(Int64)), None),
    (InstantiatedReference("C", Seq(ClassTypeVariable(0))), InstantiatedReference("C", Seq(Int64, Int32)), None),
    (InstantiatedReference("C", Seq(ClassTypeVariable(0))), InstantiatedRecord("C", Seq(Int64)), None),

    // Box(base) matches the collapsed base exactly when instantiate would collapse it
    (Box(ClassTypeVariable(0)), CangjieReference("C"), Some((Seq(CangjieReference("C")), Seq.empty))),
    (Box(ClassTypeVariable(0)), Box(Int64), Some((Seq(Int64), Seq.empty))),
    (Box(ClassTypeVariable(0)), OptionLikeEnum("O", Seq.empty, CangjieReference("C")), None),
    (Box(ClassTypeVariable(0)), Int64, None),

    (VArray(ClassTypeVariable(0), 3), VArray(Int64, 3), Some((Seq(Int64), Seq.empty))),
    (VArray(ClassTypeVariable(0), 3), VArray(Int64, 5), None),
    (Tuple(Seq(ClassTypeVariable(0), ClassTypeVariable(1))), Tuple(Seq(Int64)), None),

    // instantiate never substitutes inside a CPointer over a MethodSignature
    (CPointer(MethodSignature(LocalTypeVariable(0))(Void)), CPointer(MethodSignature(LocalTypeVariable(0))(Void)),
      Some((Seq.empty, Seq.empty))),
    (CPointer(MethodSignature(LocalTypeVariable(0))(Void)), CPointer(MethodSignature(LocalTypeVariable(1))(Void)), None),

    // Variables absent from the bindings are substituted by themselves
    (CPointer(ClassTypeVariable(3)), CPointer(Record("Roo")),
      Some((Seq(ClassTypeVariable(0), ClassTypeVariable(1), ClassTypeVariable(2), Record("Roo")), Seq.empty))),
  )) {
    test(s"$x matchInstantiated $y") {
      x.matchInstantiated(y) shouldBe expected
    }
  }

  for ((x, y, expected) <- Seq[(MethodSignature, MethodSignature, Option[(Seq[SignatureType], Seq[SignatureType])])](
    (MethodSignature()(ClassTypeVariable(0)), MethodSignature()(Int64), Some((Seq(Int64), Seq.empty))),
    (MethodSignature()(LocalTypeVariable(0)), MethodSignature()(Int64), Some((Seq.empty, Seq(Int64)))),
    (MethodSignature()(ClassTypeVariable(0)), MethodSignature()(Box(Int64)), Some((Seq(Box(Int64)), Seq.empty))),
    (MethodSignature(ClassTypeVariable(0))(ClassTypeVariable(0)), MethodSignature(Int64)(Int64), Some((Seq(Int64), Seq.empty))),
    (MethodSignature(ClassTypeVariable(0))(ClassTypeVariable(0)), MethodSignature(Int64)(Int32), None),
    (MethodSignature(Int64)(Void), MethodSignature(Int64, Int32)(Void), None),
    (MethodSignature(Int64)(Void), MethodSignature(Int64)(Int64), None),
  )) {
    test(s"$x matchInstantiated $y") {
      x.matchInstantiated(y) shouldBe expected
    }
  }

  for ((x, expected) <- Seq[(SignatureType, SignatureType)](
    // No type parameters: returned unchanged
    (Int32, Int32),
    (BString, BString),
    (Record("Roo"), Record("Roo")),
    (CangjieReference("C"), CangjieReference("C")),
    (ThisTypeInfo, ThisTypeInfo),
    (LocalTypeVariable(3), LocalTypeVariable(3)),
    (ClassTypeVariable(3), ClassTypeVariable(3)),

    // Instantiated type parameters are erased to class type variables by position
    (InstantiatedReference("C", Seq(Int32, Record("Roo"))),
      InstantiatedReference("C", Seq(ClassTypeVariable(0), ClassTypeVariable(1)))),
    (InstantiatedRecord("S", Seq(Int32)), InstantiatedRecord("S", Seq(ClassTypeVariable(0)))),
    (Tuple(Seq(Int32, Int64)), Tuple(Seq(ClassTypeVariable(0), ClassTypeVariable(1)))),
    (PackedTuple(Seq(Int32)), PackedTuple(Seq(ClassTypeVariable(0)))),
    (ZeroSizedEnum("EZ", Seq(Int32, Int64)), ZeroSizedEnum("EZ", Seq(ClassTypeVariable(0), ClassTypeVariable(1)))),
    (PrimitiveBasedEnum("EP", Seq(Int32)), PrimitiveBasedEnum("EP", Seq(ClassTypeVariable(0)))),
    (ClassBasedEnum("EC", Seq(Record("Roo"))), ClassBasedEnum("EC", Seq(ClassTypeVariable(0)))),
    (UnionBasedEnum("EU", Seq(Int32)), UnionBasedEnum("EU", Seq(ClassTypeVariable(0)))),

    // Element or pointee type is replaced with a class type variable
    (Box(Int32), Box(ClassTypeVariable(0))),
    (CangjieArray(Int32), CangjieArray(ClassTypeVariable(0))),
    (VArray(Int32, 5), VArray(ClassTypeVariable(0), 5)),
    (CPointer(Int32), CPointer(ClassTypeVariable(0))),
    (CPointer(MethodSignature(Int32)(Int64)), CPointer(ClassTypeVariable(0))),
    (OptionLikeEnum("EO", Seq(Int32), CangjieReference("C")), OptionLikeEnum("EO", Seq(ClassTypeVariable(0)), ClassTypeVariable(0))),
  )) {
    test(s"uninstantiated $x") {
      x.uninstantiated shouldBe expected
    }
  }

  import SignatureGenerators.*

  test("SignatureType.uninstantiated is idempotent") {
    forAll(genSig(3)) { (x: SignatureType) =>
      x.uninstantiated.uninstantiated shouldBe x.uninstantiated
    }
  }

  test("variable-free SignatureType is not changed by instantiate") {
    forAll(genSig(3), genInstantiationParams) { (x: SignatureType, params: (Seq[SignatureType], Seq[SignatureType])) =>
      whenever(!x.containsTypeVariables) {
        val (cparams, lparams) = params
        x.instantiate(cparams, lparams) shouldBe x
      }
    }
  }

  test("substituting each SignatureType variable by itself is identity") {
    forAll(genSig(3)) { (x: SignatureType) =>
      x.instantiate(identityCParams, identityLParams) shouldBe x
    }
  }

  test("sequential SignatureType instantiation equals instantiation of substituted parameters") {
    forAll(genSig(3), genTotalInstantiationParams, genInstantiationParams) {
      (x: SignatureType, p1: (Seq[SignatureType], Seq[SignatureType]), p2: (Seq[SignatureType], Seq[SignatureType])) =>
        val (c1, l1) = p1
        val (c2, l2) = p2
        x.instantiate(c1, l1).instantiate(c2, l2) shouldBe
          x.instantiate(c1.map(_.instantiate(c2, l2)), l1.map(_.instantiate(c2, l2)))
    }
  }

  test("uninstantiated followed by instantiate restores the original InstantiatedType") {
    forAll(genInstantiatedType) { (x: SignatureType.InstantiatedType) =>
      x.uninstantiated.instantiate(x.instantiatedTypeParameters, Seq.empty) shouldBe x
    }
  }

  test("substituting each MethodSignature variable by itself is identity") {
    forAll(genMethodSignature(3)) { (m: MethodSignature) =>
      m.instantiate(identityCParams, identityLParams) shouldBe m
    }
  }

  test("sequential MethodSignature instantiation equals instantiation of substituted parameters") {
    forAll(genMethodSignature(3), genTotalInstantiationParams, genInstantiationParams) {
      (m: MethodSignature, p1: (Seq[SignatureType], Seq[SignatureType]), p2: (Seq[SignatureType], Seq[SignatureType])) =>
        val (c1, l1) = p1
        val (c2, l2) = p2
        m.instantiate(c1, l1).instantiate(c2, l2) shouldBe
          m.instantiate(c1.map(_.instantiate(c2, l2)), l1.map(_.instantiate(c2, l2)))
    }
  }

  test("matchInstantiatedImpl collects bindings of repeated occurrences") {
    val x = InstantiatedReference("A", Seq(
      InstantiatedReference("B", Seq(ClassTypeVariable(0), LocalTypeVariable(1))),
      InstantiatedReference("C", Seq(InstantiatedReference("D", Seq(ClassTypeVariable(0))))),
    ))
    val y = InstantiatedReference("A", Seq(
      InstantiatedReference("B", Seq(Int64, ClassTypeVariable(0))),
      InstantiatedReference("C", Seq(InstantiatedReference("D", Seq(Int64)))),
    ))
    x.matchInstantiatedImpl(y) shouldBe
      Some(Map[TypeVariable, SignatureType](ClassTypeVariable(0) -> Int64, LocalTypeVariable(1) -> ClassTypeVariable(0)))
  }

  test("matchInstantiated of an instantiated signature restores the substitution") {
    forAll(genSig(3), genTotalInstantiationParams) { (x: SignatureType, params: (Seq[SignatureType], Seq[SignatureType])) =>
      val (cparams, lparams) = params
      val y = x.instantiate(cparams, lparams)
      x.matchInstantiated(y) match {
        case Some((c2, l2)) => x.instantiate(c2, l2) shouldBe y
        case None => fail(s"expected $x to match its instantiation $y")
      }
    }
  }

  test("MethodSignature matchInstantiated of an instantiated signature restores the substitution") {
    forAll(genMethodSignature(3), genTotalInstantiationParams) { (m: MethodSignature, params: (Seq[SignatureType], Seq[SignatureType])) =>
      val (cparams, lparams) = params
      val y = m.instantiate(cparams, lparams)
      m.matchInstantiated(y) match {
        case Some((c2, l2)) => m.instantiate(c2, l2) shouldBe y
        case None => fail(s"expected $m to match its instantiation $y")
      }
    }
  }
}
