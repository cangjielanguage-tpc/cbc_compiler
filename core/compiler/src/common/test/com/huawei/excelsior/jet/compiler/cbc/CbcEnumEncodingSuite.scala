/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat
import com.huawei.excelsior.jet.assembler.cbc.CbcFileFormat.{BuiltinSignature, TypeEnumKind, TypeFlag}
import com.huawei.excelsior.jet.compiler.{CompilerSuite, TypeProvider}
import com.huawei.excelsior.jet.compiler.cangjie.CangjieEnumInfo
import com.huawei.excelsior.jet.compiler.cangjie.CangjieEnumInfo.{Constructor, Kind}
import com.huawei.excelsior.jet.compiler.symlevel.{SignatureType, TypeKind}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeEnvironment, FakeType}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes
import com.huawei.excelsior.jet.compiler.cbc.CbcSignatureAdapter.toCbc

class CbcEnumEncodingSuite extends CompilerSuite {
  private val testEnv = new FakeEnvironment
  CBCFileGenerator.env = testEnv
  ReferenceTypes.setEnvForUnitTests(testEnv)
  implicit private val typeProvider: TypeProvider = testEnv

  private def encode(info: CangjieEnumInfo): CbcFileFormat.Type = {
    val file = CbcFileFormat.newBuilder()
    val builder = file.newTypeBuilder()
    builder.setName("test:Enum")
    CbcEnumEncoding.write(info, builder)
    file.build().types.head
  }

  for (imported <- Seq(false, true); generic <- Seq(false, true)) {
    test(s"class enum keeps reference signatures and class metadata (imported=$imported, generic=$generic)") {
      val name = s"test:RefEnum${imported}${generic}"
      val args = if (generic) Seq(SignatureType.Int64) else Seq.empty
      val info = CangjieEnumInfo(Kind.ClassBased, Seq(
        Constructor(Seq(SignatureType.Reference("test:Payload", jbc = false))),
        Constructor(Seq(SignatureType.Int64)), Constructor(Seq.empty)))
      val sym = FakeType(name, TypeKind.CLASS).markAsCangjieType()
      sym.setCangjieEnumInfo(info)
      sym.setCHIRDef(!imported)
      testEnv.registerFake(sym)

      val signature = SignatureType.ClassBasedEnum(name, args).toCbc
      val cbcArgs = args.map(_.toCbc)
      val expected = if (imported) CbcFileFormat.AotTypeSignature(name, cbcArgs, true)
        else CbcFileFormat.TypeSignature(name, cbcArgs, true)
      assert(signature == expected)
      val definition = encode(info)
      assert(!definition.flags.contains(TypeFlag.ENUM))
      assert(!definition.flags.contains(TypeFlag.RECORD))
      assert(!CbcEnumEncoding.isValueEnum(info))
      assert(definition.enumKind == TypeEnumKind.NotEnum)
      assert(definition.unionFields.isEmpty)
    }
  }

  test("value enum retains its tag and union metadata") {
    val definition = encode(CangjieEnumInfo(Kind.UnionBased, Seq(
      Constructor(Seq(SignatureType.Int32)), Constructor(Seq(SignatureType.Int64)))))
    assert(definition.flags.contains(TypeFlag.ENUM))
    assert(definition.enumKind == TypeEnumKind.Union)
    assert(definition.unionFields == Seq(
      CbcFileFormat.Tuple(Seq(BuiltinSignature.U32, BuiltinSignature.I32)),
      CbcFileFormat.Tuple(Seq(BuiltinSignature.U32, BuiltinSignature.I64))))
  }

  test("primitive and zero-sized enums retain their underlying representation") {
    for ((kind, base) <- Seq(Kind.PrimitiveBased -> BuiltinSignature.I32, Kind.ZeroSized -> BuiltinSignature.Unit)) {
      val definition = encode(CangjieEnumInfo(kind, Seq(Constructor(Seq.empty))))
      assert(definition.flags.contains(TypeFlag.ENUM))
      assert(definition.enumKind == TypeEnumKind.Primitive)
      assert(definition.superOrEnumType.contains(base))
      assert(definition.unionFields.isEmpty)
    }
  }

  test("option metadata preserves constructor order and payload") {
    for (someFirst <- Seq(false, true)) {
      val some = Constructor(Seq(SignatureType.Int64))
      val none = Constructor(Seq.empty)
      val definition = encode(CangjieEnumInfo(Kind.OptionLike, if (someFirst) Seq(some, none) else Seq(none, some)))
      assert(definition.flags.contains(TypeFlag.ENUM))
      assert(definition.enumKind == (if (someFirst) TypeEnumKind.Option0 else TypeEnumKind.Option1))
      assert(definition.superOrEnumType.contains(BuiltinSignature.I64))
      assert(definition.unionFields.isEmpty)
    }
  }

  test("nullable option metadata erases the reference payload to Object") {
    val objectType = FakeType("std.core:Object", TypeKind.CLASS).markAsCangjieType()
    objectType.setCHIRDef(false)
    testEnv.registerFake(objectType)
    val payload = SignatureType.Reference("test:Payload", jbc = false)
    for (someFirst <- Seq(false, true)) {
      val some = Constructor(Seq(payload))
      val none = Constructor(Seq.empty)
      val definition = encode(CangjieEnumInfo(Kind.OptionLike, if (someFirst) Seq(some, none) else Seq(none, some)))
      assert(definition.flags.contains(TypeFlag.ENUM))
      assert(definition.enumKind == (if (someFirst) TypeEnumKind.Option0 else TypeEnumKind.Option1))
      assert(definition.superOrEnumType.contains(CbcFileFormat.AotTypeSignature.ref("std.core:Object")))
      assert(definition.unionFields.isEmpty)
    }
  }

  test("value enum and option signatures keep their distinct encodings") {
    assert(SignatureType.PrimitiveBasedEnum("test:Scalar", Seq.empty).toCbc ==
      CbcFileFormat.PrimitiveEnum("test:Scalar", Seq.empty))
    assert(SignatureType.UnionBasedEnum("test:Value", Seq.empty).toCbc ==
      CbcFileFormat.UnionEnum("test:Value", Seq.empty))
    assert(SignatureType.OptionLikeEnum("test:Option", Seq(SignatureType.Int64), SignatureType.Int64).toCbc ==
      CbcFileFormat.OptionSignature("test:Option", Seq(BuiltinSignature.I64), false, true))
  }
}
