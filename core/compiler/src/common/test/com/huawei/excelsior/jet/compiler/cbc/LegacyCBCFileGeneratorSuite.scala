/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cbc

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.{CompilerSuite, RTConst}
import com.huawei.excelsior.jet.compiler.cbc.LegacyCBCFileGenerator.*
import com.huawei.excelsior.jet.compiler.cbc.LegacyCBCFileGeneratorSuite.setRTConsts
import com.huawei.excelsior.jet.compiler.symlevel.GenericInfo.Constraint
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.{CLASS, RECORD}
import com.huawei.excelsior.jet.compiler.symlevel.{ClassType, GenericInfo, Method, MethodReference, MethodSignature, MethodType, Signature, SignatureType, Type, TypeKind}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeEnvironment, FakeMethod, FakeMethodReference, FakeMethodType, FakeType}
import org.scalactic.source.Position
import xscala.io.LEB128Encoder

import scala.collection.mutable

class LegacyCBCFileGeneratorSuite extends CompilerSuite {

  object Sandbox {
    class SomeClass {
      def someMethod(c: SomeClass, arr1: Array[Byte], arr2: Array[SomeClass]): Unit = {}
    }
  }
  val someClass = FakeType.create(classOf[Sandbox.SomeClass])
  val classes = Array[Type](
    someClass,
    // some arbitrary classes with lots of methods and complex hierarchy, feel free to add more
    FakeType.create(classOf[java.lang.Object]),
    FakeType.create(classOf[java.lang.String]),
    FakeType.create(classOf[java.lang.Class[_]]),
    FakeType.create(classOf[java.lang.reflect.Method]),
    FakeType.create(classOf[java.util.ArrayList[_]]),
    FakeType.create(classOf[java.util.HashSet[_]]),
    FakeType.create(classOf[java.util.LinkedHashMap[_, _]]),
    FakeType.create(classOf[java.lang.Enum[_]]),
    FakeType.create(classOf[java.util.EnumSet[_]]),
    FakeType.create(classOf[Set[_]]),
    FakeType.create(classOf[scala.collection.mutable.LinkedHashMap[_, _]]),
    FakeType.create(classOf[Type]),
    FakeType.create(classOf[Method])
  ) ensuring (_ forall (t => !(t.isPrimitive || t.isArray)))

  private def prepareEnv() = {
    val segment = new Segment()
    val env = new FakeEnvironment
    setRTConsts(env)
    env.setAllClasses(FakeType.getCreatedTypes.filter(t => !(t.isPrimitive || t.isArray)).map(asClassType))
    CBCFileGenerator.env = env
    val gen = new LegacyCBCFileGenerator(env, "test.cbc", segment) {
      override def writeCode(): Unit = {}
      override val bytecode = mutable.Map.empty[Method, Offset].withDefaultValue(0)
    }
    (gen, segment)
  }

  private val fakeCache = new mutable.HashMap[String, FakeType]

  def fake(name: String, kind: TypeKind) = fakeCache.getOrElseUpdate(name, FakeType(name, kind)).markAsCangjieType()
  def fakeRef(name: String) = fake(name, CLASS)
  def fakeRec(name: String) = fake(name, RECORD)

  override def afterEach(): Unit = {
    // Cleanup to not break other test suites
    fakeCache.values.foreach(_.unmarkAsCangjieType())

    super.afterEach()
  }

  test("CBC file generation") {
    val (gen, _) = prepareEnv()
    gen.genCBCFile() // just check that we can gen something without problems
  }

  object SignatureSandbox {
    def prepareEnv() = {
      val segment = new Segment()
      val env = new FakeEnvironment
      setRTConsts(env)
      CBCFileGenerator.env = env
      val gen = new LegacyCBCFileGenerator(env, "fake.cbc", segment)
      (gen, segment)
    }

    def rec() = Record(fakeRec("REC"))
    def ref() = NullableWrapper(CangjieReference(fakeRef("REF")))
    def nref() = CangjieReference(fakeRef("REF"))
    def nref(name: String) = CangjieReference(fakeRef(name))
    def carr(baseType: SignatureType) = CangjieArray(baseType)
    def jarr(baseType: SignatureType, dim: Int) = JavaArray(baseType, dim)
    def as(baseType: SignatureType) = {
      val sig = ArraySlice(baseType)
      env.asInstanceOf[FakeEnvironment].typesResolution.getOrElseUpdate(XString(sig.name), fakeRec(sig.name))
      sig
    }
    def en(baseType: CangjieEnumWrapper.Base, name: String) = CangjieEnumWrapper(baseType, name)
    def ptr(p: Signature) = CPointer(p)
    def m(params: SignatureType*)(rt: SignatureType) = MethodSignature(rt, params)
    def varr(baseType: SignatureType, dim: Long) = VArray(baseType, dim)
    def iref(name: String, instantiatedTypeParameters: SignatureType*) = {
      env.asInstanceOf[FakeEnvironment].typesResolution.getOrElseUpdate(XString(name), fakeRef(name))
      InstantiatedReference(name, instantiatedTypeParameters)
    }
    def irec(name: String, instantiatedTypeParameters: SignatureType*) = {
      env.asInstanceOf[FakeEnvironment].typesResolution.getOrElseUpdate(XString(name), fakeRec(name))
      InstantiatedRecord(name, instantiatedTypeParameters)
    }
    def igm(methodRef: MethodReference, typeParameters: SignatureType*) = {
      InstantiatedGenericMethod(methodRef, typeParameters)
    }
    def tv(idx: Int) = LocalTypeVariable(idx)

    class SigDecoder(_gen: LegacyCBCFileGenerator, sigBytes: Array[Byte]) {
      private[cbc] val gen = _gen

      import gen.sigIndex.*

      private val typeIndices: mutable.Map[Int, SignatureType | ConstraintType] = prepareIndices(gen.typeIndex).map((k, v) => (k, v match {
        case t: Type => SignatureType.fromSymType(t)
        case ct: ConstraintType => ct
      }))
      private val methodIndices = prepareIndices(gen.methodRefIndex)
      private[cbc] val signatureIndices = prepareIndices(_sigIndex)

      private var sigPos: Int = gen.strings.lastOption.map((s, offset) => offset + s.length + 1).getOrElse(0)

      private[cbc] def decode(): Sig = {
        if (sigPos >= sigBytes.length) {
          return null
        }
        val sigTag = readULEB128()
        SignatureTag.fromOrdinal(sigTag) match {
          case SignatureTag.Nil =>
            shouldNotReachHere("unexpected Nil tag")
          case SignatureTag.Record =>
            SignatureType.fromSymType(getClassType)
          case SignatureTag.Reference =>
            CangjieReference(getClassType)
          case SignatureTag.JavaReference =>
            JBCReference(getClassType)
          case SignatureTag.CangjieArray =>
            CangjieArray(getSignatureType)
          case SignatureTag.JavaArray =>
            val dim = readULEB128()
            val sig = getSignatureType
            JavaArray(sig, dim)
          case SignatureTag.Nullable =>
            val sig = getSignatureType
            sig match {
              case sig: NullableWrapper.Base =>
                NullableWrapper(sig)
              case _ => shouldNotReachHere(sig)
            }
          case SignatureTag.NonNullable =>
            val sig = getSignatureType
            sig match {
              case sig: NonNullableWrapper.Base =>
                NonNullableWrapper(sig)
              case _ => shouldNotReachHere(sig)
            }
          case SignatureTag.EnumWrapper =>
            val sig = getSignatureType
            sig match {
              case sig: CangjieEnumWrapper.Base =>
                val nameOffset = readUInt()
                val name = readModifiedUtf8String(nameOffset)
                CangjieEnumWrapper(sig, name)
              case _ => shouldNotReachHere(sig)
            }
          case SignatureTag.CPointer =>
            CPointer(getSignatureType)
          case SignatureTag.MethodSignature =>
            val paramTypesNum = readUByte().asInstanceOf[Byte]
            val parameterTypes = Array.fill[Int](paramTypesNum)(readULEB128()).map { idx =>
              signatureIndices(idx).asInstanceOf[SignatureType]
            }.toSeq
            val returnType = getSignatureType
            MethodSignature(returnType, parameterTypes)
          case SignatureTag.VArray =>
            val elem = getSignatureType
            val size = readULEB128()
            VArray(elem, size)
          case SignatureTag.GenericReference =>
            val ref = getClassType
            val typeParametersNum = readUByte()
            val typeParameters = Array.fill[Int](typeParametersNum)(readULEB128()).map { idx =>
              if (idx == BuiltinSignature.Nil.idx) {
                BuiltinSignature.Nil
              } else {
                signatureIndices(idx).asInstanceOf[SignatureType | GenericType]
              }
            }.toSeq
            GenericReference(ref, typeParameters)
          case SignatureTag.GenericRecord =>
            val rec = getClassType
            val typeParametersNum = readUByte()
            val typeParameters = Array.fill[Int](typeParametersNum)(readULEB128()).map { idx =>
              if (idx == BuiltinSignature.Nil.idx) {
                BuiltinSignature.Nil
              } else {
                signatureIndices(idx).asInstanceOf[SignatureType | GenericType]
              }
            }.toSeq
            GenericRecord(rec, typeParameters)
          case SignatureTag.GenericTypeTerm =>
            val instantiated = getGenericEntity
            val freeVariablesIndicesNum = readUByte()
            val freeVariablesIndices = Array.fill[Int](freeVariablesIndicesNum)(readUByte()).toSeq
            GenericTypeTerm(instantiated, freeVariablesIndices)
          case SignatureTag.GenericMethod =>
            val methodRef = getMethodRef
            val typeParametersNum = readUByte()
            val typeParameters = Array.fill[Int](typeParametersNum)(readULEB128()).map { idx =>
              if (idx == BuiltinSignature.Nil.idx) {
                BuiltinSignature.Nil
              } else {
                signatureIndices(idx).asInstanceOf[SignatureType | GenericType]
              }
            }.toSeq
            GenericMethod(methodRef, typeParameters)
          case SignatureTag.Constraint =>
            getConstraintType
          case SignatureTag.ParameterizedConstraint =>
            val constraintType = getConstraintType
            val freeVariablesNum = readUByte()
            val freeVariables = Array.fill[Int](freeVariablesNum)(readUByte()).map(idx => tv(idx)).toSet
            ParameterizedConstraint(constraintType.supers, freeVariables)

          case SignatureTag.GenericTypeVar => shouldNotReachHere()
        }
      }

      private def getClassType: ClassType = {
        val sig = typeIndices(readULEB128()).asInstanceOf[SignatureType]
        asClassType(sig)(env.getTypeProvider)
      }

      private def getConstraintType: ConstraintType = {
        typeIndices(readULEB128()).asInstanceOf[ConstraintType]
      }

      private def getMethodRef: MethodReference = {
        methodIndices(readULEB128())
      }

      private def getSignatureType: SignatureType = {
        val sig = signatureIndices(readULEB128())
        sig.asInstanceOf[SignatureType]
      }

      private def getGenericEntity: GenericEntity = {
        val sig = signatureIndices(readULEB128())
        sig.asInstanceOf[GenericEntity]
      }

      private def readModifiedUtf8String(pos: Int): String = {
        val oldPos = this.sigPos
        this.sigPos = pos
        val bytes = Array.fill[Byte](readULEB128())(readUByte().asInstanceOf[Byte])
        this.sigPos = oldPos
        XString(bytes).toString
      }

      private def readUInt(): Int = {
        readUShort() | (readUShort() << 16)
      }

      private def readUShort(): Int = {
        0xFFFF & (readUByte() | (readUByte() << 8))
      }

      private def readUByte(): Int = {
        val res = 0xFF & sigBytes(sigPos)
        sigPos += 1
        res
      }

      private def readULEB128(): Int = {
        LEB128Encoder.decodeULEB128(() =>
          val b = sigBytes(sigPos)
          sigPos += 1
          b
        )
      }

      private def prepareIndices[T](indices: mutable.Map[T, gen.Index]): mutable.Map[gen.Index, T] = {
        val result = mutable.Map.empty[Int, T]
        for ((k, v) <- indices) {
          result.put(v, k)
        }
        result
      }
    }
  }

  test("test signature decoding") {
    val (gen, segment) = SignatureSandbox.prepareEnv()

    import SignatureSandbox.*
    import gen.sigIndex.*
    import gen.sigIndex

    def gref(name: String, typeParameters: GenericTypeParameter*) = {
      val classType = env.asInstanceOf[FakeEnvironment].typesResolution.getOrElseUpdate(XString(name), fakeRef(name))
      GenericReference(classType, typeParameters)
    }
    def grec(name: String, typeParameters: GenericTypeParameter*) = {
      val classType = env.asInstanceOf[FakeEnvironment].typesResolution.getOrElseUpdate(XString(name), fakeRef(name))
      GenericRecord(classType, typeParameters)
    }
    def constr(supers: SignatureType*) = {
      sigIndex.ConstraintType(GenericInfo.Constraint(tv(0), supers.toSeq)) ensuring(_.isInstanceOf[sigIndex.Constraint])
    }
    def pconstr(supers: SignatureType*)  = {
      sigIndex.ConstraintType(GenericInfo.Constraint(tv(0), supers.toSeq)) ensuring(_.isInstanceOf[ParameterizedConstraint])
    }
    def gtt(instantiated: GenericEntity, freeVariablesIndices: Int*) = {
      GenericTypeTerm(instantiated, freeVariablesIndices)
    }
    def gm(methodRef: MethodReference, typeParameters: GenericTypeParameter*) = {
      GenericMethod(methodRef, typeParameters)
    }

    val sigFromCodeAndMd = m()(Int32)
    val sigFromMdAndCode = m()(Int64)

    val methodRef = new FakeMethod("foo") {

      override def getDeclaringClass = {
        val declClass = super.getDeclaringClass
        fake(declClass.getName, declClass.getKind)
      }

      override def isUniversalGeneric = true
      // generic info with 2 constraints to satisfy the assertion in InstantiatedGenericMethod
      override def getGenericInfo = GenericInfo(Seq(GenericInfo.Constraint(tv(0), Seq.empty), GenericInfo.Constraint(tv(1), Seq.empty)))
    }.getMethodReference

    val genericAsTypeRefAndConstraint1 = iref("C", tv(1))
    val genericAsTypeRefAndConstraint2 = iref("D", irec("SS", tv(1)))

    val sigsFromMd = Array[Sig](
      rec(),
      BString,
      carr(Unit),
      jarr(nref(), 10),
      as(carr(nref())),
      nref(),
      carr(Nothing),
      jarr(Int32, 5),
      jarr(ref(), 10),
      en(Float16, "Enum1"),
      en(ref(), "Enum2"),
      ptr(ref()),
      m(ref(), Float32)(as(carr(ref()))),
      sigFromCodeAndMd,
      sigFromMdAndCode,
      ptr(Float64),
      ref(),
      varr(Int64, 30),
      iref("X", iref("X", Int32)),
      iref("X", Int32),
      irec("S", Float64),
      iref("Y", iref("X", iref("X", tv(0))), iref("X", tv(1))),
      irec("S", iref("X", irec("S", tv(0)))),
      genericAsTypeRefAndConstraint1,
      genericAsTypeRefAndConstraint2,
    )

    val constraints = Array[ConstraintType](
      constr(iref("X", Int32), iref("Y", iref("X", iref("X", Int32)), iref("X", Float64))),
      pconstr(iref("Y", tv(0), tv(1)), iref("Y", tv(1), tv(0))),
      // duplicate for the previous pconstr
      pconstr(iref("Y", tv(0), tv(1)), iref("Y", tv(1), tv(0))),
      pconstr(iref("Y", tv(0), tv(1)), iref("Y", tv(1), tv(1))),
      pconstr(genericAsTypeRefAndConstraint1),
      pconstr(genericAsTypeRefAndConstraint2),
    )

    sigIndex.indexFromCode(MethodType(sigFromCodeAndMd))

    sigsFromMd foreach sigIndex.indexFromMetadata

    sigIndex.indexFromCode(MethodType(sigFromMdAndCode))

    constraints foreach sigIndex.indexFromMetadata

    val sigFromCode = MethodSignature()(Float64)
    sigIndex.indexFromCode(MethodType(sigFromCode))

    val genericMethodWithFreeVars = igm(methodRef, iref("Y", tv(1), tv(0)), tv(0))
    sigIndex.indexFromCode(genericMethodWithFreeVars)

    sigIndex.freeze()

    gen.writeStringsSection(gen.coldStrings)
    gen.writeStringsSection(gen.strings)

    sigIndex.writeSignatures()

    val decoder = SigDecoder(gen, segment.toByteArray)
    for (signature <- Array[Sig](
      // signatures from code
      sigFromCodeAndMd,
      sigFromMdAndCode,
      sigFromCode,
      gtt(gm(methodRef, gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), BuiltinSignature.Nil), 1, 0, 0),
      // signatures from metadata
      rec(),
      carr(Unit),
      jarr(nref(), 10),
      nref(),
      as(carr(nref())),
      carr(nref()),
      carr(Nothing),
      jarr(Int32, 5),
      jarr(ref(), 10),
      ref(),
      en(Float16, "Enum1"),
      en(ref(), "Enum2"),
      ptr(ref()),
      m(ref(), Float32)(as(carr(ref()))),
      as(carr(ref())),
      carr(ref()),
      ptr(Float64),
      varr(Int64, 30),
      gref("X", gref("X", Int32)),
      gref("X", Int32),
      grec("S", Float64),
      gtt(gref("Y", gref("X", gref("X", BuiltinSignature.Nil)), gref("X", BuiltinSignature.Nil)), 0, 1),
      gtt(gref("X", gref("X", BuiltinSignature.Nil)), 0),
      gtt(gref("X", BuiltinSignature.Nil), 0),
      gref("X", BuiltinSignature.Nil),
      gref("X", gref("X", BuiltinSignature.Nil)),
      gtt(gref("X", BuiltinSignature.Nil), 1),
      gref("Y", gref("X", gref("X", BuiltinSignature.Nil)), gref("X", BuiltinSignature.Nil)),
      gtt(grec("S", gref("X", grec("S", BuiltinSignature.Nil))), 0),
      gtt(gref("X", grec("S", BuiltinSignature.Nil)), 0),
      gtt(grec("S", BuiltinSignature.Nil), 0),
      grec("S", BuiltinSignature.Nil),
      gref("X", grec("S", BuiltinSignature.Nil)),
      grec("S", gref("X", grec("S", BuiltinSignature.Nil))),
      gtt(gref("C", BuiltinSignature.Nil), 1),
      gref("C", BuiltinSignature.Nil),
      gtt(gref("D", grec("SS", BuiltinSignature.Nil)), 1),
      gtt(grec("SS", BuiltinSignature.Nil), 1),
      grec("SS", BuiltinSignature.Nil),
      gref("D", grec("SS", BuiltinSignature.Nil)),
      constraints(0),
      gref("Y", gref("X", gref("X", Int32)), gref("X", Float64)),
      gref("X", Float64),
      constraints(1),
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 0, 1),
      gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil),
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 1, 0),
      constraints(3),
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 1, 1),
      constraints(4),
      gtt(gref("C", BuiltinSignature.Nil), 0),
      constraints(5),
      gtt(gref("D", grec("SS", BuiltinSignature.Nil)), 0),
      gtt(grec("SS", BuiltinSignature.Nil), 0),
      nref("Fake"),
      m()(Void),
      gm(methodRef, gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), BuiltinSignature.Nil),
    )) {
      decoder.decode() shouldBe signature
    }
    decoder.decode() shouldBe null

    def checkIndexedSignatureTypes(in: Seq[Sig], out: Seq[Sig]): Unit = {
      in map sigIndex.apply map decoder.signatureIndices.apply shouldBe out
    }
    checkIndexedSignatureTypes(Seq(genericMethodWithFreeVars), Seq(
      gtt(gm(methodRef, gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), BuiltinSignature.Nil), 1, 0, 0),
    ))
    checkIndexedSignatureTypes(constraints(0).supers.toSeq, Seq(
      gref("X", Int32),
      gref("Y", gref("X", gref("X", Int32)), gref("X", Float64))
    ))
    checkIndexedSignatureTypes(constraints(1).supers.toSeq, Seq(
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 0, 1),
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 1, 0),
    ))
    checkIndexedSignatureTypes(constraints(2).supers.toSeq, Seq(
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 0, 1),
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 1, 0),
    ))
    checkIndexedSignatureTypes(constraints(2).supers.toSeq, Seq(
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 0, 1),
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 1, 0),
    ))
    checkIndexedSignatureTypes(constraints(3).supers.toSeq, Seq(
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 0, 1),
      gtt(gref("Y", BuiltinSignature.Nil, BuiltinSignature.Nil), 1, 1),
    ))
    checkIndexedSignatureTypes(constraints(4).supers.toSeq, Seq(
      gtt(gref("C", BuiltinSignature.Nil), 0),
    ))
    checkIndexedSignatureTypes(constraints(5).supers.toSeq, Seq(
      gtt(gref("D", grec("SS", BuiltinSignature.Nil)), 0),
    ))
  }

  test("test signature write unfrozen index") {
    val (gen, _) = SignatureSandbox.prepareEnv()

    import SignatureSandbox.*
    import gen.sigIndex

    sigIndex.indexFromMetadata(rec())

    a[AssertionError] should be thrownBy {
      sigIndex.writeSignatures()
    }
  }

  test("test signature fill frozen index") {
    val (gen, _) = SignatureSandbox.prepareEnv()

    import SignatureSandbox.*
    import gen.sigIndex

    sigIndex.indexFromMetadata(rec())
    sigIndex.freeze()

    a[AssertionError] should be thrownBy {
      sigIndex.indexFromMetadata(ref())
    }
  }

  test("test signature freeze indices twice") {
    val (gen, _) = SignatureSandbox.prepareEnv()

    import SignatureSandbox.*
    import gen.sigIndex

    sigIndex.indexFromMetadata(rec())
    sigIndex.freeze()

    a[AssertionError] should be thrownBy {
      sigIndex.freeze()
    }
  }

  test("test signature code sig index does not fit to uint16") {
    val (gen, _) = SignatureSandbox.prepareEnv()

    import SignatureSandbox.*
    import gen.sigIndex

    for (i <- sigIndex.MAX_BUILTIN_SIG_INDEX to sigIndex.MAX_SIG_INDEX_FROM_CODE) {
      sigIndex.indexFromCode(MethodType(m()(en(Int32, "Enum " + i))))
    }

    a[AssertionError] should be thrownBy {
      sigIndex.freeze()
    }
  }

  test("test signature code sig index fits to uint16") {
    val (gen, _) = SignatureSandbox.prepareEnv()

    import SignatureSandbox.*
    import gen.sigIndex

    for (i <- sigIndex.MAX_BUILTIN_SIG_INDEX until sigIndex.MAX_SIG_INDEX_FROM_CODE) {
      sigIndex.indexFromCode(MethodType(m()(en(Int32, "Enum " + i))))
    }

    sigIndex.freeze()
  }

  test("test make constraint type name") {
    val (gen, _) = SignatureSandbox.prepareEnv()

    import SignatureSandbox.*
    import gen.sigIndex.*
    import gen.sigIndex

    def constr(rawSupers: SignatureType*) = {
      sigIndex.Constraint(mutable.LinkedHashSet.from(rawSupers))
    }
    def pconstr(rawSupers: mutable.LinkedHashSet[SignatureType], freeVariables: LocalTypeVariable*) = {
      ParameterizedConstraint(rawSupers, mutable.LinkedHashSet.from(freeVariables))
    }

    constr(
      iref("X", iref("X", nref("C"))),
      nref("C"),
    ).name shouldBe "$CX_0_CONDESC"
    constr(
      nref("C"),
      iref("X", iref("X", nref("C"))),
    ).name shouldBe "$CX_1_CONDESC"
    pconstr(mutable.LinkedHashSet(
      nref("C"),
      iref("X", iref("X", tv(0)))
    ), tv(0)).name shouldBe "$CX_2_CONDESC"
    pconstr(mutable.LinkedHashSet(
      nref("D"),
      iref("X", iref("X", tv(0)))
    ), tv(0)).name shouldBe "$DX_0_CONDESC"
  }

  test("collect free variables indices") {
    val (gen, _) = SignatureSandbox.prepareEnv()

    def assertContain(freeVariables: Seq[LocalTypeVariable], sig: SignatureType*): Unit = {
      gen.collectFreeVariables(sig) shouldBe freeVariables
    }
    def assertNotContain(sig: SignatureType*): Unit = {
      gen.collectFreeVariables(sig) shouldBe empty
    }

    import SignatureSandbox.*

    /**
      * interface I<T, U> {}
      * interface J<T> {}
      */

    // T <: I<I<Int32, I<Ref, I<Float32, Float64>>>, Int64> & J<J<Float64>>
    assertNotContain(
      iref("X",
        iref("X",
          Int32,
          iref("X",
            nref(),
            iref("X",
              Float32,
              Float64
            )
          )
        ),
        Int64
      ),
      iref("Y",
        iref("Y",
          Float64
        )
      ),
    )
    // T <: I<I<Int32, I<T, I<Float32, Float64>>>, Int64>
    assertContain(
      Seq(tv(0)),
      iref("X",
        iref("X",
          Int32,
          iref("X",
            tv(0),
            iref("X",
              Float32,
              Float64
            )
          )
        ),
        Int64
      )
    )
    // T <: I<I<Int32, I<Ref, I<U, Float64>>>, Int64> & J<J<U>>
    assertContain(
      Seq(tv(1), tv(1)),
      iref("X",
        iref("X",
          Int32,
          iref("X",
            nref(),
            iref("X",
              tv(1),
              Float64
            )
          )
        ),
        Int64
      ),
      iref("Y",
        iref("Y",
          tv(1)
        )
      ),
    )

    assertContain(
      Seq(tv(0), tv(0)),
      iref("X",
        iref("Y", tv(0)),
        iref("Y", tv(0))
      )
    )
  }
}

object LegacyCBCFileGeneratorSuite {

  def setRTConsts(env: FakeEnvironment): Unit = {
    val resolver = env.rtConstResolver

    resolver.setIntValue(RTConst.TypeTag.NOTHING, 0)
    resolver.setIntValue(RTConst.MethodTag.NOTHING, 0)
    resolver.setIntValue(RTConst.FieldTag.NOTHING, 0)

    resolver.setIntValue(RTConst.TypeTag.GENERIC_PARAMETERS, 5)
    resolver.setIntValue(RTConst.MethodTag.GENERIC_PARAMETERS, 5)

    resolver.setIntValue(RTConst.TypeTag.GENERIC_CONSTRAINTS, 6)
    resolver.setIntValue(RTConst.MethodTag.GENERIC_CONSTRAINTS, 6)
  }
}
