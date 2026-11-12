/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.*
import org.scalacheck.Gen

/** Generators for property-based tests of SignatureType and MethodSignature.
  *
  * Only types supported by CbcSymbolAdapter.adaptSignature are generated:
  * no JBC references, Java arrays, ArraySlice, Nullable/NonNullable wrappers, CangjieEnumWrapper.
  */
object SignatureGenerators {

  private val genName: Gen[String] = Gen.oneOf("C", "D", "S")

  private val genPrimitive: Gen[SignatureType] = Gen.oneOf(SignatureType.Primitive.values.toList)

  private val genTypeVariable: Gen[SignatureType] = Gen.oneOf(
    LocalTypeVariable(0), LocalTypeVariable(1), LocalTypeVariable(2),
    ClassTypeVariable(0), ClassTypeVariable(1), ClassTypeVariable(2),
  )

  private def genLeaf: Gen[SignatureType] = Gen.oneOf(
    genPrimitive,
    Gen.const(BString: SignatureType),
    Gen.const(ThisTypeInfo: SignatureType),
    genTypeVariable,
    genName.map(name => CangjieReference(name): SignatureType),
    genName.map(name => Record(name): SignatureType),
  )

  private def genParamList(size: Int): Gen[Seq[SignatureType]] =
    Gen.chooseNum(0, 3).flatMap(Gen.listOfN(_, genSig(size)))

  private def genInstantiated(size: Int): Gen[SignatureType] = Gen.oneOf(
    for (name <- genName; params <- genParamList(size)) yield InstantiatedReference(name, params): SignatureType,
    for (name <- genName; params <- genParamList(size)) yield InstantiatedRecord(name, params): SignatureType,
  )

  private def genEnum(size: Int): Gen[SignatureType] = Gen.oneOf(
    for (name <- genName; params <- genParamList(size)) yield ZeroSizedEnum(name, params): SignatureType,
    for (name <- genName; params <- genParamList(size)) yield PrimitiveBasedEnum(name, params): SignatureType,
    for (name <- genName; params <- genParamList(size)) yield ClassBasedEnum(name, params): SignatureType,
    for (name <- genName; params <- genParamList(size)) yield UnionBasedEnum(name, params): SignatureType,
    for (name <- genName; params <- genParamList(size); someType <- genSig(size)) yield
      OptionLikeEnum(name, params, someType): SignatureType,
  )

  /** Box base must satisfy the invariant maintained by all Box producers in the pipeline:
    * a Box never wraps a plain reference (instantiateImpl collapses it), except OptionLikeEnum
    * which is kept boxed by design.
    */
  private def genBoxable(size: Int): Gen[SignatureType] =
    genSig(size).filterNot(base => base.isReference && !base.isInstanceOf[OptionLikeEnum])

  private def genComposite(size: Int): Gen[SignatureType] = Gen.oneOf(
    genSig(size - 1).map(elem => CangjieArray(elem): SignatureType),
    for (elem <- genSig(size - 1); length <- Gen.chooseNum(0L, 4L)) yield VArray(elem, length): SignatureType,
    genSig(size - 1).map(pointee => CPointer(pointee): SignatureType),
    genParamList(size).map(params => Tuple(params): SignatureType),
    genParamList(size).map(params => PackedTuple(params): SignatureType),
    genBoxable(size - 1).map(base => Box(base): SignatureType),
  )

  def genSig(size: Int): Gen[SignatureType] =
    if (size <= 0) genLeaf
    else Gen.frequency(
      6 -> genLeaf,
      2 -> genInstantiated(size),
      2 -> genComposite(size),
      1 -> genEnum(size),
    )

  def genMethodSignature(size: Int): Gen[MethodSignature] =
    for (ret <- genSig(size); params <- genParamList(size)) yield MethodSignature(ret, params)

  /** Arbitrary substitution parameters, possibly empty. */
  def genInstantiationParams: Gen[(Seq[SignatureType], Seq[SignatureType])] =
    for (cparams <- genParamList(1); lparams <- genParamList(1)) yield (cparams, lparams)

  /** Total substitution over the variable index range used by genTypeVariable (indices 0 to 2).
    * Totality is required for the composition law: partial substitutions fall back to the
    * variable itself for uncovered indices, which mapping the parameter lists loses.
    */
  def genTotalInstantiationParams: Gen[(Seq[SignatureType], Seq[SignatureType])] =
    for (cparams <- Gen.listOfN(3, genSig(1)); lparams <- Gen.listOfN(3, genSig(1))) yield (cparams, lparams)

  def identityCParams: Seq[SignatureType] = Seq.tabulate(3)(ClassTypeVariable.apply)

  def identityLParams: Seq[SignatureType] = Seq.tabulate(3)(LocalTypeVariable.apply)

  def genInstantiatedType: Gen[SignatureType.InstantiatedType] = Gen.oneOf(
    for (name <- genName; params <- genParamList(1)) yield InstantiatedReference(name, params),
    for (name <- genName; params <- genParamList(1)) yield InstantiatedRecord(name, params),
  )
}
