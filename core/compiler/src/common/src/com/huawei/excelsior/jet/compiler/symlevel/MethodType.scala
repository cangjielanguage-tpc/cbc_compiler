/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.symlevel

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.assembler.Symbol
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.Env.isStandalone
import com.huawei.excelsior.jet.compiler.TypeProvider
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.MANAGED
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.*
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.AltLocationInfo.NoAltLocation
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParamSet.Position.{Custom, End, Start}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParamSet.{Position, completeListOfEndSpecialParameters, completeListOfStartSpecialParameters}
import com.huawei.excelsior.jet.compiler.symlevel.MethodType.SpecialParameter.*
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType.{JavaArray, fromSymType}
import com.huawei.excelsior.jet.compiler.verifier.VerifiableMethodType
import com.huawei.excelsior.jet.util.ScalaCollections
import xscala.util.MathUtils

import scala.collection.immutable
import scala.reflect.ClassTag


/** A method type contains all information needed to generate a method call,
  * except actual method reference, which is located in [[MethodReference]].
  *
  * The structure is a return type accompanied by any number of parameter types.
  * The types (primitive, void, and reference) are represented by [[SignatureType]] objects.
  * Plus, it contains [[CallConv]] used to invoke associated method
  * and an indicator of the ability of this method to accept AJ var. args.
  *
  * All instances of [[MethodType]] are immutable.
  * This type is originally created either by the constructor of associated [[Method]]
  * or from all known properties via call to [[methodType]] or [[fromMethodDescriptorString]].
  * Afterwards it can be transformed by virtual methods which
  * modify precursor method types (e.g., by changing a selected parameter) producing new ones.
  *
  * Public interface of this class inspired by [[java.lang.invoke.MethodType]]
  * but does not follow it precisely.
  *
  * @author ijorch
  */
object MethodType {
  val UNINITIALIZED_FIRST_VAR_ARG = -1

  case class AltLocationInfo private(methodHasAltLocationResult: Boolean, altLocationParameterMask: Int) {
    def isAltLocationParam(i: Int): Boolean = (altLocationParameterMask & (1 << i)) != 0
    def dropFirstNParameters(n: Int): AltLocationInfo = this.copy(altLocationParameterMask = altLocationParameterMask >>> n)
    def dropParameter(n: Int): AltLocationInfo = this.copy(altLocationParameterMask = dropParameterFromMask(altLocationParameterMask, n))

    def nonEmpty = methodHasAltLocationResult || altLocationParameterMask != 0
  }

  object AltLocationInfo {
    def apply(methodHasAltLocationResult: Boolean, altLocationParameterMask: Int) = if (!methodHasAltLocationResult && altLocationParameterMask == 0) {
      NoAltLocation
    } else {
      new AltLocationInfo(methodHasAltLocationResult, altLocationParameterMask)
    }

    /** Instance of [[AltLocationInfo]] describing the situation when neither method parameters nor method itself
      * are annotated with `@CallConv.AltLocation`.
      */
    val NoAltLocation: AltLocationInfo = new AltLocationInfo(methodHasAltLocationResult = false, altLocationParameterMask = 0)
  }

  /** Creates an instance of a method type from given method signature without resolve procedure. */
  def jbcErased(sig: XString, tp: TypeProvider, hasReceiver: Boolean): MethodType = {
    val msig = tp.eraseMethodSignature(sig)
    apply(msig, CallConv.MANAGED, CallKind.NORMAL, SpecialParamSet(), isVarArgs = false)
      .insertReceiverType(fromSymType(tp.getObjectType), hasReceiver)
  }

  /** Creates an instance of a method type from given method signature.
    * Types from the signature are resolved by type provider using given refType or erased if `refType` is null.
    *
    * TODO-DECAF: use default parameters
    */
  def forJava(sig: XString, tp: TypeProvider, refType: ClassType): MethodType = {
    forJava(sig, tp, refType, MANAGED, CallKind.NORMAL, isVarArgs = false)
  }

  /** Creates an instance of a method type from given method signature.
    * Types from the signature are resolved by type provider using given refType or erased if `refType` is null.
    *
    * TODO-DECAF: use default parameters
    */
  def forJava(sig: XString, tp: TypeProvider, refType: ClassType, callConv: CallConv, callKind: CallKind, isVarArgs: Boolean): MethodType = {
    val msig = tp.resolveMethodSignature(sig, refType)
    apply(msig, callConv, callKind, SpecialParamSet(), isVarArgs)
  }

  /** Creates an instance of a method type from given method signature. */
  def apply(sig: MethodSignature): MethodType = {
    apply(sig, SpecialParamSet())
  }

  /** Creates an instance of a method type from given method signature and special params. */
  def apply(sig: MethodSignature, specialParams: SpecialParamSet): MethodType = {
    apply(sig, CallConv.MANAGED, CallKind.NORMAL, specialParams, isVarArgs = false)
  }

  /** Creates an instance of a method type from given method signature. */
  def apply(sig: MethodSignature, callConv: CallConv, callKind: CallKind, specialParams: SpecialParamSet, isVarArgs: Boolean): MethodType = {
    apply(sig, callConv, callKind, specialParams, isVarArgs, UNINITIALIZED_FIRST_VAR_ARG)
  }

  /** Creates an instance of a method type, given all its properties. */
  def apply(sig: MethodSignature, callConv: CallConv, callKind: CallKind, specialParams: SpecialParamSet,
            isVarArgs: Boolean, firstVarArg: Int, headInLimit: Int = Int.MaxValue, headOutLimit: Int = 1,
            preservedParameterMask: Int = 0, altLocationInfo: AltLocationInfo = NoAltLocation): MethodType =
    new MethodType(sig, callConv, callKind, specialParams, isVarArgs, firstVarArg,
      headInLimit, headOutLimit, preservedParameterMask, altLocationInfo)

  def asVerifiableMethodType(impl: MethodType): VerifiableMethodType = new VerifiableMethodType {
    override def parameterCount = impl.parameterCount
    override def parameterTypeKind(i: Int) = impl.parameterType(i).jbcKindErased
    override def returnTypeKind = impl.returnType.jbcKindErased
  }

  def dropParameterFromMask(mask: Int, n: Int): Int = {
    val bit = MathUtils.nthBit32(n)
    val cleared = mask & ~bit

    val belowMask = bit - 1
    val aboveMask = ~belowMask

    val belowBits = cleared & belowMask
    val aboveBits = cleared & aboveMask
    (aboveBits >> 1) | belowBits
  }

  /** Special parameters are inserted by the compiler for implementation purpose but are not present in source-level function declarations.
    * Has runtime implementation as [[com.huawei.excelsior.jet.runtime.jit.cbc.representation.CbcABI.SpecialParameter]]
    */
  enum SpecialParameter(val position: Position) {
    // should preserve the same order as they appear in method parameter types
    case RetByVal extends SpecialParameter(Start)
    case MutRecord extends SpecialParameter(Start)
    case MutObject extends SpecialParameter(Start)
    case SMutRecord extends SpecialParameter(Start)
    case SMutObject extends SpecialParameter(Start)
    case Receiver extends SpecialParameter(Start)
    case GenericFuncParams extends SpecialParameter(Custom)
    case UGDesc extends SpecialParameter(End)
    case OuterTypeInfo extends SpecialParameter(End)
    case ThisTypeInfo extends SpecialParameter(End)
    case CFuncRetByVal extends SpecialParameter(End)
  }

  trait SpecialParamSet {
    def addElement(param: SpecialParameter): SpecialParamSet
    def getElementIndex(param: SpecialParameter): Int
    def elements: Iterable[SpecialParameter]
    def contains(specialParameter: SpecialParameter): Boolean
    def toBitSet: immutable.BitSet

    def specialParametersStart = completeListOfStartSpecialParameters.iterator.filter(contains)
    def specialParametersEnd = completeListOfEndSpecialParameters.iterator.filter(contains)
  }

  object SpecialParamSet {

    enum Position {
      case Start
      case Custom
      case End
    }

    val completeListOfStartSpecialParameters = SpecialParameter.values.filter(_.position == Start)
    val completeListOfEndSpecialParameters = SpecialParameter.values.filter(_.position == End)

    def apply(): SpecialParamSet = {
      apply(Iterable.empty)
    }

    /** Use this method for creating new SpecialParamSet objects. */
    def apply(elements: Iterable[SpecialParameter]): SpecialParamSet = {
      OnBitSet(elements)
    }

    def apply(elements: SpecialParameter*): SpecialParamSet = {
      apply(elements)
    }

    def fromBitSet(bitSet: immutable.BitSet): SpecialParamSet = {
      new OnBitSet(bitSet)
    }

    private object OnBitSet {
      def apply(elements: Iterable[SpecialParameter]): SpecialParamSet = {
        new OnBitSet(immutable.BitSet.fromSpecific(elements.map(_.ordinal)))
      }
    }

    /** Implementation based on BitSet. */
    private case class OnBitSet(paramBitSet: immutable.BitSet) extends SpecialParamSet {
      override def toBitSet = paramBitSet

      override def addElement(param: SpecialParameter): SpecialParamSet = {
        // only one special parameter for every type is allowed
        assert(!contains(param))
        OnBitSet(paramBitSet + param.ordinal)
      }

      override def elements: Iterable[SpecialParameter] = paramBitSet.toSeq.map(SpecialParameter.fromOrdinal)

      override def contains(specialParameter: SpecialParameter): Boolean = paramBitSet.contains(specialParameter.ordinal)

      override def getElementIndex(param: SpecialParameter): Int = {
        assert(contains(param))
        val index = paramBitSet.iterator.indexOf(param.ordinal)
        param.position match {
          case Start => index
          case End => paramBitSet.size - index - 1
          case Custom => shouldNotReachHere(param)
        }
      }
    }
  }
}

case class MethodType (signature: MethodSignature, callConv: CallConv,
                       callKind: CallKind, specialParameters: SpecialParamSet, isVarArgs: Boolean, firstVarArg: Int,
                       headInLimit: Int, headOutLimit: Int,
                       preservedParameterMask: Int, altLocationInfo: AltLocationInfo) extends Symbol {

  def returnType: SignatureType = signature.returnType

  private def hasSpecialArg(specialParameter: SpecialParameter): Boolean = specialParameters.contains(specialParameter)

  private def getSpecialArgIdx(specialParameter: SpecialParameter): Int = specialParameter.position match {
    case Start => specialParameters.getElementIndex(specialParameter)
    case End => parameterCount - specialParameters.getElementIndex(specialParameter) - 1
    case Custom => (specialParameter: @unchecked) match {
      case GenericFuncParams => startSpecialParamsCount + signature.parameterTypes.size
    }
  }

  def startSpecialParamsCount: Int = specialParameters.specialParametersStart.size

  def getReceiverArgIdx: Int = getSpecialArgIdx(Receiver)

  def getMutObjectArgIdx: Int = getSpecialArgIdx(if (isStandalone) SMutObject else MutObject)

  def getMutRecordArgIdx: Int = getSpecialArgIdx(if (isStandalone) SMutRecord else MutRecord)

  def getUGDescArgIdx: Int = getSpecialArgIdx(UGDesc)

  def getGenericFuncParamsStartIdx: Int = getSpecialArgIdx(GenericFuncParams)

  def getOuterTypeInfoArgIdx: Int = getSpecialArgIdx(OuterTypeInfo)

  def getThisTypeInfoArgIdx: Int = getSpecialArgIdx(ThisTypeInfo)

  def getRetByValArgIdx: Int = getSpecialArgIdx(RetByVal)

  def getCFuncRetByValArgIdx: Int = getSpecialArgIdx(CFuncRetByVal)

  /** Returns true iff MethodType has information about receiver arg. */
  def hasReceiverParameter: Boolean = hasSpecialArg(Receiver)

  /** Returns true iff MethodType has information about mut_object arg. */
  def hasMutObjectParameter: Boolean = hasSpecialArg(if (isStandalone) SMutObject else MutObject)

  /** Returns true iff MethodType has information about mut_record arg. */
  def hasMutRecordParameter: Boolean = hasSpecialArg(if (isStandalone) SMutRecord else MutRecord)

  /** Returns true iff MethodType has information about ug_desc arg. */
  def hasUGDescParameter: Boolean = hasSpecialArg(UGDesc)

  def hasGenericFuncParams: Boolean = hasSpecialArg(GenericFuncParams)

  def hasOuterTypeInfoParameter: Boolean = hasSpecialArg(OuterTypeInfo)

  /** Returns true iff MethodType has information about this_type_info arg. */
  def hasThisTypeInfoParameter: Boolean = hasSpecialArg(ThisTypeInfo)

  /** Returns true iff MethodType has information about ret_by_val arg. */
  def hasRetByValParameter: Boolean = hasSpecialArg(RetByVal)

  /** Returns true iff MethodType has information about c_func_ret_by_val arg. */
  def hasCFuncRetByValParameter: Boolean = hasSpecialArg(CFuncRetByVal)

  /** Creates a method type with a different state of varArgs flag. */
  def changeVarArgsFlag(isVarArgs: Boolean) = copy(isVarArgs = isVarArgs)

  /** Creates a method type with a different calling convention. */
  def changeCallConv(callConv: CallConv) = copy(callConv = callConv)

  /** Creates a method type with a different calling kind. */
  def changeCallKind(callKind: CallKind) = copy(callKind = callKind)

  /** Creates a method type with a different return type. */
  def changeReturnType(returnType: SignatureType) = copy(signature = signature.copy(returnType = returnType))

  /** Creates a method type with a different head in and out limits. */
  def changeHeadLimits(headInLimit: Int, headOutLimit: Int) = copy(headInLimit = headInLimit, headOutLimit = headOutLimit)

  /** Creates a method type with a different preserved parameter mask. */
  def changePreservedParameterMask(preservedParameterMask: Int) = copy(preservedParameterMask = preservedParameterMask)

  /** Creates a method type with a different set of parameters. Used in unit-tests. */
  def changeParameters(newParams: Seq[SignatureType]): MethodType = changeParameters(newParams, specialParameters)

  /** Creates a method type with a different set of parameters and special parameters. */
  def changeParameters(newParams: Seq[SignatureType], newSpecialParams: SpecialParamSet) = copy(
    signature = signature.copy(parameterTypes = newParams),
    specialParameters = newSpecialParams,
    preservedParameterMask = preservedParameterMask ensuring (_ == 0),
    altLocationInfo = altLocationInfo ensuring (_ == NoAltLocation)
  )

  /** Creates a method type with additional parameter type at the end. */
  def appendParameterType(additionalParam: SignatureType) = {
    assert(!isVarArgs)
    changeParameters(parameterTypes.toSeq :+ additionalParam)
  }

  /** Creates a method type with additional heading parameter type. */
  def prependParameterType(additionalParam: SignatureType) = {
    assert(!areVarArgsInitialized)
    changeParameters(additionalParam +: parameterTypes.toSeq)
  }

  /** Creates a method type with [[receiverType]] if [[shouldHaveReceiver]].
    */
  def insertReceiverType(receiverType: SignatureType, shouldHaveReceiver: Boolean) = {
    assert(!hasReceiverParameter)
    assert(!areVarArgsInitialized)

    if (shouldHaveReceiver) {
      val receiverIdx = if (hasRetByValParameter) 1 else 0
      val newSpecialParams = specialParameters.addElement(Receiver)
      changeParameters(ScalaCollections.insertAt(parameterTypes, receiverIdx, receiverType).toSeq, newSpecialParams)
    } else {
      this
    }
  }

  /** Creates a method type without receiver. */
  def dropReceiverParameter = if (!hasReceiverParameter) this else dropParameter(getReceiverArgIdx)

  /** Creates a method type without parameter with index `n`. */
  private def dropParameter(n: Int): MethodType = {
    val paramsCount = parameterCount
    assert(paramsCount > n)

    val newParams = ScalaCollections.removeAt(signature.parameterTypes, n).toSeq
    val newSpecialParams = SpecialParamSet(specialParameters.elements.filter(getSpecialArgIdx(_) != n))

    if (!areVarArgsInitialized && preservedParameterMask == 0 && altLocationInfo == NoAltLocation) {
      changeParameters(newParams, newSpecialParams)
    } else {
      copy(signature = signature.copy(parameterTypes = newParams),
        specialParameters = newSpecialParams,
        firstVarArg = if (n <= firstVarArg) firstVarArg - 1 else firstVarArg,
        preservedParameterMask = dropParameterFromMask(preservedParameterMask, n),
        altLocationInfo = altLocationInfo dropParameter n)
    }
  }

  /** Creates a method type without leading `n` parameters. */
  def dropFirstNParameters(n: Int): MethodType = {
    if (n == 0) return this

    val paramsCount = parameterCount
    assert(paramsCount >= n)

    val newParams = parameterTypes(n).toSeq
    val newSpecialParams = SpecialParamSet(specialParameters.elements.filter(getSpecialArgIdx(_) >= n))

    if (!areVarArgsInitialized && preservedParameterMask == 0 && altLocationInfo == NoAltLocation) {
      changeParameters(newParams, newSpecialParams)
    } else {
      copy(signature = signature.copy(parameterTypes = newParams),
        specialParameters = newSpecialParams,
        firstVarArg = firstVarArg - n,
        preservedParameterMask = preservedParameterMask >>> n,
        altLocationInfo = altLocationInfo dropFirstNParameters n)
    }
  }

  /** Creates a method type without tailing parameter. */
  def dropLastParameter = {
    assert(!areVarArgsInitialized)
    copy(signature = signature.copy(parameterTypes = signature.parameterTypes.init),
      specialParameters = SpecialParamSet(specialParameters.elements.filter(getSpecialArgIdx(_) != parameterCount - 1)))
  }

  def areVarArgsInitialized = firstVarArg != UNINITIALIZED_FIRST_VAR_ARG

  /** Creates a method type with specific var.args parameter types instead of last array. */
  def appendVarArgs(varArgs: Iterable[SignatureType]): MethodType = {
    if (!this.isVarArgs) return this
    assert(!areVarArgsInitialized)

    val lastParam = parameterType(parameterCount - 1)
    assert(lastParam.isInstanceOf[JavaArray])

    var newParams = dropLastParameter.parameterTypes.toSeq

    if (varArgs != null) {
      newParams ++= varArgs
    }

    copy(signature.copy(parameterTypes = newParams), firstVarArg = parameterCount - 1)
  }

  /** Checks whether the parameter at position `i` belongs to AJ var. args. */
  def isVarArgParam(i: Int) = isVarArgs && i >= firstVarArg

  /** Presents the parameter types as an iterable. */
  private def parameterTypes(startParam: Int): Iterator[SignatureType] = signature.parameterTypes.iterator.drop(startParam)

  /** Presents the types of AJ var. args.
    * Should be called only for method with AJ var. args.
    */
  def varArgTypes: Iterator[SignatureType] = {
    assert(isVarArgs && areVarArgsInitialized)
    parameterTypes(firstVarArg)
  }

  /** Presents the parameter types as an iterable. */
  def parameterTypes: Iterator[SignatureType] = parameterTypes(0)

  def parameterCount: Int = signature.parameterTypes.size

  lazy val parameterSlotCount = parameterTypes.map(_.jbcKindErased.nslots).sum

  def parameterType(i: Int): SignatureType = signature.parameterTypes(i)

  def isAJLongSafe = callKind == CallKind.AJ_LONG_SAFE

  def isCJForeign = callKind == CallKind.CJ_FOREIGN

  def isReceiverParameter(i: Int) = hasReceiverParameter && getReceiverArgIdx == i

  def isCVarArgs = isVarArgs && !callConv.isJET
  def isJETVarArgs = isVarArgs && callConv.isJET

  def hasPreservedParameters = preservedParameterMask != 0
  def isPreservedParameter(i: Int) = (i < parameterCount) && ((preservedParameterMask & (1 << i)) != 0)

  def isAltLocationParameter(i: Int) = (i < parameterCount) && altLocationInfo.isAltLocationParam(i)

  def toMethodDescriptor: MethodSignature = {
    if (hasReceiverParameter) {
      val newParams = ScalaCollections.removeAt(parameterTypes, getReceiverArgIdx)
      signature.copy(parameterTypes = newParams.toSeq)
    } else {
      signature
    }
  }

  override def toString = toMethodDescriptor.toJETSignature
}
