/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.jet.compiler.abi.DAIGenerator
import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.common.DAIRefKind
import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind
import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Universe}
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.STATIC
import com.huawei.excelsior.jet.compiler.symlevel.{BytecodeMethodReference, MethodReference, MethodType, SigPolyMethodID, SignatureType, MethodReferenceAccessKind as AccessKind, Type as SymType}

trait DeferredNodes { self: Universe with Nodes =>

  class Deferred private(proto: Deferred.Proto)
    extends NodeWithFixedArgs(proto) with CompositeNode with SpinalMemoryNode with CanThrow with ProducesValue {

    private def firstExtraArg = 2

    def extraArgs: Seq[Node] = argsTail(firstExtraArg)

    override def name = s"${super.name}[${proto.simpleName}]"

    private[ir] final override def argEnrichment(argEdge: Edge): Option[SymType] = {
      val argIdx = argEdge.targetArgIndex
      if (argIdx >= firstExtraArg) proto.extraArgEnrichment(argIdx - firstExtraArg) else None
    }
  }

  object Deferred {

    ///////////////////// Common ///////////////////////

    sealed abstract class Proto private[Deferred] (argTypes: Type*)(retType: ValueType)
      extends FixedArgs[Deferred]((Seq(ControlType, MemoryType) ++ argTypes)*)(retType)
        with ControlMemoryValueTagged[Deferred] {

      def newInstance() = new Deferred(this)

      def simpleName: String
      def extraArgEnrichment(extraArgIdx: Int): Option[SymType]
    }

    trait Unresolved extends Proto {
      def cpIndex: Int
    }

    object Unresolved {
      sealed abstract class Proto private[Deferred] (cpIndex: Int)(argTypes: Type*)(retType: ValueType)
        extends Deferred.Proto(argTypes*)(retType)
    }

    ///////////////////// New ///////////////////////

    case object New {
      case class Proto private[Deferred] (cpIndex: Int) extends Deferred.Proto()(TRefType) with Unresolved {
        def simpleName = New.toString
        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] = shouldNotReachHere(extraArgIdx)
      }

      def apply(cpIndex: Int) = Prototype.intern(Proto(cpIndex))
    }

    case object NewArray {
      case class Proto private[Deferred] (cpIndex: Int, allDimNum: Int, dimSpec: Int)
        extends Deferred.Proto(Seq.fill(dimSpec)(IntType)*)(TRefType) with Unresolved {

        def simpleName = NewArray.toString
        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] = None
      }

      def apply(cpIndex: Int, allDimNum: Int, dimSpec: Int) = Prototype.intern(Proto(cpIndex, allDimNum, dimSpec))
    }

    ///////////////////// Type checks ///////////////////////

    case object InstanceOf {
      case class Proto private[Deferred] (cpIndex: Int) extends Deferred.Proto(TRefType)(IntType) with Unresolved {
        def simpleName = InstanceOf.toString
        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] = None
      }

      def apply(cpIndex: Int) = Prototype.intern(Proto(cpIndex))
    }

    case object CheckCast {
      case class Proto private[Deferred] (cpIndex: Int) extends Deferred.Proto(TRefType)(IntType) with Unresolved {
        def simpleName = CheckCast.toString
        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] = None
      }

      def apply(cpIndex: Int) = Prototype.intern(Proto(cpIndex))
    }

    ///////////////////// Misc ///////////////////////

    case object ClassObject {
      case class Proto private[Deferred] (cpIndex: Int) extends Deferred.Proto()(TRefType) with Unresolved {
        def simpleName = ClassObject.toString
        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] = shouldNotReachHere(extraArgIdx)
      }

      def apply(cpIndex: Int) = Prototype.intern(Proto(cpIndex))
    }

    case object MethodHandle {
      case class Proto private[Deferred](cpIndex: Int) extends Deferred.Proto()(TRefType) with Unresolved {
        def simpleName = MethodHandle.toString
        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] = shouldNotReachHere(extraArgIdx)
      }

      def apply(cpIndex: Int) = Prototype.intern(Proto(cpIndex))
    }

    case object MethodType {
      case class Proto private[Deferred](cpIndex: Int) extends Deferred.Proto()(TRefType) with Unresolved {
        def simpleName = MethodType.toString
        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] = shouldNotReachHere(extraArgIdx)
      }

      def apply(cpIndex: Int) = Prototype.intern(Proto(cpIndex))
    }

    ///////////////////// Field ///////////////////////

    case object FieldOp {
      case class Proto private[Deferred] (cpIndex: Int, fieldType: SignatureType, isWrite: Boolean, isStatic: Boolean)
        extends Deferred.Proto(makeArgs(TRefType, ValueType.fromSig(fieldType, instantiateRich = true), isWrite, isStatic)*)(retType(fieldType, isWrite))
          with Unresolved {
        assert(!fieldType.isZST)

        def simpleName = FieldOp.toString

        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] = {
          val symType = fieldType.symType
          val enrichments = makeArgs(None, if (symType.isInterface && !symType.isDeferred) Some(symType) else None)
          enrichments(extraArgIdx)
        }

        /** Note that extraArgs are ordered in special order convenient for runtime.
          * @see [[DAIGenerator.FieldAccessParametersOrdering]]
          */
        def makeArgs[A](obj: A, value: A): Seq[A] = {
          FieldOp.makeArgs(obj, value, isWrite, isStatic)
        }

        /** @see [[makeArgs]] */
        def objArg[A >: Null](args: Seq[A]): A = {
          DAIGenerator.FieldAccessParametersOrdering.getObject(args, isWrite, isStatic)
        }
      }

      /** Note that extraArgs are ordered in special order convenient for runtime.
        * @see [[DAIGenerator.FieldAccessParametersOrdering]]
        */
      def makeArgs[A](obj: A, value: A, isWrite: Boolean, isStatic: Boolean): Seq[A] = {
        DAIGenerator.FieldAccessParametersOrdering.forMethodInvocation(obj, value, isWrite, isStatic)
      }

      private def retType(fieldType: SignatureType, isWrite: Boolean) = if (isWrite) VoidType else ValueType.fromSig(fieldType, instantiateRich = true)

      def apply(cpIndex: Int, fieldType: SignatureType, isWrite: Boolean, isStatic: Boolean) =
        Prototype.intern(Proto(cpIndex, fieldType, isWrite, isStatic))
    }

    ///////////////////// Invoke ///////////////////////

    sealed trait Invoke extends Proto {
      def akind: AccessKind
      def targetRef: MethodReference
    }

    case object Invoke {
      sealed abstract class Proto private[Deferred] (_targetRef: MethodReference)
        extends Deferred.Proto(argTypes(_targetRef)*)(retType(_targetRef)) {

        def targetRef: MethodReference

        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] =
          methodParamEnrichment(targetRef.methodType, extraArgIdx).toOption
      }

      object Proto {
        def unapply(x: Proto) = Some(x.targetRef)
      }

      private def argTypes(targetRef: MethodReference) = {
        val methodType = targetRef.methodType
        methodType.parameterTypes.zipWithIndex.map { case (t, idx) =>
          ValueType.fromSig(t, instantiateRich = !methodType.isReceiverParameter(idx))
        }.toSeq
      }

      private def retType(targetRef: MethodReference) = ValueType.fromSig(targetRef.methodType.returnType, instantiateRich = true)
    }

    case object UnresolvedInvoke {
      case class Proto private[Deferred] (cpIndex: Int, targetRef: MethodReference)
        extends Invoke.Proto(targetRef) with Unresolved {
        def simpleName = UnresolvedInvoke.toString
      }

      def apply(cpIndex: Int, targetRef: MethodReference) =
        Prototype.intern(Proto(cpIndex, targetRef))
    }

    case object JSR292Invoke {
      sealed abstract class Proto private[Deferred](methodType: MethodType)
        extends Deferred.Proto(argTypes(methodType) *)(retType(methodType)) {
        def extraArgEnrichment(extraArgIdx: Int): Option[SymType] =
          methodParamEnrichment(methodType, extraArgIdx).toOption
      }

      private def argTypes(methodType: MethodType) = {
        methodType.parameterTypes.zipWithIndex.map { case (t, idx) =>
          ValueType.fromSig(t, instantiateRich = !methodType.isReceiverParameter(idx))
        }.toSeq
      }

      private def retType(methodType: MethodType) = ValueType.fromSig(methodType.returnType, instantiateRich = true)
    }

    case object DynamicOrSigPolyInvoke {
      case class Proto private[Deferred] (cpIndex: Int, refKind: DAIRefKind, methodType: MethodType, hasAppendix: Boolean)
        extends JSR292Invoke.Proto(methodType) with Unresolved {
        def simpleName = DynamicOrSigPolyInvoke.toString
      }

      def apply(cpIndex: Int, refKind: DAIRefKind, targetRef: MethodType, hasAppendix: Boolean) =
        Prototype.intern(Proto(cpIndex, refKind, targetRef, hasAppendix))
    }

    case object SigPolyInvokeBasic {
      case class Proto private[Deferred](cpIndex: Int, methodType: MethodType)
        extends JSR292Invoke.Proto(methodType) with Unresolved {
        def simpleName = SigPolyInvokeBasic.toString
      }

      def apply(cpIndex: Int, targetRef: MethodType) =
        Prototype.intern(Proto(cpIndex, targetRef))
    }
  }
}
