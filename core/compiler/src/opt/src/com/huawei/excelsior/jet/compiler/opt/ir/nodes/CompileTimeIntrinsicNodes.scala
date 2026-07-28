/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.types.CompileTimeComputations
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.condOpt

/** Implementation of runtime's CompileTimeComputable intrinsics.
  *
  * Usages typically have the following structure:
  *
  * {{{
  *   if (ComputableAtCompileTime.calcSmth(key, args) {
  *     CompileTime.calcSmth(key, args)
  *     ...
  *   }
  * }}}
  *
  * At compile time we try to calculate asked property of obj.
  * If we succeed, condition is replaced by "true" and body is replaced by calculated value.
  * Otherwise we replace condition by "false" during lowering and all compile-time-stuff disappears.
  *
  * @see [[CompileTimeComputations]]
  */
trait CompileTimeIntrinsicNodes { self: Universe with Nodes =>

  sealed abstract class CompileTimeOp protected(proto: CompileTimeOp.Proto[_ <: CompileTimeOp]) extends FloatingNodeWithFixedArgs(proto) {
    def kind = proto.kind
  }

  object CompileTimeOp {
    sealed abstract class Proto[N <: CompileTimeOp](_kind: Kind)(argTypes: Type*)(resultTpe: Type)
      extends FixedArgs[N](argTypes ++ _kind.argTypes: _*)(resultTpe) {

      def kind: Kind
    }

    enum Kind {
      case IsNull
      case HasTraceableFields
      case IsArray
      case GetReferenceArrayElementFormalType
      case GetComponentType
      case GetArrayDimNum
      case GetArrayElemLog2Size
      case HasPrimitiveArrayBaseType
      case IsArrayClass
      case IsInstance
      case IsAssignable

      def argTypes: Seq[Type] = this match {
        case IsNull | HasTraceableFields | IsArray | GetReferenceArrayElementFormalType | GetComponentType | IsArrayClass => Seq(TRefType)
        case IsInstance | IsAssignable => Seq(TRefType, TRefType)
        case GetArrayDimNum | GetArrayElemLog2Size | HasPrimitiveArrayBaseType => Seq(AddrType)
      }

      def retType: Type = this match {
        case IsNull | HasTraceableFields | IsArray | IsArrayClass | IsInstance | IsAssignable | HasPrimitiveArrayBaseType => ConditionType
        case GetReferenceArrayElementFormalType => AddrType
        case GetComponentType => TRefType
        case GetArrayDimNum | GetArrayElemLog2Size => IntType
      }
    }
  }

  class IsComputableAtCompileTime private(proto: IsComputableAtCompileTime.Proto)
    extends CompileTimeOp(proto) with ControlledNode with CompositeNode

  object IsComputableAtCompileTime {
    case class Proto(kind: CompileTimeOp.Kind)
      extends CompileTimeOp.Proto[IsComputableAtCompileTime](kind)(ControlType)(ConditionType) {

      def newInstance() = new IsComputableAtCompileTime(this)
    }

    def apply(kind: CompileTimeOp.Kind) = Prototype.intern(Proto(kind))
    def unapply(n: IsComputableAtCompileTime) = Some(n.kind)
  }

  class ComputeAtCompileTime private(proto: ComputeAtCompileTime.Proto)
    extends CompileTimeOp(proto) with ControlledNode with CompositeNode

  object ComputeAtCompileTime {
    case class Proto(kind: CompileTimeOp.Kind)
      extends CompileTimeOp.Proto[ComputeAtCompileTime](kind)(ControlType)(kind.retType) {

      def newInstance() = new ComputeAtCompileTime(this)
    }

    def apply(kind: CompileTimeOp.Kind) = Prototype.intern(Proto(kind))
    def unapply(n: ComputeAtCompileTime) = Some(n.kind)
  }

}
