/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir.nodes

import com.huawei.excelsior.jet.assembler
import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Universe}
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType

/** Set of nodes for mut-functions support in Cangjie.
  * 
  * [[MutFunc.Host]] and [[MutFunc.Offset]] used to split mutated record into the pair of host and offset.
  * [[MutFunc.HostLocal]] and [[MutFunc.HostGlobal]] used to indicate
  * whether the host is known to be local (e.g. stack allocated) or global (e.g. static).
  * [[MutFunc.Combine]] used to combine host and offset back into IntraPointer to mutated record.
  */
trait MutFuncNodes { self: Universe with Nodes =>

  trait MutFuncArgNode extends FloatingNode

  object MutFunc {
    class Host private extends LeafNode[Host](TRefType) with MutFuncArgNode with Constant

    object Host {
      def apply() = new Host()() // intentionally unique to be distinguished in pairs with Offset nodes
    }

    class HostGlobal private extends LeafNode[HostGlobal](TRefType) with MutFuncArgNode

    object HostGlobal {
      private lazy val instance = new HostGlobal

      def apply() = instance()
    }

    class HostLocal private extends LeafNode[HostLocal](TRefType) with MutFuncArgNode

    object HostLocal {
      private lazy val instance = new HostLocal

      def apply() = instance()
    }

    class Offset private(proto: Offset.Proto) extends FloatingNodeWithFixedArgs(proto) with MutFuncArgNode with ProducesValue {
      def hostArgIdx = 0
      def host = arg(hostArgIdx)

      def recordArgIdx = 1
      def record = arg(recordArgIdx)

      def recordType = proto.recordType
    }

    object Offset {
      case class Proto private[Offset](recordType: SignatureType) extends FixedArgs[Offset](TRefType, ValueType(recordType))(AddrType) {
        require(recordType.isRecord)

        def newInstance() = new Offset(this)
      }

      def proto(record: SignatureType) = Prototype.intern(Proto(record))

      def apply(recordType: SignatureType, host: Node, record: Node): Node = proto(recordType)(host, record)
      
      def unapply(n: Offset) = Some(n.host, n.record)
    }

    class Combine private(proto: Combine.Proto) extends FloatingNodeWithFixedArgs(proto) with ProducesValue {
      def hostArgIdx = 0
      def host = arg(hostArgIdx)

      def offsetArgIdx = 1
      def offset = arg(offsetArgIdx)
    }

    object Combine {
      case class Proto private[Combine](retType: Type) extends FixedArgs[Combine](TRefType, AddrType)(retType) {
        require(retType.isRecordAddrType || retType == IntraReferenceType)
        def newInstance() = new Combine(this)
      }

      def proto(retType: Type) = Prototype.intern(Proto(retType))

      def apply(host: Node, offset: Node, retType: Type): Node = proto(retType)(host, offset)

      def unapply(n: Combine) = Some(n.host, n.offset)
    }

    /** Special version of [[Offset]] used to get offset from host in CBC Interpreter/LoweringJIT by sequence of fields. */
    class OffsetCBC private(proto: OffsetCBC.Proto) extends FloatingNodeWithFixedArgs(proto) with MutFuncArgNode with ProducesValue {
      def hostArgIdx = 0
      def host = arg(hostArgIdx)

      def fields = proto.fields

      override def name = s"$simpleName${fields.mkString("[", " -> ", "]")}"
    }

    object OffsetCBC {
      case class Proto private[OffsetCBC](fields: Array[assembler.Symbol], refClassType: Type) extends FixedArgs[OffsetCBC](refClassType)(AddrType) with FieldOperationProto {
        def newInstance() = new OffsetCBC(this)
      }

      def proto(fields: List[assembler.Symbol], refClassType: Type) = Prototype.intern(Proto(Array.from(fields), refClassType))

      def apply(host: Node, refClassType: Type, fields: List[assembler.Symbol]): Node = proto(fields, refClassType)(host)

      def unapply(n: OffsetCBC) = Some(n.fields, n.host)
    }
  }
}
