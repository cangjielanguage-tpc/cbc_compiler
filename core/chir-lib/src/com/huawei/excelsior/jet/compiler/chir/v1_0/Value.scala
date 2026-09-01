package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR.Func
import com.huawei.excelsior.jet.compiler.chir.v1_0.CHIRUtils.toSeq
import com.huawei.excelsior.jet.compiler.chir.v1_0.PackageFormat.{Block, BlockGroup, BoolLiteral, FloatLiteral, FuncKind, Function, GlobalVar, IntLiteral, LiteralValue, LocalVar, NullLiteral, Parameter, RuneLiteral, StringLiteral}

final class FuncImpl(f: Function, val id: Long)(using provider: CHIRItemProvider) extends CHIR.Func
  with HasAnnotationsImpl(f.base.base.base) with HasAttributesImpl(f.base.base.base.attributes) with HasDeclaringDefImpl(f.base) {

  private val gv = f.base
  private val v = gv.base

  override def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](v.`type`).get
  override def identifier: String = v.identifier
  override def srcCodeIdentifier: String = gv.srcCodeIdentifier
  override def packageName: String = gv.packageName
  override def kind: Func.Kind = f.funcKind() match {
    case FuncKind.DEFAULT => Func.Kind.Default
    case FuncKind.GETTER => Func.Kind.Getter
    case FuncKind.SETTER => Func.Kind.Setter
    case FuncKind.LAMBDA => Func.Kind.Lambda
    case FuncKind.CLASS_CONSTRUCTOR => Func.Kind.ClassCtor
    case FuncKind.PRIMAL_CLASS_CONSTRUCTOR => Func.Kind.PrimalClassCtor
    case FuncKind.STRUCT_CONSTRUCTOR => Func.Kind.StructCtor
    case FuncKind.PRIMAL_STRUCT_CONSTRUCTOR => Func.Kind.PrimalStructCtor
    case FuncKind.GLOBALVAR_INIT => Func.Kind.GlobalVarInit
    case FuncKind.FINALIZER => Func.Kind.Finalizer
    case FuncKind.MAIN_ENTRY => Func.Kind.MainEntry
    case FuncKind.ANNOFACTORY_FUNC => Func.Kind.AnnoFactory
    case FuncKind.MACRO_FUNC => Func.Kind.Macro
    case FuncKind.DEFAULT_PARAMETER_FUNC => Func.Kind.DefaultParameter
    case FuncKind.INSTANCEVAR_INIT => Func.Kind.InstanceVarInit
  }
  override def genericTypeParams: Seq[CHIR.GenericType] = {
    for (idx <- f.genericTypeParamsVector.toSeq) yield {
      provider.getType[CHIR.GenericType](idx).get
    }
  }
  override def body: Option[CHIR.BlockGroup] = provider.getValue[CHIR.BlockGroup](f.body)
  override def params: Seq[CHIR.Parameter] = {
    for (idx <- f.paramsVector.toSeq) yield {
      provider.getValue[CHIR.Parameter](idx).get
    }
  }
  override def retVal: Option[CHIR.LocalVar] = provider.getValue[CHIR.LocalVar](f.retVal)
}

class BlockGroupImpl(b: BlockGroup)(using provider: CHIRItemProvider) extends CHIR.BlockGroup {
  override def blocks: Seq[CHIR.Block] = {
    for (idx <- b.blocksVector.toSeq) yield {
      block(idx)
    }
  }
  override def entryBlock: CHIR.Block = block(b.entryBlock)
  private def block(idx: Long) = provider.getValue[CHIR.Block](idx).get
}

final class BlockImpl(b: Block)(using provider: CHIRItemProvider) extends CHIR.Block {
  override lazy val expressions: Seq[CHIR.Expression] = {
    for (idx <- b.exprsVector.toSeq) yield {
      provider.getExpr[CHIR.Expression](idx)
    }
  }
  override def nonTerminatorExpressions: Seq[CHIR.Expression] = expressions.init
  override def terminator: CHIR.Terminator = expressions.last.asInstanceOf[CHIR.Terminator]
  override def isLandingPadBlock: Boolean = b.isLandingPadBlock
}

final class GlobalVarImpl(g: GlobalVar, val id: Long)(using provider: CHIRItemProvider) extends CHIR.GlobalVar
  with HasAnnotationsImpl(g.base.base.base) with HasAttributesImpl(g.base.base.base.attributes) with HasDeclaringDefImpl(g.base) {

  private val gv = g.base
  private val v = gv.base
  private val b = v.base

  override def identifier: String = v.identifier
  override def srcCodeIdentifier: String = gv.srcCodeIdentifier
  override def packageName: String = gv.packageName
  override def tpe: CHIR.Type = provider.getType[CHIR.Type](v.`type`).get
  override def initializer: Option[CHIR.Value] = provider.getValue[CHIR.Value](g.initializer)
}

final class LocalVarImpl(l: LocalVar)(using provider: CHIRItemProvider) extends CHIR.LocalVar {
  override def tpe: CHIR.Type = provider.getType[CHIR.Type](l.base.`type`).get
  override def associatedExpr: CHIR.Expression = provider.getExpr[CHIR.Expression](l.associatedExpr)
}

final class ParameterImpl(p: Parameter)(using provider: CHIRItemProvider) extends CHIR.Parameter {
  override def tpe: CHIR.Type = provider.getType[CHIR.Type](p.base.`type`).get
}

trait LiteralImpl(l: LiteralValue)(using provider: CHIRItemProvider) extends CHIR.Literal {
  override def tpe: CHIR.Type = provider.getType[CHIR.Type](l.base.`type`).get
}

final class NullLiteralImpl(n: NullLiteral)(using provider: CHIRItemProvider) extends LiteralImpl(n.base) with CHIR.NullLiteral {
}

final class IntLiteralImpl(n: IntLiteral)(using provider: CHIRItemProvider) extends LiteralImpl(n.base) with CHIR.IntLiteral {
  override def value: Long = n.`val`
}

final class FloatLiteralImpl(n: FloatLiteral)(using provider: CHIRItemProvider) extends LiteralImpl(n.base) with CHIR.FloatLiteral {
  override def value: Double = n.`val`
}

final class BoolLiteralImpl(n: BoolLiteral)(using provider: CHIRItemProvider) extends LiteralImpl(n.base) with CHIR.BoolLiteral {
  override def value: Boolean = n.`val`
}

final class RuneLiteralImpl(n: RuneLiteral)(using provider: CHIRItemProvider) extends LiteralImpl(n.base) with CHIR.RuneLiteral {
  override def value: Long = n.`val`
}

final class StringLiteralImpl(n: StringLiteral)(using provider: CHIRItemProvider) extends LiteralImpl(n.base) with CHIR.StringLiteral {
  override def value: String = n.`val`
}