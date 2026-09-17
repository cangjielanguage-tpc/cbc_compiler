package com.huawei.excelsior.jet.compiler.chir.v100

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR.Func
import com.huawei.excelsior.jet.compiler.chir.v100.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v100.CHIRUtils.{toExprSeq, toTypeSeq, toValueSeq}

final class FuncImpl(f: Function, val id: Long)(using val provider: CHIRItemProvider) extends CHIR.Func
  with HasAnnotationsImpl with HasAttributesImpl with HasDeclaringDefImpl {

  val gv: GlobalValue = f.base
  private val v = gv.base
  val base: Base = v.base
  val attrs: Long = base.attributes

  def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](v.`type`).get
  def identifier: String = v.identifier
  def srcCodeIdentifier: String = gv.srcCodeIdentifier
  def packageName: String = gv.packageName
  def kind: Func.Kind = f.funcKind match {
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
  def genericTypeParams = f.genericTypeParamsVector.toTypeSeq[CHIR.GenericType]
  def body: Option[CHIR.BlockGroup] = provider.getValue[CHIR.BlockGroup](f.body)
  def params = f.paramsVector.toValueSeq[CHIR.Parameter]
  def retVal: Option[CHIR.LocalVar] = provider.getValue[CHIR.LocalVar](f.retVal)
}

final class BlockGroupImpl(b: BlockGroup)(using provider: CHIRItemProvider) extends CHIR.BlockGroup {
  def blocks = b.blocksVector.toValueSeq[CHIR.Block]
  def entryBlock: CHIR.Block = block(b.entryBlock)
  private def block(idx: Long) = provider.getValue[CHIR.Block](idx).get
}

final class BlockImpl(b: Block)(using provider: CHIRItemProvider) extends CHIR.Block {
  lazy val expressions = b.exprsVector.toExprSeq[CHIR.Expression]
  def nonTerminatorExpressions: Seq[CHIR.Expression] = expressions.init
  def terminator: CHIR.Terminator = expressions.last.asInstanceOf[CHIR.Terminator]
  def isLandingPadBlock: Boolean = b.isLandingPadBlock
}

final class GlobalVarImpl(g: GlobalVar, val id: Long)(using val provider: CHIRItemProvider) extends CHIR.GlobalVar
  with HasAnnotationsImpl with HasAttributesImpl with HasDeclaringDefImpl {

  val gv: GlobalValue = g.base
  private val v = gv.base
  val base: Base = v.base
  val attrs: Long = base.attributes

  def identifier: String = v.identifier
  def srcCodeIdentifier: String = gv.srcCodeIdentifier
  def packageName: String = gv.packageName
  def tpe: CHIR.Type = provider.getType[CHIR.Type](v.`type`).get
  def initializer: Option[CHIR.Value] = provider.getValue[CHIR.Value](g.initializer)
}

final class LocalVarImpl(l: LocalVar)(using provider: CHIRItemProvider) extends CHIR.LocalVar {
  def tpe: CHIR.Type = provider.getType[CHIR.Type](l.base.`type`).get
  def associatedExpr: CHIR.Expression = provider.getExpr[CHIR.Expression](l.associatedExpr)
}

final class ParameterImpl(p: Parameter)(using provider: CHIRItemProvider) extends CHIR.Parameter {
  def tpe: CHIR.Type = provider.getType[CHIR.Type](p.base.`type`).get
}

trait LiteralImpl extends CHIR.Literal {
  def lv: LiteralValue
  implicit def provider: CHIRItemProvider

  def tpe: CHIR.Type = provider.getType[CHIR.Type](lv.base.`type`).get
}

final class NullLiteralImpl(n: NullLiteral)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.NullLiteral {
  val lv: LiteralValue = n.base
}

final class IntLiteralImpl(n: IntLiteral)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.IntLiteral {
  val lv: LiteralValue = n.base
  def value: Long = n.`val`
}

final class FloatLiteralImpl(n: FloatLiteral)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.FloatLiteral {
  val lv: LiteralValue = n.base
  def value: Double = n.`val`
}

final class BoolLiteralImpl(n: BoolLiteral)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.BoolLiteral {
  val lv: LiteralValue = n.base
  def value: Boolean = n.`val`
}

final class RuneLiteralImpl(n: RuneLiteral)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.RuneLiteral {
  val lv: LiteralValue = n.base
  def value: Long = n.`val`
}

final class StringLiteralImpl(n: StringLiteral)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.StringLiteral {
  val lv: LiteralValue = n.base
  def value: String = n.`val`
}