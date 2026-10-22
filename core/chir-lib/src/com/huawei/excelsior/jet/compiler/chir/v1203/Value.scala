package com.huawei.excelsior.jet.compiler.chir.v1203

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR.Func
import com.huawei.excelsior.jet.compiler.chir.v1203.PackageFormat.*
import com.huawei.excelsior.jet.compiler.chir.v1203.CHIRUtils.{toTypeSeq, toValueSeq}

final class FuncImpl(private val f: FunctionT, val id: Long)(using val provider: CHIRItemProvider) extends CHIR.Func
  with HasAnnotationsImpl with HasAttributesImpl with HasDeclaringDefImpl {
  def gv: GlobalValueT = f.getBase
  private def v = gv.getBase
  def base: BaseT = v.getBase
  def attrs: Long = base.getAttributes

  def replaceAttributes(attributes: Seq[CHIR.Attribute]): Unit = {
    val mask = attributes.foldLeft(0L)((bits, attribute) => bits | (1L << attribute.ordinal))
    base.setAttributes(mask)
  }

  def tpe: CHIR.FuncType = provider.getType[CHIR.FuncType](v.getType).get
  def identifier: String = v.getIdentifier
  def srcCodeIdentifier: String = gv.getSrcCodeIdentifier
  def packageName: String = gv.getPackageName
  def kind: Func.Kind = f.getFuncKind match {
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
  def genericTypeParams = f.getGenericTypeParams.toTypeSeq[CHIR.GenericType]
  def body: Option[CHIR.BlockGroup] = provider.getValue[CHIR.BlockGroup](f.getBody)
  def params = f.getParams.toValueSeq[CHIR.Parameter]
  def retVal: Option[CHIR.LocalVar] = provider.getValue[CHIR.LocalVar](f.getRetVal)
  def sourceFile: Option[String] = Option(base).flatMap(b => Option(b.getLoc)).flatMap(l => Option(l.getFilePath))
  def ownerPackage: CHIR.Package = provider.asInstanceOf[PackageImpl]

  def moveBody(destination: CHIR.Func): Unit = {
    val pkg = provider.asInstanceOf[PackageImpl]
    pkg.valueId(this)
    pkg.valueId(destination)
    destination match {
      case target: FuncImpl if target.provider eq provider => pkg.moveBody(f, target.f)
      case _ => throw new IllegalArgumentException("Functions belong to another package")
    }
  }

  // APIs currently called only by tests in this repository (excluding forwarders).
  def replaceBody(body: Option[CHIR.BlockGroup], retVal: Option[CHIR.LocalVar]): Unit =
    provider.asInstanceOf[PackageImpl].replaceFunctionBody(id, body, retVal)
}

final class BlockGroupImpl private[v1203] (
  val ownerPackage: PackageImpl,
  private[v1203] val id: Long
) extends CHIR.BlockGroup {
  private def table = ownerPackage.groupTable(id)
  def blocks: Seq[CHIR.Block] = PackageImpl.ids(table.getBlocks).toSeq.map(ownerPackage.blockView)
  def entryBlock: CHIR.Block = ownerPackage.blockView(table.getEntryBlock)
  def replaceBlocks(blocks: Seq[CHIR.Block], entryBlock: CHIR.Block): Unit =
    ownerPackage.replaceBlocks(id, blocks, entryBlock)
}

final class BlockImpl private[v1203] (val ownerPackage: PackageImpl, private[v1203] val id: Long) extends CHIR.Block {
  private def b = ownerPackage.blockTable(id)
  def expressions: Seq[CHIR.Expression] =
    PackageImpl.ids(b.getExprs).toSeq.map(ownerPackage.getExpr[CHIR.Expression])
  def nonTerminatorExpressions: Seq[CHIR.Expression] = expressions.lastOption match {
    case Some(_: CHIR.Terminator) => expressions.init
    case _ => expressions
  }
  def terminator: CHIR.Terminator = expressions.lastOption match {
    case Some(t: CHIR.Terminator) => t
    case _ => throw new IllegalStateException(s"Block $id has no terminator")
  }
  def isLandingPadBlock: Boolean = b.getIsLandingPadBlock
  def replaceTerminator(terminator: CHIR.Terminator): Unit =
    replaceExpressions(nonTerminatorExpressions, terminator)
  def replaceExpressions(nonTerminators: Seq[CHIR.Expression], terminator: CHIR.Terminator): Unit =
    ownerPackage.replaceExpressions(id, nonTerminators, terminator)
}

final class GlobalVarImpl(g: GlobalVarT, val id: Long)(using val provider: CHIRItemProvider) extends CHIR.GlobalVar
  with HasAnnotationsImpl with HasAttributesImpl with HasDeclaringDefImpl {
  def gv: GlobalValueT = g.getBase
  private def v = gv.getBase
  def base: BaseT = v.getBase
  def attrs: Long = base.getAttributes

  def replaceAttributes(attributes: Seq[CHIR.Attribute]): Unit = {
    val mask = attributes.foldLeft(0L)((bits, attribute) => bits | (1L << attribute.ordinal))
    base.setAttributes(mask)
  }

  def identifier: String = v.getIdentifier
  def srcCodeIdentifier: String = gv.getSrcCodeIdentifier
  def packageName: String = gv.getPackageName
  def tpe: CHIR.Type = provider.getType[CHIR.Type](v.getType).get
  def initializer: Option[CHIR.Value] = provider.getValue[CHIR.Value](g.getInitializer)
}

final class LocalVarImpl(l: LocalVarT)(using provider: CHIRItemProvider) extends CHIR.LocalVar {
  def identifier: String = l.getBase.getIdentifier
  def srcCodeIdentifier: String = l.getSrcCodeIdentifier
  def tpe: CHIR.Type = provider.getType[CHIR.Type](l.getBase.getType).get
  def associatedExpr: CHIR.Expression = provider.getExpr[CHIR.Expression](l.getAssociatedExpr)
}

final class ParameterImpl(p: ParameterT)(using provider: CHIRItemProvider) extends CHIR.Parameter {
  def identifier: String = p.getBase.getIdentifier
  def srcCodeIdentifier: String = p.getSrcCodeIdentifier
  def tpe: CHIR.Type = provider.getType[CHIR.Type](p.getBase.getType).get
}

trait LiteralImpl extends CHIR.Literal {
  def lv: LiteralValueT
  implicit def provider: CHIRItemProvider
  def tpe: CHIR.Type = provider.getType[CHIR.Type](lv.getBase.getType).get
}

final class NullLiteralImpl(n: NullLiteralT)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.NullLiteral {
  def lv: LiteralValueT = n.getBase
}

final class IntLiteralImpl(n: IntLiteralT)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.IntLiteral {
  def lv: LiteralValueT = n.getBase
  def value: Long = n.getVal
}

final class FloatLiteralImpl(n: FloatLiteralT)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.FloatLiteral {
  def lv: LiteralValueT = n.getBase
  def value: Double = n.getVal
}

final class BoolLiteralImpl(n: BoolLiteralT)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.BoolLiteral {
  def lv: LiteralValueT = n.getBase
  def value: Boolean = n.getVal
}

final class RuneLiteralImpl(n: RuneLiteralT)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.RuneLiteral {
  def lv: LiteralValueT = n.getBase
  def value: Long = n.getVal
}

final class StringLiteralImpl(n: StringLiteralT)(using val provider: CHIRItemProvider) extends LiteralImpl with CHIR.StringLiteral {
  def lv: LiteralValueT = n.getBase
  def value: String = n.getVal
}

final class UnitLiteralImpl(n: UnitLiteralT)(using val provider: CHIRItemProvider)
  extends LiteralImpl with CHIR.UnitLiteralValue {
  def lv: LiteralValueT = n.getBase
}
