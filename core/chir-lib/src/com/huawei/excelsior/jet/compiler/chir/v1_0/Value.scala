package com.huawei.excelsior.jet.compiler.chir.v1_0

import com.huawei.excelsior.jet.compiler.chir.CHIR
import com.huawei.excelsior.jet.compiler.chir.CHIR.{Func, HasAnnotations}
import com.huawei.excelsior.jet.compiler.chir.v1_0.PackageFormat.{Function, GlobalValue, GlobalVar, MemberVarInfo, Value}

class FuncImpl(f: Function)(using provider: CHIRItemProvider) extends CHIR.Func
  with HasAnnotationsImpl(f.base.base.base) with HasAttributesImpl(f.base.base.base.attributes) with HasDeclaringDefImpl(f.base) {

  override def tpe(): CHIR.FuncType = ???

  override def id(): Long = ???

  override def identifier(): String = ???

  override def srcCodeIdentifier(): String = ???

  override def packageName(): String = ???

  override def kind(): Func.Kind = ???

  override def genericTypeParams(): Seq[CHIR.GenericType] = ???

  override def body(): Option[CHIR.BlockGroup] = ???

  override def params(): Seq[CHIR.Parameter] = ???

  override def retVal(): Option[CHIR.LocalVar] = ???
}

class BlockGroupImpl extends CHIR.BlockGroup {

  override def blocks(): Seq[CHIR.Block] = ???
}

class BlockImpl extends CHIR.Block {

  override def nonTerminatorExpressions(): Seq[CHIR.Expression] = ???

  override def terminator(): CHIR.Terminator = ???

  override def isLandingPadBlock(): Boolean = ???
}

class InstanceVarImpl(m: MemberVarInfo)(using provider: CHIRItemProvider) extends CHIR.InstanceVar with HasAttributesImpl(m.attributes) {
  override def tpe(): CHIR.Type = ???
  override def name(): String = ???
}

class GlobalVarImpl(g: GlobalVar, val id: Long)(using provider: CHIRItemProvider) extends CHIR.GlobalVar
  with HasAnnotationsImpl(g.base.base.base) with HasAttributesImpl(g.base.base.base.attributes) with HasDeclaringDefImpl(g.base) {

  private val gv = g.base
  private val v = gv.base
  private val b = v.base

  override def identifier(): String = v.identifier
  override def srcCodeIdentifier(): String = gv.srcCodeIdentifier
  override def packageName(): String = gv.packageName
  override lazy val tpe: CHIR.Type = provider.getType[CHIR.Type](v.`type`)
  override def initializer(): Option[CHIR.Value] = provider.getValue[CHIR.Value](g.initializer)
}

class LocalVarImpl extends CHIR.LocalVar {

  override def tpe(): CHIR.Type = ???

  override def associatedExpr(): CHIR.Expression = ???
}

class ParameterImpl extends CHIR.Parameter {
  override def tpe(): CHIR.Type = ???
}

class NullLiteralImpl extends CHIR.NullLiteral {

  override def tpe(): CHIR.Type = ???
}

class IntLiteralImpl extends CHIR.IntLiteral {

  override def value(): Long = ???

  override def tpe(): CHIR.Type = ???
}

class FloatLiteralImpl extends CHIR.FloatLiteral {

  override def value(): Double = ???

  override def tpe(): CHIR.Type = ???
}

class BoolLiteralImpl extends CHIR.BoolLiteral {

  override def value(): Boolean = ???

  override def tpe(): CHIR.Type = ???
}

class RuneLiteralImpl extends CHIR.RuneLiteral {

  override def value(): Long = ???

  override def tpe(): CHIR.Type = ???
}

class StringLiteralImpl extends CHIR.StringLiteral {

  override def value(): String = ???

  override def tpe(): CHIR.Type = ???
}