package com.huawei.excelsior.jet.compiler.chir.dsl

import com.huawei.excelsior.jet.compiler.chir.CHIR

final class Intrinsic(val kind: CHIR.Intrinsic.Kind, val args: Seq[CHIR.Value]) extends CHIR.Intrinsic with HasResultVar

final class InstanceOf(val obj: CHIR.Value, val testType: CHIR.Type) extends CHIR.InstanceOf with HasResultVar

final class StaticCast(val value: CHIR.Value) extends CHIR.StaticCast with HasResultVar {
  def targetTpe = resultTpe
}

final class Constant(val literal: CHIR.Literal) extends CHIR.Constant with HasResultVar

final class StringLiteral(val tpe: CHIR.Type, val value: String) extends CHIR.StringLiteral

final class IntLiteral(val tpe: CHIR.Type, val value: Long) extends CHIR.IntLiteral

final class Apply(val callee: CHIR.Func, val thisType: Option[CHIR.Type], val thisArgOpt: Option[CHIR.Value], val args: Seq[CHIR.Value]) extends CHIR.Apply with HasResultVar {
  def thisArg: CHIR.Value = thisArgOpt.get
  def instantiatedTypeArgs = Seq.empty
}

final class Store(val value: CHIR.Value, val location: CHIR.Value) extends CHIR.Store

final class Allocate(val allocatedType: CHIR.Type) extends CHIR.Allocate

final class Invoke(val callee: CHIR.Func, val thisType: CHIR.Type, val thisArgOpt: Option[CHIR.Value], val args: Seq[CHIR.Value]) extends CHIR.Invoke with HasResultVar {
  def thisArg: CHIR.Value = thisArgOpt.get
  def instantiatedTypeArgs: Seq[CHIR.Type] = Seq.empty
}

// Terminators

final class TryApply(val callee: CHIR.Func, val thisType: Option[CHIR.Type], val thisArgOpt: Option[CHIR.Value], val args: Seq[CHIR.Value])(succBlock: CHIR.Block, errBlock: CHIR.Block)
  extends CHIR.TryApply with HasResultVar {
  def successors = Seq(succBlock, errBlock)

  def thisArg: CHIR.Value = thisArgOpt.get
  def instantiatedTypeArgs = Seq.empty
}

final class Branch(val condition: CHIR.Value, val trueBlock: CHIR.Block, val falseBlock: CHIR.Block) extends CHIR.Branch {
  def successors = Seq(trueBlock, falseBlock)
}

final class RaiseException(val exceptionValue: CHIR.Value, val exceptionBlock: Option[CHIR.Block]) extends CHIR.RaiseException {
  def successors = exceptionBlock.toSeq
}

final class GetException extends CHIR.GetException {
}

final class Exit extends CHIR.Exit {
}