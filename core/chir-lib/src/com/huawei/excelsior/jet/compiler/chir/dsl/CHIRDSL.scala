package com.huawei.excelsior.jet.compiler.chir.dsl

import com.huawei.excelsior.jet.compiler.chir.CHIR

import scala.collection.mutable

object CHIRDSL {

  def genBlockGroup(pkg: CHIR.Package)(action: CHIRBodyGen => Unit): CHIR.BlockGroup = {
    val gen = CHIRBodyGen(pkg)
    action(gen)
    gen.finish()
  }

  final class CHIRBodyGen(pkg: CHIR.Package) {

    private val blocks = mutable.ArrayBuffer.empty[CHIR.Block]
    val entryBlock: Block = newBlock()

    private var curBlock: Block = _

    def finish(): BlockGroup = BlockGroup(blocks.toSeq, entryBlock)

    def newBlock(): Block = {
      val b = Block()
      blocks += b
      b
    }

    def newXBlock(): Block = {
      val b = newBlock()
      b.markAsLandingPad()
      b
    }

    def startBlock(b: Block): Unit = {
      assert(!b.frozen)
      curBlock = b
    }

    def local(tpe: CHIR.Type, expr: CHIR.Expression): CHIR.LocalVar = {
      assert(!curBlock.frozen)

      val loc = LocalVar(tpe, expr)

      expr match {
        case expr: HasResultVar =>
          expr.resultTpe = tpe
          expr.resultVar = loc
        case _ => // do nothing
      }

      expr match {
        case expr: CHIR.Terminator => curBlock.setTerminator(expr)
        case _ => curBlock.addExpr(expr)
      }

      loc
    }

    def getException: CHIR.Expression = CHIR.GetException

    def apply(callee: CHIR.Func, thisType: Option[CHIR.Type], args: CHIR.Value*): CHIR.Apply = {
      Apply(callee, thisType, args)
    }

    def intrinsic(kind: CHIR.Intrinsic.Kind, args: CHIR.Value*): CHIR.Intrinsic = {
      Intrinsic(kind, args)
    }

    def iof(obj: CHIR.Value, tpe: CHIR.Type): CHIR.InstanceOf = {
      InstanceOf(obj, tpe)
    }

    def cast(value: CHIR.Value): CHIR.StaticCast = {
      StaticCast(value)
    }

    def const(tpe: CHIR.Type, value: String): CHIR.Constant = {
      Constant(StringLiteral(tpe, value))
    }

    def const(tpe: CHIR.Type, value: Long): CHIR.Constant = {
      Constant(IntLiteral(tpe, value))
    }

    def st(value: CHIR.Value, location: CHIR.Value): CHIR.Store = {
      Store(value, location)
    }

    def alloc(allocatedType: CHIR.Type): CHIR.Allocate = {
      Allocate(allocatedType)
    }

    def invoke(callee: CHIR.Func, thisType: CHIR.Type, thisArg: CHIR.Value, allArgs: CHIR.Value*): CHIR.Invoke = {
      Invoke(callee, thisType, thisArg, allArgs)
    }

    // Terminators

    def br(cond: CHIR.Value, trueBlock: CHIR.Block, falseBlock: CHIR.Block): CHIR.Branch = {
      Branch(cond, trueBlock, falseBlock)
    }

    def exit(): CHIR.Terminator = CHIR.Exit

    def raise(exceptionValue: CHIR.Value, exceptionBlock: Option[CHIR.Block]): CHIR.RaiseException = {
      RaiseException(exceptionValue, exceptionBlock)
    }

    def tryApply(callee: CHIR.Func, thisType: Option[CHIR.Type], args: CHIR.Value*)(succBlock: CHIR.Block, errBlock: CHIR.Block): CHIR.TryApply = {
      TryApply(callee, thisType, args)(succBlock, errBlock)
    }
  }
}
