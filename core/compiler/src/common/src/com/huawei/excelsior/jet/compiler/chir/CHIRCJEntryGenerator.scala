package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.compiler.chir.CHIRCJEntryGenerator.*
import CHIRDSL.Ref

import scala.collection.mutable

object CHIRCJEntryGenerator {
  val name = "cj_entry"
  private val Bool = CHIR.BuiltinType.Boolean
  private val Unit = CHIR.BuiltinType.Unit
  private val Int64 = CHIR.BuiltinType.Int64
}

class CHIRCJEntryGenerator(pkg: CHIR.Package, _id: Long, userMain: CHIR.Func) {

  private val OOM = pkg.getDef("_CNat16OutOfMemoryErrorE").get.tpe
  private val Object = pkg.getDef("_CNat6ObjectE").get.tpe
  private val String = pkg.getDef("_CNat6StringE").get.tpe
  private val Error = pkg.getDef("_CNat5ErrorE").get.tpe
  private val Exception = pkg.getDef("_CNat9ExceptionE").get.tpe
  private val eprintlnFunc = pkg.getFunc("_CNat8eprintlnHRNat6StringE").get
  private val handleExFunc = pkg.getFunc("_CNat15handleExceptionHCNat9ExceptionE").get

  def gen(): CHIR.Func = {
    new CHIR.Func {
      private var _retVal: CHIR.LocalVar = _

      def tpe: CHIR.FuncType = new CHIR.FuncType {
        def paramTypes: Seq[CHIR.Type] = Seq.empty
        def paramTypesWithoutReceiver: Seq[CHIR.Type] = Seq.empty
        def receiverType: CHIR.Type = CodeHelpers.shouldNotCallThis(s"receiver type is not expected for $name")
        def returnType: CHIR.Type = Int64
        def isC: Boolean = false
        def hasVarArg: Boolean = false
      }

      def id: Long = _id
      def identifier: String = name
      def srcCodeIdentifier: String = name
      def packageName: String = userMain.packageName
      def kind: CHIR.Func.Kind = CHIR.Func.Kind.Default
      def genericTypeParams: Seq[CHIR.GenericType] = Seq.empty

      val body: Option[CHIR.BlockGroup] = Some(genBlockGroup(pkg) { gen =>

        // Reserve blocks for main CFG path
        val callPkgInit = gen.entryBlock
        val callPkgLitInit = gen.newBlock()
        val callMain = gen.newBlock()
        val saveMainRes = gen.newBlock()
        val catchBlock = gen.newXBlock()

        // Call pkg init

        gen.startBlock(callPkgInit)
        _retVal = gen.local(Ref(Int64), gen.alloc(Int64))
                  gen.local(Unit,       gen.tryApply(pkg.packageInitFunc, thisType = None)(callPkgLitInit, catchBlock))

        // Call pkg literal init

        gen.startBlock(callPkgLitInit)
        gen.local(Unit, gen.tryApply(pkg.packageInitLiteralFunc, thisType = None)(callMain, catchBlock))

        // Call main

        gen.startBlock(callMain)
        val userMainArgs = if (userMain.tpe.paramTypes.isEmpty) {
          Seq.empty
        } else {
          val getCmdLineArgsFunc = pkg.getFunc("_CNat18getCommandLineArgsHv").get
          val ArrOfStr = pkg.getDef("_CNat5ArrayIRNat6StringEE").get.tpe
          val args = gen.local(ArrOfStr, gen.apply(getCmdLineArgsFunc, thisType = None))
          Seq(args)
        }
        val mainRes = gen.local(Int64, gen.tryApply(userMain, thisType = None, userMainArgs*)(saveMainRes, catchBlock))

        // Save main result

        gen.startBlock(saveMainRes)
        gen.local(Unit, gen.st(mainRes, _retVal))
        gen.local(Unit, gen.exit())

        // Start exception handler

        gen.startBlock(catchBlock)
        val rawEx = gen.local(Ref(Object), gen.getException())
        val ex    = gen.local(Ref(Object), gen.intrinsic(CHIR.Intrinsic.Kind.BeginCatch, rawEx))

        def handleException(tpe: CHIR.Type, checkBlock: CHIRDSL.Block, fallthroughBlock: CHIRDSL.Block)(handler: CHIR.Value => Unit): Unit = {
          val handlerBlock = gen.newBlock()

          // Check if exception is instance of given type
          gen.startBlock(checkBlock)
          val isException = gen.local(Bool, gen.iof(ex, tpe))
                            gen.local(Unit, gen.br(isException, handlerBlock, fallthroughBlock))

          // Cast and handle exception if it is of given type
          gen.startBlock(handlerBlock)
          val except = gen.local(tpe,   gen.cast(ex))
                       handler(except)
          val res    = gen.local(Int64, gen.const(Int64, 1L))
                       gen.local(Unit,  gen.st(res, _retVal))
                       gen.local(Unit,  gen.exit())

          // fallthrough otherwise
        }

        val checkOOM = catchBlock
        val checkError = gen.newBlock()
        val checkException = gen.newBlock()
        val rethrow = gen.newBlock()

        handleException(Ref(OOM), checkOOM, checkError) { _ =>
          val msg = gen.local(String, gen.const(String, "An exception has occurred:    Out of memory"))
                    gen.local(Unit,   gen.apply(eprintlnFunc, thisType = None, msg))
        }

        handleException(Ref(Error), checkError, checkException) { _ =>
          // TODO write detailed message field value as printStackTrace is not available?
          val msg = gen.local(String,     gen.const(String, "An error has occurred: "))
                    gen.local(Unit,       gen.apply(eprintlnFunc, thisType = None, msg))
        }

        handleException(Ref(Exception), checkException, rethrow) { except =>
          gen.local(Unit, gen.apply(handleExFunc, thisType = None, except))
        }

        // Rethrow if not OOM, Error or Exception
        gen.startBlock(rethrow)
        gen.local(Unit, gen.raise(ex, exceptionBlock = None))
      })

      def params: Seq[CHIR.Parameter] = Seq.empty

      def retVal: Option[CHIR.LocalVar] = {
        val _ = body.get // just to ensure, that body is generated before the method is called
        Some(_retVal)
      }

      def annotations: Seq[CHIR.Annotation] = Seq.empty
      // TODO need some?
      def attributes: Seq[CHIR.Attribute] = Seq.empty
      def declaringDef: Option[CHIR.CustomTypeDef] = Option.empty
    }
  }
}

def genBlockGroup(pkg: CHIR.Package)(action: CHIRBodyGen => Unit): CHIR.BlockGroup = {
  val gen = CHIRBodyGen(pkg)
  action(gen)
  gen.finish()
}

class CHIRBodyGen(pkg: CHIR.Package) {

  private val blocks = mutable.ArrayBuffer.empty[CHIR.Block]
  val entryBlock: CHIRDSL.Block = newBlock()

  private var curBlock: CHIRDSL.Block = _

  def finish(): CHIRDSL.BlockGroup = CHIRDSL.BlockGroup(blocks.toSeq, entryBlock)

  def newBlock(): CHIRDSL.Block = CHIRDSL.Block()

  def newXBlock(): CHIRDSL.Block = {
    val b = newBlock()
    b.setIsLandingPad()
    b
  }

  def startBlock(b: CHIRDSL.Block): Unit = {
    assert(!b.frozen)
    curBlock = b
  }

  def local(tpe: CHIR.Type, expr: CHIR.Expression): CHIR.LocalVar = {
    assert(!curBlock.frozen)

    val loc = CHIRDSL.LocalVar(tpe, expr)

    expr match {
      case expr: CHIRDSL.HasResultVar =>
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

  def getException(): CHIR.Expression = CHIR.GetException

  def apply(callee: CHIR.Func, thisType: Option[CHIR.Type], args: CHIR.Value*): CHIR.Apply = {
    CHIRDSL.Apply(callee, thisType, args)
  }

  def intrinsic(kind: CHIR.Intrinsic.Kind, args: CHIR.Value*): CHIR.Intrinsic = {
    CHIRDSL.Intrinsic(kind, args)
  }

  def iof(obj: CHIR.Value, tpe: CHIR.Type): CHIR.InstanceOf = {
    CHIRDSL.InstanceOf(obj, tpe)
  }

  def cast(value: CHIR.Value): CHIR.StaticCast = {
    CHIRDSL.StaticCast(value)
  }

  def const(tpe: CHIR.Type, value: String): CHIR.Constant = {
    CHIRDSL.Constant(CHIRDSL.StringLiteral(tpe, value))
  }

  def const(tpe: CHIR.Type, value: Long): CHIR.Constant = {
    CHIRDSL.Constant(CHIRDSL.IntLiteral(tpe, value))
  }

  def st(value: CHIR.Value, location: CHIR.Value): CHIR.Store = {
    CHIRDSL.Store(value, location)
  }

  def alloc(allocatedType: CHIR.Type): CHIR.Allocate = {
    CHIRDSL.Allocate(allocatedType)
  }

  // Terminators

  def br(cond: CHIR.Value, trueBlock: CHIR.Block, falseBlock: CHIR.Block): CHIR.Branch = {
    CHIRDSL.Branch(cond, trueBlock, falseBlock)
  }

  def exit(): CHIR.Terminator = CHIR.Exit

  def raise(exceptionValue: CHIR.Value, exceptionBlock: Option[CHIR.Block]): CHIR.RaiseException = {
    CHIRDSL.RaiseException(exceptionValue, exceptionBlock)
  }

  def tryApply(callee: CHIR.Func, thisType: Option[CHIR.Type], args: CHIR.Value*)(succBlock: CHIR.Block, errBlock: CHIR.Block): CHIR.TryApply = {
    CHIRDSL.TryApply(callee, thisType, args)(succBlock, errBlock)
  }
}

object CHIRDSL {

  case class Ref(baseType: CHIR.Type) extends CHIR.RefType

  trait HasResultVar extends CHIR.HasResultVar {
    var resultTpe: CHIR.Type = _
    var resultVar: CHIR.LocalVar = _
  }

  case class BlockGroup(blocks: Seq[CHIR.Block], entryBlock: CHIR.Block) extends CHIR.BlockGroup

  class Block() extends CHIR.Block {
    private val exprs = mutable.ArrayBuffer.empty[CHIR.Expression]
    private var term: CHIR.Terminator = _
    private var isLandingPad = false

    def frozen = term != null

    def addExpr(expr: CHIR.Expression): Unit = exprs += expr
    def setTerminator(expr: CHIR.Terminator): Unit = term = expr
    def setIsLandingPad(): Unit = isLandingPad = true

    def expressions: Seq[CHIR.Expression] = {
      assert(frozen)
      exprs.toSeq :+ term
    }

    def nonTerminatorExpressions: Seq[CHIR.Expression] = {
      assert(frozen)
      exprs.toSeq
    }

    def terminator: CHIR.Terminator = {
      assert(frozen)
      term
    }

    def isLandingPadBlock: Boolean = {
      assert(frozen)
      isLandingPad
    }
  }

  case class LocalVar(tpe: CHIR.Type, associatedExpr: CHIR.Expression) extends CHIR.LocalVar

  case class Intrinsic(kind: CHIR.Intrinsic.Kind, args: Seq[CHIR.Value]) extends CHIR.Intrinsic with HasResultVar

  case class InstanceOf(obj: CHIR.Value, testType: CHIR.Type) extends CHIR.InstanceOf with HasResultVar

  case class StaticCast(value: CHIR.Value) extends CHIR.StaticCast with HasResultVar {
    def targetTpe = resultTpe
  }

  case class Constant(literal: CHIR.Literal) extends CHIR.Constant with HasResultVar

  case class StringLiteral(tpe: CHIR.Type, value: String) extends CHIR.StringLiteral

  case class IntLiteral(tpe: CHIR.Type, value: Long) extends CHIR.IntLiteral

  case class Apply(callee: CHIR.Func, thisType: Option[CHIR.Type], args: Seq[CHIR.Value]) extends CHIR.Apply with HasResultVar {
    def instantiatedTypeArgs = Seq.empty
  }

  case class Store(value: CHIR.Value, location: CHIR.Value) extends CHIR.Store

  case class Allocate(allocatedType: CHIR.Type) extends CHIR.Allocate

  // Terminators

  case class TryApply(callee: CHIR.Func, thisType: Option[CHIR.Type], args: Seq[CHIR.Value])(succBlock: CHIR.Block, errBlock: CHIR.Block)
    extends CHIR.TryApply with HasResultVar {
    def successors = Seq(succBlock, errBlock)

    def instantiatedTypeArgs = Seq.empty
  }

  case class Branch(condition: CHIR.Value, trueBlock: CHIR.Block, falseBlock: CHIR.Block) extends CHIR.Branch {
    def successors = Seq(trueBlock, falseBlock)
  }

  case class RaiseException(exceptionValue: CHIR.Value, exceptionBlock: Option[CHIR.Block]) extends CHIR.RaiseException {
    def successors = exceptionBlock.toSeq
  }

}

