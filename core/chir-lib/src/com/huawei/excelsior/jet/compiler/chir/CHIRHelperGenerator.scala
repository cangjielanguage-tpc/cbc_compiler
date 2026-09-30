package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.compiler.chir.CHIR.Func
import com.huawei.excelsior.jet.compiler.chir.CHIRHelperGenerator.cjEntryName
import com.huawei.excelsior.jet.compiler.chir.dsl.CHIRDSL

import scala.collection.mutable

/** We expect this types to be included in the compiled CHIR package.
 *
 * It is not guaranteed that they are included - however we hope that they are.
 */
object AOTDefs {
  def OOM(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat16OutOfMemoryErrorE").get.tpe
  def Object(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat6ObjectE").get.tpe
  def String(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat6StringE").get.tpe
  def Error(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat5ErrorE").get.tpe
  def Exception(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat9ExceptionE").get.tpe
  def initException(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat9Exception6<init>HRNat6StringE").get
  def eprintlnFunc(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat8eprintlnHRNat6StringE").get
  def errToString(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat5Error8toStringHv").get
  def handleExFunc(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat15handleExceptionHCNat9ExceptionE").get
}

object CHIRHelperGenerator {
    val cjEntryName = "cj_entry"
}

class CHIRHelperGenerator(_pkg: CHIR.Package) {
  implicit val pkg: CHIR.Package = _pkg
  // If you add new throw helpers you should also add them to
  // NewAsmParser to synthesize fake throwers for asm tests
  private val throwHelpers = Seq(
    ThrowHelper("throwSymbolResolutionError", "CBC internal error: symbol resolution error"),
    ThrowHelper("throwAbstractMethodCallError", "CBC internal error: abstract method was called")
  )

  private def generateHelper(_id: Long, _name: String,
                             _tpe: CHIR.FuncType,
                             _body: CHIR.BlockGroup,
                             _params: Seq[CHIR.Parameter] = Seq.empty,
                             _retVal: CHIR.LocalVar = null): CHIR.Func = {
    new CHIR.Func {
      def tpe: CHIR.FuncType = _tpe
      def id: Long = _id
      def identifier: String = _name
      def srcCodeIdentifier: String = _name
      def packageName: String = "cbc_intrinsics"
      def kind: Func.Kind = CHIR.Func.Kind.Default
      def genericTypeParams: Seq[CHIR.GenericType] = Seq.empty
      def body: Option[CHIR.BlockGroup] = Option(_body)
      def params: Seq[CHIR.Parameter] = Seq.empty
      def retVal: Option[CHIR.LocalVar] = Option(_retVal)
      def annotations: Seq[CHIR.Annotation] = Seq.empty
      def attributes: Seq[CHIR.Attribute] = Seq.empty
      def declaringDef: Option[CHIR.CustomTypeDef] = Option.empty
    }
  }

  private case class ThrowHelper(name: String, exceptionMsg: String)

  private object ThrowHelperGenerator {
    private def throwHelperType: CHIR.FuncType = new CHIR.FuncType {
      def paramTypes: Seq[CHIR.Type] = Seq.empty
      def paramTypesWithoutReceiver: Seq[CHIR.Type] = Seq.empty
      def receiverType: CHIR.Type = CodeHelpers.shouldNotCallThis(s"receiver type is not expected for internal throw helper")
      def returnType: CHIR.Type = CHIR.BuiltinType.Nothing
      def isC: Boolean = false
      def hasVarArg: Boolean = false
    }

    private def throwHelperBody(exceptionType: CHIR.Type = AOTDefs.Exception,
                                exceptionInitFunc: CHIR.Func = AOTDefs.initException,
                                throwHelper: ThrowHelper): CHIR.BlockGroup = CHIRDSL.genBlockGroup(pkg) { gen =>
      gen.startBlock(gen.entryBlock)
      val exception = gen.local(exceptionType, gen.alloc(exceptionType))

      val exceptionMsg = gen.local(AOTDefs.String, gen.const(AOTDefs.String, throwHelper.exceptionMsg))

      gen.apply(exceptionInitFunc, Some(exceptionType), Some(exception), exception, exceptionMsg)

      gen.local(CHIR.BuiltinType.Void, gen.apply(exceptionInitFunc, Some(exceptionType), Some(exception), exception, exceptionMsg))

      gen.local(CHIR.BuiltinType.Nothing, gen.raise(exception, Option.empty))
    }

    def throwHelper(helper: ThrowHelper): Int => CHIR.Func =
      id => generateHelper(id, helper.name, throwHelperType, throwHelperBody(throwHelper = helper))
  }

  private class CHIRCJEntryGenerator(_id: Long, userMain: CHIR.Func) {
    private val Bool = CHIR.BuiltinType.Boolean
    private val Unit = CHIR.BuiltinType.Unit
    private val Int64 = CHIR.BuiltinType.Int64

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

        def identifier: String = cjEntryName

        def srcCodeIdentifier: String = cjEntryName 

        def packageName: String = userMain.packageName

        def kind: CHIR.Func.Kind = CHIR.Func.Kind.Default

        def genericTypeParams: Seq[CHIR.GenericType] = Seq.empty

        val body: Option[CHIR.BlockGroup] = Some(CHIRDSL.genBlockGroup(pkg) { gen =>

          // Reserve blocks for main CFG path
          val callPkgInit = gen.entryBlock
          val callPkgLitInit = gen.newBlock()
          val callMain = gen.newBlock()
          val saveMainRes = gen.newBlock()
          val catchBlock = gen.newXBlock()

          // Call pkg init

          gen.startBlock(callPkgInit)
          _retVal = gen.local(dsl.Ref(Int64), gen.alloc(Int64))
          gen.local(Unit, gen.tryApplyStatic(pkg.packageInitFunc)(callPkgLitInit, catchBlock))

          // Call pkg literal init

          gen.startBlock(callPkgLitInit)
          gen.local(Unit, gen.tryApplyStatic(pkg.packageInitLiteralFunc)(callMain, catchBlock))

          // Call main

          gen.startBlock(callMain)
          val userMainArgs = if (userMain.tpe.paramTypes.isEmpty) {
            Seq.empty
          } else {
            val getCmdLineArgsFunc = pkg.getFunc("_CNat18getCommandLineArgsHv").get
            val ArrOfStr = pkg.getDef("_CNat5ArrayIRNat6StringEE").get.tpe
            val args = gen.local(ArrOfStr, gen.applyStatic(getCmdLineArgsFunc))
            Seq(args)
          }
          val mainRes = gen.local(Int64, gen.tryApplyStatic(userMain, userMainArgs *)(saveMainRes, catchBlock))

          // Save main result

          gen.startBlock(saveMainRes)
          gen.local(Unit, gen.st(mainRes, _retVal))
          gen.local(Unit, gen.exit())

          // Start exception handler

          gen.startBlock(catchBlock)
          val rawEx = gen.local(dsl.Ref(AOTDefs.Object), gen.getException)
          val ex = gen.local(dsl.Ref(AOTDefs.Object), gen.intrinsic(CHIR.Intrinsic.Kind.BeginCatch, rawEx))

          def handleException(tpe: CHIR.Type, checkBlock: dsl.Block, fallthroughBlock: dsl.Block)(handler: CHIR.Value => Unit): Unit = {
            val handlerBlock = gen.newBlock()

            // Check if exception is instance of given type
            gen.startBlock(checkBlock)
            val isException = gen.local(Bool, gen.iof(ex, tpe))
            gen.local(Unit, gen.br(isException, handlerBlock, fallthroughBlock))

            // Cast and handle exception if it is of given type
            gen.startBlock(handlerBlock)
            val except = gen.local(tpe, gen.cast(ex))
            handler(except)
            val res = gen.local(Int64, gen.const(Int64, 1L))
            gen.local(Unit, gen.st(res, _retVal))
            gen.local(Unit, gen.exit())

            // fallthrough otherwise
          }

          val checkOOM = catchBlock
          val checkError = gen.newBlock()
          val checkException = gen.newBlock()
          val rethrow = gen.newBlock()

          handleException(dsl.Ref(AOTDefs.OOM), checkOOM, checkError) { _ =>
            val msg = gen.local(AOTDefs.String, gen.const(AOTDefs.String, "An exception has occurred:    Out of memory"))
            gen.local(Unit, gen.applyStatic(AOTDefs.eprintlnFunc, msg))
          }

          handleException(dsl.Ref(AOTDefs.Error), checkError, checkException) { except =>
            val errStr = gen.local(AOTDefs.String, gen.invoke(AOTDefs.errToString, thisType = dsl.Ref(AOTDefs.Error), thisArg = Some(except), allArgs = except))
            gen.local(Unit, gen.applyStatic(AOTDefs.eprintlnFunc, errStr))
          }

          handleException(dsl.Ref(AOTDefs.Exception), checkException, rethrow) { except =>
            gen.local(Unit, gen.applyStatic(AOTDefs.handleExFunc, except))
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


  def generateHelpers: Seq[Int => CHIR.Func] = {
    val throwHelperFuncs = throwHelpers.map(ThrowHelperGenerator.throwHelper(_))
    val cjEntry = pkg.getFunc("user.main").map((mainFunc: CHIR.Func) => (id: Int) => CHIRCJEntryGenerator(id, mainFunc).gen())
    throwHelperFuncs ++ cjEntry.iterator
  }
}
