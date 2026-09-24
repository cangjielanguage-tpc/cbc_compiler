package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.compiler.chir.CHIRCJEntryGenerator.*
import com.huawei.excelsior.jet.compiler.chir.dsl.CHIRDSL

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
  private val errToString = pkg.getFunc("_CNat5Error8toStringHv").get
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
      def identifier: String = CHIRCJEntryGenerator.name
      def srcCodeIdentifier: String = CHIRCJEntryGenerator.name
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
                  gen.local(Unit,           gen.tryApplyStatic(pkg.packageInitFunc)(callPkgLitInit, catchBlock))

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
        val mainRes = gen.local(Int64, gen.tryApplyStatic(userMain, userMainArgs*)(saveMainRes, catchBlock))

        // Save main result

        gen.startBlock(saveMainRes)
        gen.local(Unit, gen.st(mainRes, _retVal))
        gen.local(Unit, gen.exit())

        // Start exception handler

        gen.startBlock(catchBlock)
        val rawEx = gen.local(dsl.Ref(Object), gen.getException)
        val ex    = gen.local(dsl.Ref(Object), gen.intrinsic(CHIR.Intrinsic.Kind.BeginCatch, rawEx))

        def handleException(tpe: CHIR.Type, checkBlock: dsl.Block, fallthroughBlock: dsl.Block)(handler: CHIR.Value => Unit): Unit = {
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

        handleException(dsl.Ref(OOM), checkOOM, checkError) { _ =>
          val msg = gen.local(String, gen.const(String, "An exception has occurred:    Out of memory"))
                    gen.local(Unit,   gen.applyStatic(eprintlnFunc, msg))
        }

        handleException(dsl.Ref(Error), checkError, checkException) { except =>
          val errStr = gen.local(String, gen.invoke(errToString, thisType = dsl.Ref(Error), thisArg = Some(except), allArgs = except))
                       gen.local(Unit,   gen.applyStatic(eprintlnFunc, errStr))
        }

        handleException(dsl.Ref(Exception), checkException, rethrow) { except =>
          gen.local(Unit, gen.applyStatic(handleExFunc, except))
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
