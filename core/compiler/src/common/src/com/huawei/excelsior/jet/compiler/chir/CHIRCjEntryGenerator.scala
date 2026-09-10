package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.compiler.chir.CHIRCjEntryGenerator.*

import scala.collection.mutable

object CHIRCjEntryGenerator {
  val name = "cj_entry"
  private val getEx = CHIR.GetException
  private val Bool = CHIR.BuiltinType.Boolean
  private val Unit = CHIR.BuiltinType.Unit
  private val Int64 = CHIR.BuiltinType.Int64
  private val Exit = CHIR.Exit
}

class CHIRCjEntryGenerator(pkg: CHIR.Package, _id: Long, main: CHIR.Func) {

  private val OOM = pkg.getCustomType("_CNat16OutOfMemoryErrorE").get
  private val Object = pkg.getCustomType("_CNat6ObjectE").get
  private val String = pkg.getCustomType("_CNat6StringE").get
  private val Error = pkg.getCustomType("_CNat5ErrorE").get
  private val Exception = pkg.getCustomType("_CNat9ExceptionE").get
  private val eprintlnFunc = pkg.getFunc("_CNat8eprintlnHRNat6StringE").get
  private val handleExFunc = pkg.getFunc("_CNat15handleExceptionHCNat9ExceptionE").get

  def gen(): CHIR.Func = {
    new CHIR.Func {
      private given g: bg.Raw = bg.Raw()
      private val retValIdx = 2

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
      def packageName: String = main.packageName
      def kind: CHIR.Func.Kind = CHIR.Func.Kind.Default
      def genericTypeParams: Seq[CHIR.GenericType] = Seq.empty

      lazy val body: Option[CHIR.BlockGroup] = {
        val exCatchBlock = g.bb(3)(
          exprs = Seq(
            (lv(8, ref(Object)), getEx),
            (lv(9, ref(Object)), intrinsic(CHIR.Intrinsic.Kind.BeginCatch, 8)),
            (lv(11, Bool),       iof(9, ref(OOM))),
          ),
          terminator = br(
            condIdx = 11,
            g.bb(8)(
              exprs = Seq(
                (lv(10, ref(OOM)), cast(9)),
                (lv(12, String),   const("An exception has occurred:    Out of memory")),
                (lv(13, Unit),     apply(eprintlnFunc, Seq(12))),
                (lv(14, Int64),    const(1L)),
                (lv(15, Unit),     st(14, 2)),
              ),
              terminator = Exit
            ),
            g.bb(9)(
              exprs = Seq(
                (lv(17, Bool), iof(9, ref(Error))),
              ),
              terminator = br(
                condIdx = 17,
                g.bb(12)(
                  exprs = Seq(
                    (lv(16, ref(Error)), cast(9)),
                    // TODO write detailed message field value as printStackTrace is not available?
                    (lv(18, String),     const("An error has occurred: ")),
                    (lv(19, Unit),       apply(eprintlnFunc, Seq(18))),
                    (lv(20, Int64),      const(1L)),
                    (lv(21, Unit),       st(20, 2)),
                  ),
                  terminator = Exit
                ),
                g.bb(13)(
                  exprs = Seq(
                    (lv(23, Bool), iof(9, ref(Exception))),
                  ),
                  terminator = br(
                    condIdx = 23,
                    g.bb(16)(
                      exprs = Seq(
                        (lv(22, ref(Exception)), cast(9)),
                        (lv(24, Unit),           apply(handleExFunc, argsIndices = Seq(22))),
                        (lv(25, Int64),          const(1L)),
                        (lv(26, Unit),           st(25, 2)),
                      ),
                      terminator = Exit
                    ),
                    g.bb(17)(
                      exprs = Seq(
                      ),
                      terminator = throwEx(9)
                    ),
                  )
                ),
              )
            )
          ),
          isLandingPadBlock = true
        )

        g.entry(
          exprs = Seq(
            (lv(retValIdx, ref(Int64)), alloc(Int64)),
          ),
          terminator = (lv(4, Unit), tryApply(pkg.packageInitFunc, Seq.empty,
            g.bb(5)(
              exprs = Seq(
              ),
              terminator = (lv(5, Unit), tryApply(pkg.packageInitLiteralFunc, Seq.empty,
                g.bb(6)(
                  exprs = Seq(
                  ),
                  terminator = (lv(6, Int64), tryApply(main, Seq.empty,
                    g.bb(7)(
                      exprs = Seq(
                        (lv(27, Unit), st(6, 2))
                      ),
                      terminator = Exit,
                    ),
                    exCatchBlock
                  )),
                  isLandingPadBlock = false
                ),
                exCatchBlock
              )),
              isLandingPadBlock = false
            ),
            exCatchBlock
          ))
        )

        Some(g.build())
      }

      def params: Seq[CHIR.Parameter] = Seq.empty

      def retVal: Option[CHIR.LocalVar] = {
        val _ = body.get // just to ensure, that body is generated before the method is called
        Some(g(retValIdx).asInstanceOf[CHIR.LocalVar])
      }

      def annotations: Seq[CHIR.Annotation] = Seq.empty
      // TODO
      def attributes: Seq[CHIR.Attribute] = Seq.empty
      def declaringDef: Option[CHIR.CustomTypeDef] = Option.empty
    }
  }

  private trait HasResultVar extends CHIR.HasResultVar {
    def resultTpe_=(tpe: CHIR.Type): Unit
    def resultVar_=(v: CHIR.LocalVar): Unit
  }

  private trait ValueProvider(deps: Int*)(implicit b: bg.Raw) {
    deps.foreach(b.indexVal)
  }

  private class alloc(val allocatedType: CHIR.Type) extends CHIR.Allocate {}

  private class tryApply(callee: CHIR.Func, argsIndices: Seq[Int], succBlock: CHIR.Block, errBlock: CHIR.Block)
                        (implicit b: bg.Raw) extends apply(callee, argsIndices) with CHIR.TryApply {
    def successors: Seq[CHIR.Block] = Seq(succBlock, errBlock)
  }

  private class apply(val callee: CHIR.Func, argsIndices: Seq[Int], val thisType: Option[CHIR.Type] = None)
                     (implicit b: bg.Raw) extends ValueProvider(argsIndices: _*) with CHIR.Apply with HasResultVar {
    def args: Seq[CHIR.Value] = argsIndices.map(b.apply)
    def instantiatedTypeArgs: Seq[CHIR.Type] = Seq.empty

    var resultTpe: CHIR.Type = _
    var resultVar: CHIR.LocalVar = _
  }

  private class invoke(val callee: CHIR.Func, val thisType: CHIR.Type, argsIndices: Int*)(implicit b: bg.Raw)
    extends ValueProvider(argsIndices: _*) with CHIR.Invoke with HasResultVar {
    def thisArg: CHIR.Value = args.head

    def args: Seq[CHIR.Value] = argsIndices.map(b.apply)

    def instantiatedTypeArgs: Seq[CHIR.Type] = Seq.empty

    var resultTpe: CHIR.Type = _
    var resultVar: CHIR.LocalVar = _
  }

  private class st(valueIdx: Int, locationIdx: Int)(implicit b: bg.Raw) extends ValueProvider(valueIdx, locationIdx) with CHIR.Store {
    def value: CHIR.Value = b(valueIdx)

    def location: CHIR.Value = b(locationIdx)
  }

  private class intrinsic(val kind: CHIR.Intrinsic.Kind, argsIndices: Int*)(implicit b: bg.Raw) extends ValueProvider(argsIndices: _*)
    with CHIR.Intrinsic with HasResultVar {
    def args: Seq[CHIR.Value] = argsIndices.map(b.apply)

    var resultTpe: CHIR.Type = _
    var resultVar: CHIR.LocalVar = _
  }

  private class iof(objIdx: Int, val testType: CHIR.Type)(implicit b: bg.Raw) extends ValueProvider(objIdx) with CHIR.InstanceOf with HasResultVar {
    def obj: CHIR.Value = b(objIdx)

    var resultTpe: CHIR.Type = _
    var resultVar: CHIR.LocalVar = _
  }

  private class cast(valueIdx: Int)(implicit b: bg.Raw) extends ValueProvider(valueIdx) with CHIR.StaticCast with HasResultVar {
    def value: CHIR.Value = b(valueIdx)

    def targetTpe: CHIR.Type = resultTpe

    var resultTpe: CHIR.Type = _
    var resultVar: CHIR.LocalVar = _
  }

  private class const(var literal: CHIR.Literal) extends CHIR.Constant with HasResultVar {
    var resultTpe: CHIR.Type = _
    var resultVar: CHIR.LocalVar = _
  }

  object const {
    private[CHIRCjEntryGenerator] def apply(str: String): const = {
      new const(new CHIR.StringLiteral {
        def value: String = str
        def tpe: CHIR.Type = String
      })
    }

    private[CHIRCjEntryGenerator] def apply(i: Long): const = {
      new const(new CHIR.IntLiteral {
        def value: Long = i
        def tpe: CHIR.Type = Int64
      })
    }
  }

  private final class br(condIdx: Int, val trueBlock: CHIR.Block, val falseBlock: CHIR.Block)(implicit b: bg.Raw) extends ValueProvider(condIdx) with CHIR.Branch {
    def condition: CHIR.Value = b(condIdx)
    def successors: Seq[CHIR.Block] = Seq(trueBlock, falseBlock)
  }

  private final class throwEx(exValIdx: Int)(implicit b: bg.Raw) extends ValueProvider(exValIdx) with CHIR.RaiseException {
    def exceptionValue: CHIR.Value = b(exValIdx)
    def exceptionBlock: Option[CHIR.Block] = None
    def successors: Seq[CHIR.Block] = Seq.empty
  }

  private final class lv(idx: Int, val tpe: CHIR.Type)(implicit b: bg.Raw) extends CHIR.LocalVar {
    b.indexVal(idx, this)

    var associatedExpr: CHIR.Expression = _
  }

  private final class ref(val baseType: CHIR.Type) extends CHIR.RefType {}

  private final class bb(val id: Int, val exprs: Seq[(lv, CHIR.Expression)], val isLandingPadBlock: Boolean = false) extends CHIR.Block {
    for ((v, expr) <- exprs if v != null) {
      v.associatedExpr = expr
      expr match {
        case e: HasResultVar =>
          e.resultTpe = v.tpe
          e.resultVar = v
        case _ => // do nothing
      }
    }

    def nonTerminatorExpressions: Seq[CHIR.Expression] = expressions.init
    def terminator: CHIR.Terminator = expressions.last.asInstanceOf[CHIR.Terminator]
    def expressions: Seq[CHIR.Expression] = exprs.map(_._2)
  }

  private final class bg(val blocks: Seq[CHIR.Block], val entryBlock: CHIR.Block) extends CHIR.BlockGroup {}

  private object bg {
    class Raw {
      private var eb: bb = _
      private val vals = mutable.HashMap.empty[Int, CHIR.Value]
      private val bblocks = mutable.HashMap.empty[Int, bb]

      private val verified = false

      private object ValueStub extends CHIR.Value {}

      def entry(exprs: Seq[(lv, CHIR.Expression)], terminator: CHIR.Terminator): bb = {
        entry(exprs, (null, terminator))
      }

      def entry(exprs: Seq[(lv, CHIR.Expression)], terminator: (lv, CHIR.Terminator)): bb = {
        assert(eb == null, "entry block was alreayd set")
        eb = bb(0)(exprs, terminator, false)
        eb
      }

      def bb(idx: Int)(exprs: Seq[(lv, CHIR.Expression)], terminator: CHIR.Terminator, isLandingPadBlock: Boolean = false): bb = {
        bb(idx)(exprs, (null, terminator), isLandingPadBlock)
      }

      def bb(idx: Int)(exprs: Seq[(lv, CHIR.Expression)], terminator: (lv, CHIR.Terminator), isLandingPadBlock: Boolean): bb = {
        val b = new bb(idx, exprs :+ terminator, isLandingPadBlock)
        val res = bblocks.put(idx, b)
        if (res.isDefined) {
          throw new IllegalArgumentException(s"bb with idx '$idx' already exists")
        }
        b
      }

      def apply(idx: Int): CHIR.Value = vals(idx).ensuring(_ != ValueStub, s"value with idx '$idx' was not set")

      def indexVal(idx: Int): Unit = vals.getOrElseUpdate(idx, ValueStub)

      def indexVal(idx: Int, v: CHIR.Value): Unit = {
        if (vals.put(idx, v).getOrElse(ValueStub) != ValueStub) {
          throw new IllegalArgumentException(s"Value with idx '$idx' already exists")
        }
      }

      def build(): bg = {
        assert(eb != null, "entry block was not set")
        for ((idx, v) <- vals) {
          assert(v != ValueStub, s"value with idx '$idx' was not set")
        }
        new bg(bblocks.values.toSeq, eb)
      }
    }
  }
}

