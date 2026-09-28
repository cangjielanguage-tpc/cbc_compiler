package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers
import com.huawei.excelsior.jet.compiler.chir.CHIR.Func
import com.huawei.excelsior.jet.compiler.chir.dsl.CHIRDSL

import scala.collection.mutable

/** We expect this types to be included in the compiled CHIR package.
 *
 * It is not guaranteed that they are included - however we hope that they are.
 */
object AOTDefinitions {
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

class CHIRHelperGenerator(private var helperId: Long, _pkg: CHIR.Package) {
  implicit val pkg: CHIR.Package = _pkg
  private val helpers: mutable.HashMap[Long, CHIR.Func] = mutable.HashMap()
  private val helpersByName: mutable.HashMap[String, CHIR.Func] = mutable.HashMap()
  private val throwHelpers = Iterator(ThrowHelper("foo_helper", "CBC Internal error"))

  private case class ThrowHelper(name: String, exceptionMsg: String)

  private def throwHelperType: CHIR.FuncType = new CHIR.FuncType {
    def paramTypes: Seq[CHIR.Type] = Seq.empty
    def paramTypesWithoutReceiver: Seq[CHIR.Type] = Seq.empty
    def receiverType: CHIR.Type = CodeHelpers.shouldNotCallThis(s"receiver type is not expected for internal throw helper")
    def returnType: CHIR.Type = CHIR.BuiltinType.Nothing
    def isC: Boolean = false
    def hasVarArg: Boolean = false
  }

  private def throwHelperBody(exceptionType: CHIR.Type = AOTDefinitions.Exception,
                              exceptionInitFunc: CHIR.Func = AOTDefinitions.initException,
                              throwHelper: ThrowHelper)(implicit pkg: CHIR.Package): CHIR.BlockGroup = CHIRDSL.genBlockGroup(pkg) { gen =>
    gen.startBlock(gen.entryBlock)
    // %0 = Allocate(ExceptionType)
    val exception = gen.local(exceptionType, gen.alloc(exceptionType))

    // %1 = Constant(exceptionMsg)
    val exceptionMsg = gen.local(AOTDefinitions.String,
      gen.const(AOTDefinitions.String, throwHelper.exceptionMsg))

    // %2 = Apply(ExceptionType.init, %1, %2)
    val initException = gen.apply(exceptionInitFunc, Some(exceptionType), Some(exception), exception, exceptionMsg)
    gen.local(CHIR.BuiltinType.Void, initException)

    // RaiseException(%2)
    gen.local(CHIR.BuiltinType.Nothing, gen.raise(exception, Option.empty))
  }

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

      def packageName: String = "VERY_COOL_PACKAGE"

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

  private def throwHelper(_id: Long, helper: ThrowHelper)(implicit pkg: CHIR.Package): CHIR.Func =
    generateHelper(_id, helper.name, throwHelperType, throwHelperBody(throwHelper = helper))

  def generateHelpers(): CHIRHelperGenerator = {
    for (helper <- throwHelpers) {
      val func = throwHelper(helperId, helper)
      helpers.addOne((helperId, func))
      helpersByName.addOne((helper.name, func))
      helperId += 1
    }

    this
  }

  def contains(id: Long): Boolean = helpers.contains(id)
  def contains(name: String): Boolean = throwHelpers.contains(name)

  def function(idx: Int): CHIR.Func = helpers(idx)
  def getFunc(identifier: String): Option[CHIR.Func] = helpersByName.get(identifier)

  def values: Iterator[CHIR.Value] = helpers.values.map(f => (f: CHIR.Value)).iterator
}
