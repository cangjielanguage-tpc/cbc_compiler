package com.huawei.excelsior.jet.compiler.chir

/** We expect this types and functions to be included in the compiled CHIR package.
 *
 * It is not guaranteed that they are included.
 */
object AOTDefs {
  def OOM(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat16OutOfMemoryErrorE").get.tpe
  def Object(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat6ObjectE").get.tpe
  def String(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat6StringE").get.tpe
  def Error(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat5ErrorE").get.tpe
  def Exception(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat9ExceptionE").get.tpe
  def arrOfStr(implicit pkg: CHIR.Package): CHIR.Type = pkg.getDef("_CNat5ArrayIRNat6StringEE").get.tpe
  def initException(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat9Exception6<init>HRNat6StringE").get
  def eprintlnFunc(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat8eprintlnHRNat6StringE").get
  def errToString(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat5Error8toStringHv").get
  def handleExFunc(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat15handleExceptionHCNat9ExceptionE").get
  def atExitCallbacks(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat27CJ_CORE_ExecAtexitCallbacksHv").get
  def getCommandLineArgs(implicit pkg: CHIR.Package): CHIR.Func = pkg.getFunc("_CNat18getCommandLineArgsHv").get
}

trait CHIRFuncGenerator {
  def generateFunc(id: Int): Option[CHIR.Func]
}
