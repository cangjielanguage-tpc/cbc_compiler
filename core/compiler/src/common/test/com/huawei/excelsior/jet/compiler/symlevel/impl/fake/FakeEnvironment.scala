/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */
package com.huawei.excelsior.jet.compiler.symlevel.impl.fake

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.common.LanguagePack.JAVA
import com.huawei.excelsior.jet.assembler.Location.*
import com.huawei.excelsior.jet.assembler.{Label, Segment, Symbol}
import com.huawei.excelsior.jet.codeemitter.SymbolInfo
import com.huawei.excelsior.jet.codeemitter.SymbolInfo.AccessKind
import com.huawei.excelsior.jet.common.{BuiltInField, XString}
import com.huawei.excelsior.jet.compiler.*
import com.huawei.excelsior.jet.compiler.PDB2.Location
import com.huawei.excelsior.jet.compiler.abi.amd64.PlatformAmd64
import com.huawei.excelsior.jet.compiler.abi.{ABI, Frame, Platform}
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.driver.ProjectLogic
import com.huawei.excelsior.jet.compiler.ir.XInfo
import com.huawei.excelsior.jet.compiler.options.NumOption.ConsistencyCheckLevel
import com.huawei.excelsior.jet.compiler.options.{BoolOption, Option as CompilerOption}
import com.huawei.excelsior.jet.compiler.symlevel.*
import com.huawei.excelsior.jet.compiler.symlevel.CallConv.VMCALL
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.*
import xscala.io.{ByteBuffer, Path}
import xscala.properties.OS.WINDOWS

import java.lang.ref.Reference
import scala.collection.mutable

/** Environment with setters for all getters
  *
  * @author cypok
  */
object FakeEnvironment {
  private def getFakeSymbol(smth: AnyRef): Symbol = new Symbol() {
    override def toString: String = smth.toString
  }

  private val primTypes = mutable.HashMap.empty[TypeKind, FakeType]
  private val arraysCache = mutable.HashMap.empty[Type, mutable.HashMap[Integer, FakeType]]

  private val comExcelsiorAjLangAJRefArray =
    FakeType("com/huawei/excelsior/aj/lang/AJRefArray", ARRAY, null)
      .markAsAJManagedType()
  private val comExcelsiorAjLangAJObject =
    FakeType("com/huawei/excelsior/aj/lang/AJObject", CLASS, null)
      .markAsAJManagedType()
  private val comExcelsiorAjLangFinalizable =
    FakeType("com/huawei/excelsior/aj/lang/Finalizable", INTERFACE, comExcelsiorAjLangAJManaged)
      .markAsAJManagedType()
  private val comExcelsiorAjLangLockableAJObject =
    FakeType("com/huawei/excelsior/aj/lang/LockableAJObject", CLASS, comExcelsiorAjLangAJObject)
      .markAsAJManagedType()
  private val comExcelsiorAjLangAJString =
    FakeType("com/huawei/excelsior/aj/lang/AJString", CLASS, comExcelsiorAjLangAJObject)
      .markAsAJManagedType()
  private val comExcelsiorAjLangAJThrowable =
    FakeType("com/huawei/excelsior/aj/lang/AJThrowable", CLASS, comExcelsiorAjLangAJObject)
      .markAsAJManagedType()
  private val comExcelsiorAjLangAJManaged =
    FakeType("com/huawei/excelsior/aj/lang/AJManaged", INTERFACE, null)
      .markAsAJManagedType()
  private val comExcelsiorAjUtilAJIterator =
    FakeType("com/huawei/excelsior/aj/util/AJIterator", INTERFACE, comExcelsiorAjLangAJManaged)
      .markAsAJManagedType()
  private val comExcelsiorAjLangThinType =
    FakeType("com/huawei/excelsior/aj/lang/ThinType", CLASS, null)
      .markAsThinClass()
      .setAbstractClass(true)
  private val comExcelsiorAjLangPolyThinType =
    FakeType("com/huawei/excelsior/aj/lang/PolyThinType", CLASS, null)
      .markAsThinClass()
      .markAsPolyThinClass()
      .setAbstractClass(true)
  private val backtraceType =
    FakeType("com/huawei/excelsior/jet/runtime/excepts/stacktrace/Backtrace", CLASS, null)
      .markAsAJManagedType()
  private val managedEopType =
    FakeType("com/huawei/excelsior/aj/jetrt/ManagedEopType", CLASS, null)
      .markAsAJManagedType()
  private val ajWeakRefType =
    FakeType("com/huawei/excelsior/aj/util/ref/WeakRef", CLASS, null)
      .markAsAJManagedType();
  private val comExcelsiorJetRuntimeLangJavaRefType =
    FakeType("com/huawei/excelsior/jet/runtime/lang/JavaRefType", CLASS, comExcelsiorAjLangLockableAJObject)
  private val comExcelsiorJetRuntimeLangScalaRefType =
    FakeType("com/huawei/excelsior/jet/runtime/lang/ScalaRefType", CLASS, comExcelsiorAjLangLockableAJObject)
  private val comExcelsiorJetRuntimeLangCangjieRefType =
    FakeType("com/huawei/excelsior/jet/runtime/lang/CangjieRefType", CLASS, comExcelsiorAjLangAJObject)
  private val comExcelsiorJetRuntimeCompilerinterface =
    FakeType("com/huawei/excelsior/jet/runtime/compilerinterface/CompilerInterface", CLASS, comExcelsiorAjLangAJObject)
      .markAsAJManagedType()

  var rtConstResolver: FakeRTConstResolver = new FakeRTConstResolver()
}

class FakeEnvironment(targetPlatform: Platform[? <: IReg, ? <: FReg, ? <: ABI[? <: IReg, ? <: FReg]] = new PlatformAmd64(WINDOWS),
                      isJIT: Boolean = false, isDynamicBundle: Boolean = true) extends Environment with TypeProvider with SymbolLinker {
  import FakeEnvironment.*

  final protected val configuration = mutable.HashMap.empty[CompilerOption[?], Any]

  override def initEnv(): Unit = {
    super.initEnv()
    Env.setUnitTestsEnv()
    Env.init(targetPlatform, isJIT, isWorkMode = true, isDynamicBundle, JAVA, isStandalone = false)
    ProjectLogic.setEnvForUnitTests(this)
  }

  override def rtConstResolver: FakeRTConstResolver = FakeEnvironment.rtConstResolver

  define(ConsistencyCheckLevel, 3)

  // TODO-DECAF: ABI tests
  def this(targetPlatform: Platform[? <: IReg, ? <: FReg, ? <: ABI[? <: IReg, ? <: FReg]]) =
    this(targetPlatform, false)

  // TODO-DECAF: CangjieSymLevelMaker tests
  def this() =
    this(new PlatformAmd64(WINDOWS))

  def stage[A](stage: Stage)(action: => A): A = action

  override protected def sendMethodCode0(codeUnit: CodeUnit, seg: Segment, xinfo: XInfo, xTable: ByteBuffer, trivialXHandler: Boolean,
                                         hasMarkedRegions: Boolean, siberiaOffset: Int, frame: Frame[?, ?, ?]): Unit = {}

  override def sendData(seg: Segment, method: Method): Unit = {}

  override def sendBytecode(seg: Segment): Unit = {}

  override def getMarkedForPreparationTypes: Iterator[Type] = shouldNotCallThis()
  override def markForPreparation(`type`: Type): Unit = shouldNotCallThis()
  override def markForBootstrapPreparation(`type`: Type): Unit = shouldNotCallThis()

  override def getRTSGlobalSymbol(global: RTSGlobal): Symbol = getFakeSymbol(global)

  private val rtsProcs = mutable.HashMap.empty[RTSProc, FakeMethod]
  override def getRTSProc(proc: RTSProc): FakeMethod = rtsProcs.getOrElse(proc,
    new FakeMethod(proc.toString, FakeMethodType.create(VOID)
      .changeCallConv(VMCALL))
  )
  def setRtsProc(proc: RTSProc, method: FakeMethod): Unit = rtsProcs.put(proc, method)

  override def getSpecStrConcatMethod(format: String) = new FakeMethod(s"mi_StrConcat_concat_$format")
  override def getStaticFieldSymbol(host: Type, fieldOffset: Int) = getFakeSymbol("StaticFieldSymbol")

  override def getStringPoolEntry(host: Type, index: Int): ConstString = null

  override def makeStringRef(str: XString): Symbol = getFakeSymbol(s"StringRef[\"$str\"]")

  override def accessKind(symbol: Symbol): SymbolInfo.AccessKind = symbol match {
    case _: Label => AccessKind.DIRECT
    case _: FakeMethod => Method.accessKind
    case s: FakeSymbol => s.accessKind
  }

  override def makeConstStringData(str: XString, bstr: Boolean): Symbol = getFakeSymbol(s"\"$str\"")
  override def makeConstData(value: Array[Byte], align: Int): Symbol = getFakeSymbol(s"ConstData${value.mkString("[", ", ", "]")}@align$align")
  override def makeUninitializedData(size: Int): Symbol = getFakeSymbol(s"BSS[$size bytes]")
  override def makeDataSymbol(): Symbol = getFakeSymbol("Data")

  override def getTypeProvider: TypeProvider = this

  override def getSymbolLinker(rootClass: ClassType): SymbolLinker = this

  override val pdb: PDB2 = new PDB2 {
    override def getFile(name: String) = null

    override def getDataInputOrNull(loc: Location) = shouldNotCallThis()

    override def getDataOutput(loc: Location) = shouldNotCallThis()

    override def exists(loc: Location) = false // Try to prevent inter-procedural analysis.
  }

  private var allClasses = Seq.empty[ClassType]
  override def getAllClasses: Iterator[ClassType] = allClasses.iterator
  def setAllClasses(allClasses: Seq[ClassType]): Unit = {
    this.allClasses = allClasses
  }

  override def getPrimitiveType(typeKind: TypeKind): FakeType = {
    assert(typeKind.isPrimitive)
    primTypes.getOrElseUpdate(typeKind, FakeType(typeKind.toString, typeKind))
  }

  override def getArrayType(baseType: Type, dimNum: Int): FakeType = {
    assert(dimNum > 0)
    assert(!baseType.isJavaArray)
    val elemType = if (dimNum == 1) baseType else getArrayType(baseType, dimNum - 1)
    val dimsCache = arraysCache.getOrElseUpdate(baseType, mutable.HashMap.empty)
    dimsCache.getOrElseUpdate(dimNum, {
      val name = baseType.toString + "[]" * dimNum
      FakeType(name, TypeKind.ARRAY).setArrayElemType(elemType)
    })
  }

  override def getObjectType: FakeType             = FakeType.create(classOf[Object])
  override def getCloneableType: FakeType          = FakeType.create(classOf[Cloneable])
  override def getSerializableType: FakeType       = FakeType.create(classOf[Serializable])
  override def getStringType: FakeType             = FakeType.create(classOf[String])
  override def getClassType: FakeType              = FakeType.create(classOf[Class[?]])
  override def getThrowableType: FakeType          = FakeType.create(classOf[Throwable])
  override def getReferenceType: FakeType          = FakeType.create(classOf[Reference[?]])
  override def getIteratorType: FakeType           = FakeType.create(classOf[java.util.Iterator[?]])
  override def getScalaIteratorType: FakeType      = null
  override def getScalaBoxesRunTimeType: ClassType = null
  override def getXScalaAnyRef: ClassType          = null
  override def getXScalaString: ClassType          = null
  override def getXScalaClass: ClassType           = null
  override def getXScalaSerializable: ClassType    = null
  override def getAJObjectType: FakeType           = comExcelsiorAjLangAJObject
  override def getFinalizableType: FakeType        = comExcelsiorAjLangFinalizable
  override def getLockableAJObjectType: FakeType   = comExcelsiorAjLangLockableAJObject
  override def getAJStringType: FakeType           = comExcelsiorAjLangAJString
  override def getAJThrowableType: FakeType        = comExcelsiorAjLangAJThrowable
  override def getAJIteratorType: FakeType         = comExcelsiorAjUtilAJIterator
  override def getThinTypeType: FakeType           = comExcelsiorAjLangThinType
  override def getPolyThinTypeType: FakeType       = comExcelsiorAjLangPolyThinType

  override def getJavaRefType: FakeType            = comExcelsiorJetRuntimeLangJavaRefType
  override def getScalaRefType: FakeType           = comExcelsiorJetRuntimeLangScalaRefType
  override def getCangjieRefType: FakeType         = comExcelsiorJetRuntimeLangCangjieRefType

  override def getCompilerInterfaceType: FakeType  = comExcelsiorJetRuntimeCompilerinterface

  override def getAJArrayType(kind: BytecodeTypeKind): ClassType = {
    assert(kind == BytecodeTypeKind.CLASS) // TODO: support more if needed
    comExcelsiorAjLangAJRefArray
  }


  override def getParameterPassingLocationsType: FakeType = shouldNotCallThis()
  override def getCVarArgListDescType: FakeType = shouldNotCallThis()

  override def getBacktraceType: FakeType = backtraceType
  override def getAJWeakRefType: FakeType = ajWeakRefType


  override def isCangjieIterator(`type`: ClassType) = false

  override def isCangjieWeakRef(`type`: Type) = false

  override def getManagedEopType = managedEopType

  final val typesResolution = mutable.HashMap.empty[XString, ClassType]

  override def resolveTypeByName(refType: ClassType, name: XString): ClassType = findClass(name)

  override def findClass(name: XString, loadPDB: Boolean): ClassType =
    typesResolution.getOrElse(name, shouldNotReachHere(s"unexpected resolve of $name"))

  def registerFake(fakeClasses: ClassType*): Unit = for (fakeClass <- fakeClasses) typesResolution(fakeClass.getXName) = fakeClass

  override def getSymlevelWriter(writer: SymlevelWriter.StreamWriter, contextClass: Type): SymlevelWriter = shouldNotCallThis()
  override def getSymlevelReader(reader: SymlevelReader.StreamReader, contextClass: Type): SymlevelReader = shouldNotCallThis()

  var chaEnabled = false
  override def isCHAEnabledForTraceableReferences = chaEnabled

  override def getBuiltInFieldOffset(f: BuiltInField) = 42
  override def dropSymCache(): Unit = {}
  override def getClassTypeByNameAndClassLoaderSID(name: String, clsid: String): ClassType = null
  override def forcePrint(s: String): Unit = {}
  override def print(s: String): Unit = {}
  override def reportStatus(stage: String, methodName: String): Unit = {}
  override def reportPGOFailure(methodName: String, fatal: Boolean): Unit = {}
  override def reportWarning(s: String): Unit = {}
  override def getImportedClassIdx(importedType: Type, host: AnyRef) = 0

  def define(option: CompilerOption[?], value: Any): Unit = {
    configuration.put(option, value)
    dropOptionsCache()
  }

  def enable(option: BoolOption): Unit = {
    define(option, true)
  }

  def disable(option: BoolOption): Unit = {
    define(option, false)
  }

  override protected def optionValueOrNullFromConfig(option: CompilerOption[?]): Any =
    configuration.getOrElse(option, null)

  override def getPass: Pass = Pass.Backend

  override def getHotSwitchCases(method: Method, cases: Int): Iterator[Int] = shouldNotCallThis()

  override def getDebugIrLogsDir: Path = null

  override def asVerifiableMethod(method: Method) = method match {
    case method: FakeMethod => new FakeVerifiableMethod(method)
  }
}
