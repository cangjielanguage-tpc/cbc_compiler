package com.huawei.excelsior.jet.compiler.chir

import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}

/** Editing capabilities extend the corresponding read interfaces. */
object EditableCHIR {
  trait Package extends CHIR.Package {
    def edit(function: CHIR.Func): Func
    def edit(block: CHIR.Block): Block
    def edit(group: CHIR.BlockGroup): BlockGroup
    def edit(variable: CHIR.GlobalVar): GlobalVar
    def edit(definition: CHIR.CustomTypeDef): CustomTypeDef
    def edit(attributes: CHIR.HasAttributes): HasAttributes
    def edit(expression: CHIR.Expression): Expression

    def function(index: Int): Func
    def getFunc(identifier: String): Option[Func]
    def packageInitFunc: Func
    def packageInitLiteralFunc: Func
    final def functions: Iterator[Func] = values.collect { case f: CHIR.Func => edit(f) }
    def typeDefs: Iterator[CustomTypeDef]
    def getDef(identifier: String): Option[CustomTypeDef]

    def newGoto(destination: CHIR.Block, debugLoc: Option[CHIR.DebugLocation] = None): CHIR.Goto & Expression

    /** Normalize and validate once before serializing. No file I/O is performed. */
    def toBytes: Array[Byte]

    final def write(output: Path): Unit = {
      val bytes = toBytes
      val target = output.toAbsolutePath
      val temporary = Files.createTempFile(target.getParent, ".chir-", ".tmp")
      try {
        Files.write(temporary, bytes)
        try Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
        catch { case _: AtomicMoveNotSupportedException => Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING) }
      } finally Files.deleteIfExists(temporary)
    }

    // Construction helpers currently used only by tests.
    def newGlobalVar(identifier: String, tpe: CHIR.Type): GlobalVar
    def newFunction(identifier: String, paramTypes: Seq[CHIR.Type], returnType: CHIR.Type): Func
    def newExit(debugLoc: Option[CHIR.DebugLocation] = None): CHIR.Exit & Expression
    def newBlock(isLandingPad: Boolean = false): Block
    def newBlockGroup(blocks: Seq[CHIR.Block], entryBlock: CHIR.Block): BlockGroup
    def newBranch(condition: CHIR.Value, trueBlock: CHIR.Block, falseBlock: CHIR.Block,
                  debugLoc: Option[CHIR.DebugLocation] = None): CHIR.Branch & Expression
  }

  trait HasAttributes extends CHIR.HasAttributes {
    def replaceAttributes(attributes: Seq[CHIR.Attribute]): Unit
  }

  trait Func extends CHIR.Func with HasAttributes {
    protected[chir] def pkg: Package
    def body: Option[BlockGroup]
    def expressions: Seq[CHIR.Expression]
    def isInternal: Boolean
    def moveBody(destination: CHIR.Func): Unit
    final def removeBody(): Unit = replaceBody(None, None)

    // Explicit callers are tests; removeBody also uses this operation.
    def replaceBody(body: Option[CHIR.BlockGroup], retVal: Option[CHIR.LocalVar]): Unit
  }

  trait BlockGroup extends CHIR.BlockGroup {
    def blocks: Seq[Block]
    def entryBlock: Block

    // Currently used only by tests.
    def replaceBlocks(blocks: Seq[CHIR.Block], entryBlock: CHIR.Block): Unit
  }

  trait Block extends CHIR.Block {
    def expressions: Seq[Expression]
    def nonTerminatorExpressions: Seq[Expression]
    def terminator: CHIR.Terminator & Expression
    def replaceExpressions(expressions: Seq[CHIR.Expression], terminator: CHIR.Terminator): Unit
    def insert(operation: Operation, before: Option[CHIR.Expression] = None): CHIR.LocalVar

    // Currently used only by tests.
    final def replaceTerminator(terminator: CHIR.Terminator): Unit =
      replaceExpressions(nonTerminatorExpressions, terminator)
  }

  trait Expression extends CHIR.Expression {
    def replace(operation: Operation): Unit
  }

  trait GlobalVar extends CHIR.GlobalVar with HasAttributes

  trait CustomTypeDef extends CHIR.CustomTypeDef with HasAttributes {
    def replaceMethods(methods: Seq[CHIR.Func]): Unit
  }

  def fromBytes(bytes: Array[Byte], version: Int = CHIR.defaultVersion): Package =
    CHIRConfig.newEditable(bytes, version)

  def read(input: Path, version: Int = CHIR.defaultVersion): Package =
    fromBytes(Files.readAllBytes(input), version)

  /** The small set of expressions needed by package transformations. */
  enum Operation {
    case Allocate(tpe: CHIR.Type)
    case StaticCast(value: CHIR.Value, tpe: CHIR.Type)
    case BooleanConstant(value: Boolean)
    case Tuple(values: Seq[CHIR.Value], tpe: CHIR.Type)
    case Store(value: CHIR.Value, location: CHIR.Value)
    case Apply(function: CHIR.Func, args: Seq[CHIR.Value])
  }

  // Empty package construction is currently used only by tests.
  def empty(name: String, version: Int = CHIR.defaultVersion): Package =
    CHIRConfig.newEditable(name, version)
}
