package com.huawei.excelsior.jet.compiler.chir

import com.huawei.excelsior.common.CodeHelpers.*
import java.nio.file.Path

/** CHIR read and editing interfaces. Loaded packages use generated mutable
  * object-API tables as their single backing representation. Existing read
  * signatures remain unchanged; ownership-aware edits maintain graph invariants.
  */
object CHIR {

  val defaultVersion = 13002
  val releaseBeta120Rc3Version = 1203

  def newPackage(source: String, version: Int): Package = PatchedPackage(version match {
    case CHIR.defaultVersion => new v13002.PackageImpl(source)
    case CHIR.releaseBeta120Rc3Version => new v1203.PackageImpl(source)
    case _ => notImplemented("unsupported CHIR version", version)
  })

  /** Load the serialized package itself, without compiler-generated overlays. */
  def read(input: Path, version: Int = defaultVersion): Package = version match {
    case CHIR.defaultVersion => v13002.PackageImpl.read(input)
    case CHIR.releaseBeta120Rc3Version => v1203.PackageImpl.read(input)
    case _ => notImplemented("unsupported CHIR version", version)
  }

  trait PackageOwned {
    def ownerPackage: Package
  }

  trait Package {
    def name: String
    def typeDefs: Iterator[CustomTypeDef]
    def values: Iterator[Value]
    def function(idx: Int): Func
    def packageInitFunc: Func
    def packageInitLiteralFunc: Func
    def getFunc(identifier: String): Option[Func]
    def getDef(identifier: String): Option[CustomTypeDef]

    def valueId(value: Value): Long
    def expressionId(expression: Expression): Long
    def definitionId(definition: CustomTypeDef): Long

    /** Create and register a goto expression in this package. */
    def newGoto(destination: Block, debugLoc: Option[DebugLocation] = None): Goto

    /** Move a non-member body to a function with one extra leading receiver.
      * Remap parameters and generic types; preserve the destination signature
      * and leave the source as a declaration. Bounds must match and composite
      * destination types must exist. Nested groups, lambdas, spawn, generic
      * instances and source self references are currently unsupported.
      * Cleanup and validation are automatic; nothing is serialized here.
      */
    def moveBody(source: Func, destination: Func): Unit

    /** Clean up unreachable blocks, validate, then save the complete schema.
      * Preserve table IDs, metadata and the input file-identifier convention.
      * Reject invalid graphs before replacing an existing output file.
      */
    def write(output: Path): Unit

    // APIs currently called only by tests in this repository (excluding forwarders).
    def typeId(tpe: Type): Long

    /** Factories immediately register nodes. Graph edits may be staged, but
      * all new nodes must be attached before moving a body or writing a file.
      */
    def newBlock(isLandingPad: Boolean = false): Block
    def newBlockGroup(blocks: Seq[Block], entryBlock: Block): BlockGroup
    def newBranch(condition: Value, trueBlock: Block, falseBlock: Block,
                  debugLoc: Option[DebugLocation] = None): Branch
  }

  trait HasAnnotations {
    def annotations: Seq[Annotation]
  }

  trait Annotation {
  }

  trait IsAutoEnvClass extends Annotation {
    def value: Boolean
  }

  trait OverrideSrcFuncType extends Annotation {
    def tpe: FuncType
  }

  trait WrappedRawMethod extends Annotation {
    def rawMethod: Func
  }

  trait HasDeclaringDef {
    def declaringDef: Option[CustomTypeDef]
  }

  trait HasAttributes {
    def attributes: Seq[Attribute]
  }

  trait Value {
  }

  trait Func extends Value with PackageOwned with FuncSig with HasDeclaringDef with HasAnnotations with HasAttributes {
    def tpe: FuncType
    def id: Long
    def identifier: String
    def name: String = identifier
    def srcCodeIdentifier: String
    def packageName: String
    def kind: Func.Kind
    def genericTypeParams: Seq[GenericType]
    def body: Option[BlockGroup]
    def params: Seq[Parameter]
    def retVal: Option[LocalVar]
    def sourceFile: Option[String]

    override def equalityKey: Product =
      (classOf[Func], super.equalityKey, srcCodeIdentifier, packageName, kind, attributes)

    // APIs currently called only by tests in this repository (excluding forwarders).
    /** Replace body and return local together. Reject foreign/unregistered
      * nodes and bodies still owned by another function. Use Package.moveBody
      * for transfers that require parameter and generic-type substitution.
      */
    def replaceBody(body: Option[BlockGroup], retVal: Option[LocalVar]): Unit

  }

  object Func {
    enum Kind {
      case Default
      case Getter
      case Setter
      case Lambda
      case ClassCtor
      case PrimalClassCtor
      case StructCtor
      case PrimalStructCtor
      case GlobalVarInit
      case Finalizer
      case MainEntry
      case AnnoFactory
      case Macro
      case DefaultParameter
      case InstanceVarInit
    }
  }

  trait BlockGroup extends Value with PackageOwned {
    def blocks: Seq[Block]
    def entryBlock: Block

    /** Update membership, parents and predecessors; entry must be a member.
      * Reject duplicate/shared/foreign blocks. Complete cleanup and validation
      * run automatically before writing or moving a body.
      */
    def replaceBlocks(blocks: Seq[Block], entryBlock: Block): Unit
  }

  trait Block extends Value with PackageOwned {
    def expressions: Seq[Expression]
    def nonTerminatorExpressions: Seq[Expression]
    def terminator: Terminator
    def isLandingPadBlock: Boolean

    /** Maintain ownership and predecessors, pruning unreachable blocks when
      * the attached body has complete control flow. Staged edits are allowed.
      */
    def replaceTerminator(terminator: Terminator): Unit
    def replaceExpressions(nonTerminators: Seq[Expression], terminator: Terminator): Unit
  }

  // static field or global var
  trait GlobalVar extends Value with HasDeclaringDef with HasAnnotations with HasAttributes {
    def id: Long
    def identifier: String
    def srcCodeIdentifier: String
    def packageName: String
    def tpe: Type
    def initializer: Option[Value]

    final override def equals(other: Any): Boolean = other match {
      case that: GlobalVar =>
        (this eq that) ||
          (identifier == that.identifier &&
            srcCodeIdentifier == that.srcCodeIdentifier &&
            packageName == that.packageName &&
            tpe == that.tpe &&
            attributes == that.attributes)
      case _ => false
    }

    final override def hashCode(): Int =
      (identifier, srcCodeIdentifier, packageName, tpe, attributes).hashCode()
  }

  trait LocalVar extends Value {
    def identifier: String
    def srcCodeIdentifier: String
    def tpe: Type
    def associatedExpr: Expression
  }

  trait Parameter extends Value {
    def identifier: String
    def srcCodeIdentifier: String
    def tpe: Type
  }

  trait UnitLiteralValue extends Literal

  object UnitLiteral extends UnitLiteralValue {
    def tpe: Type = BuiltinType.Unit
  }

  trait NullLiteral extends Literal {
  }

  trait IntLiteral extends Literal {
    def value: Long
  }

  trait FloatLiteral extends Literal {
    def value: Double
  }

  trait BoolLiteral extends Literal {
    def value: Boolean
  }

  trait RuneLiteral extends Literal {
    def value: Long
  }

  trait StringLiteral extends Literal {
    def value: String
  }

  trait InstanceVar extends HasAttributes {
    def tpe: Type
    def name: String
  }

  trait Type {
  }

  trait BoxType extends Type {
    def baseType: Type

    final override def equals(other: Any): Boolean = other match {
      case that: BoxType => (this eq that) || baseType == that.baseType
      case _ => false
    }

    final override def hashCode(): Int = (classOf[BoxType], baseType).hashCode()
  }

  enum BuiltinType extends Type {
    case Rune
    case Boolean
    case Void
    case Unit
    case Nothing
    case Int8, Int16, Int32, Int64, IntNative
    case UInt8, UInt16, UInt32, UInt64, UIntNative, Float16, Float32, Float64
    case CString
    case This
  }

  trait CustomType extends Type {
    def typeDef: CustomTypeDef
    def genericTypeParams: Seq[Type]

    def equalityKey: Product = (classOf[CustomType], typeDef, genericTypeParams)

    final override def equals(other: Any): Boolean = other match {
      case that: CustomType => (this eq that) || equalityKey == that.equalityKey
      case _ => false
    }

    final override def hashCode(): Int = equalityKey.hashCode()
  }

  trait ClassType extends CustomType {
    def typeDef: ClassDef

    override def equalityKey: Product = (classOf[ClassType], super.equalityKey)
  }

  trait EnumType extends CustomType {
    def typeDef: EnumDef

    override def equalityKey: Product = (classOf[EnumType], super.equalityKey)
  }

  trait StructType extends CustomType {
    def typeDef: StructDef

    override def equalityKey: Product = (classOf[StructType], super.equalityKey)
  }

  trait CPointerType extends Type {
    def elementType: Type

    final override def equals(other: Any): Boolean = other match {
      case that: CPointerType => (this eq that) || elementType == that.elementType
      case _ => false
    }

    final override def hashCode(): Int = (classOf[CPointerType], elementType).hashCode()
  }

  trait FuncType extends Type {
    def paramTypes: Seq[Type]
    def paramTypesWithoutReceiver: Seq[Type]
    def receiverType: Type
    def returnType: Type
    def isC: Boolean
    def hasVarArg: Boolean

    final override def equals(other: Any): Boolean = other match {
      case that: FuncType =>
        (this eq that) ||
          (paramTypes == that.paramTypes &&
            returnType == that.returnType &&
            isC == that.isC &&
            hasVarArg == that.hasVarArg)
      case _ => false
    }

    final override def hashCode(): Int =
      (paramTypes, returnType, isC, hasVarArg).hashCode()
  }

  trait GenericType extends Type {
    def identifier: String
    def upperBounds: Seq[Type]

    final override def equals(other: Any): Boolean = other match {
      case that: GenericType =>
        (this eq that) ||
          (identifier == that.identifier && upperBounds == that.upperBounds)
      case _ => false
    }

    final override def hashCode(): Int = (identifier, upperBounds).hashCode()
  }

  trait RefType extends Type {
    def baseType: Type

    final override def equals(other: Any): Boolean = other match {
      case that: RefType => (this eq that) || baseType == that.baseType
      case _ => false
    }

    final override def hashCode(): Int = (classOf[RefType], baseType).hashCode()
  }

  object RefType {
    def skip(t: Type): Type = t match {
      case t: RefType => t.baseType
      case t => t
    }
  }

  trait RawArrayType extends Type {
    def elementType: Type
    def dimension: Long

    final override def equals(other: Any): Boolean = other match {
      case that: RawArrayType =>
        (this eq that) ||
          (elementType == that.elementType && dimension == that.dimension)
      case _ => false
    }

    final override def hashCode(): Int = (elementType, dimension).hashCode()
  }

  trait TupleType extends Type {
    def fieldTypes: Seq[Type]

    final override def equals(other: Any): Boolean = other match {
      case that: TupleType => (this eq that) || fieldTypes == that.fieldTypes
      case _ => false
    }

    final override def hashCode(): Int = fieldTypes.hashCode()
  }

  trait VArrayType extends Type {
    def elementType: Type
    def size: Long

    final override def equals(other: Any): Boolean = other match {
      case that: VArrayType =>
        (this eq that) ||
          (elementType == that.elementType && size == that.size)
      case _ => false
    }

    final override def hashCode(): Int = (elementType, size).hashCode()
  }

  trait CustomTypeDef extends HasAttributes with HasAnnotations {
    def tpe: Type
    def packageName: String
    def identifier: String
    def srcCodeIdentifier: String
    def instanceVars: Seq[InstanceVar]
    def staticVars: Seq[GlobalVar]
    def methods: Seq[Func]
    def vtables: Seq[VTable]
    def implementedInterfaces: Seq[ClassType]

    def equalityKey: Product =
      (classOf[CustomTypeDef], packageName, identifier, srcCodeIdentifier, attributes)

    final override def equals(other: Any): Boolean = other match {
      case that: CustomTypeDef => (this eq that) || equalityKey == that.equalityKey
      case _ => false
    }

    final override def hashCode(): Int = equalityKey.hashCode()
  }

  trait EnumDef extends CustomTypeDef {
    def nonExhaustive: Boolean
    def tpe: EnumType
    def ctors: Seq[EnumCtor]

    override def equalityKey: Product =
      (classOf[EnumDef], super.equalityKey, nonExhaustive, ctors)
  }

  trait ClassDef extends CustomTypeDef {
    def isClass: Boolean
    def tpe: ClassType
    def superClass: Option[ClassType]

    override def equalityKey: Product =
      (classOf[ClassDef], super.equalityKey, isClass, superClass)
  }

  trait StructDef extends CustomTypeDef {
    def tpe: StructType

    override def equalityKey: Product =
      (classOf[StructDef], super.equalityKey)
  }

  trait ExtendDef extends CustomTypeDef {
    def genericTypeParams: Seq[GenericType]

    override def equalityKey: Product =
      (classOf[ExtendDef], super.equalityKey, genericTypeParams)
  }

  trait FuncSig {
    def name: String
    def tpe: FuncType
    def genericTypeParams: Seq[Type]

    def equalityKey: Product = (classOf[FuncSig], name, tpe, genericTypeParams)

    final override def equals(other: Any): Boolean = other match {
      case that: FuncSig => (this eq that) || equalityKey == that.equalityKey
      case _ => false
    }

    final override def hashCode(): Int = equalityKey.hashCode()
  }

  trait VTable {
    def srcParentType: ClassType
    def vMethods: Seq[VMethod]
  }

  trait VMethod extends FuncSig with HasAttributes {
    def instance: Func
    def originalType: FuncType
    def parentType: Type
    def returnType: Type

    override def equalityKey: Product =
      (classOf[VMethod], super.equalityKey, instance, originalType, parentType, returnType, attributes)
  }

  trait EnumCtor {
    def tpe: FuncType
  }

  case class DebugLocation(startLine: Long, endLine: Long)

  trait Expression {
    def debugLoc: Option[DebugLocation]
  }

  trait Cast extends Expression {
    def value: Value
    def targetTpe: Type
  }

  trait NumericCast extends Cast {
    def overflowStrategy: OverflowStrategy
  }

  trait StaticCast extends Cast {
  }

  trait UnboxToRef extends Cast {
  }

  trait UnboxToValue extends Cast {
  }

  trait Box extends Cast {
  }

  trait CastToConcrete extends Cast {
  }

  trait CastToGeneric extends Cast {
  }

  trait HasResultVar {
    def resultTpe: CHIR.Type
    def resultVar: CHIR.LocalVar
  }

  trait Unary extends Expression with HasResultVar {
    def operand: Value
    def kind: Unary.Kind
    def overflowStrategy: OverflowStrategy
  }

  object Unary {
    enum Kind {
      case Neg, Not, BitNot
    }
  }

  trait Binary extends Expression with HasResultVar {
    def kind: Binary.Kind
    def overflowStrategy: OverflowStrategy
    def leftOperand: Value
    def rightOperand: Value
  }

  object Binary {
    enum Kind {
      case Add, Sub, Mul, Div
      case Mod, Exp
      case LShift, RShift, And, Or, Xor
      case Lt, Gt, Le, Ge, Eq, NotEq
    }
  }

  trait Allocate extends Expression {
    def allocatedType: Type
  }

  trait RawArrayAllocate extends Expression {
    def elementType: Type
    def size: Value
  }

  trait GetElementRef extends Expression {
    def base: Value
    def path: Seq[Long]
  }

  trait StoreElementRef extends Expression {
    def value: Value
    def location: Value
    def path: Seq[Long]
  }

  trait Field extends Expression {
    def base: Value
    def path: Seq[Long]
  }

  trait Apply extends Expression with HasResultVar {
    def callee: Value
    def thisType: Option[Type]
    def thisArg: Value
    def instantiatedTypeArgs: Seq[Type]
    def args: Seq[Value]
  }

  trait Invoke extends Expression with HasResultVar {
    def callee: FuncSig
    def thisType: Type
    def thisArg: Value
    def instantiatedTypeArgs: Seq[Type]
    def overflowStrategy: OverflowStrategy
    def args: Seq[Value]
  }

  trait GetRTTIStatic extends Expression {
    def rttiType: Type
  }

  trait GetRTTI extends Expression {
    def obj: Value
  }

  trait InstanceOf extends Expression with HasResultVar {
    def obj: Value
    def testType: Type
  }

  trait Intrinsic extends Expression with HasResultVar {
    def kind: Intrinsic.Kind
    def args: Seq[Value]
  }

  object Intrinsic {
    enum Kind {
      case Abs
      case Fabs
      case ArrayAcquireRawData
      case ArrayGetUnchecked
      case ArrayGetRefUnchecked
      case ArrayGet
      case ArrayReleaseRawData
      case ArraySetUnchecked
      case ArraySet
      case VArrayGet
      case VArraySet
      case ArraySize
      case ArrayBuiltinCopyTo
      case AtomicFetchAnd
      case AtomicFetchAdd
      case AtomicFetchOr
      case AtomicFetchSub
      case AtomicFetchXor
      case AtomicCAS
      case AtomicLoad
      case AtomicStore
      case AtomicSwap
      case BeginCatch
      case Preinitialize
      case CPointerRead
      case CPointerWrite
      case CPointerInit0
      case CPointerInit1
      case ObjectZeroValue
      case Sqrt
      case Pow
      case Sin
      case Cos
    }
  }

  trait Spawn extends Expression with HasResultVar {
    def obj: Value
    def executeClosure: Option[Func]
  }

  trait Debug extends Expression {}

  trait Literal extends Value {
    def tpe: Type
  }

  trait Constant extends Expression with HasResultVar {
    def literal: Value
  }

  trait Load extends Expression {
    def location: Value
  }

  trait Store extends Expression {
    def value: Value
    def location: Value
  }

  trait Tuple extends Expression with HasResultVar {
    def elementValues: Seq[Value]
  }

  trait GetException extends Expression {
  }

  trait VArray extends Expression with HasResultVar {
    def elementValues: Seq[Value]
  }

  trait VArrayBuilder extends Expression with HasResultVar {
    def size: Value
    def initValue: Value
    def initializer: Value
  }

  trait RawArrayInitByValue extends Expression {
    def array: Value
    def size: Value
    def initValue: Value
  }

  trait RawArrayLiteralInit extends Expression {
    def array: Value
    def elementValues: Seq[Value]
  }

  trait Terminator extends Expression {}

  trait HasSuccessors {
    def successors: Seq[Block]
  }

  trait Branch extends Expression with Terminator with HasSuccessors {
    def condition: Value
    def trueBlock: Block
    def falseBlock: Block
  }

  trait Exit extends Expression with Terminator

  trait Goto extends Expression with Terminator with HasSuccessors {
    def destination: Block
  }

  trait MultiBranch extends Expression with Terminator with HasSuccessors {
    def condition: Value
    def defaultBlock: Block
    def normalBlocks: Seq[Block]
    def caseValues: Seq[Long]
  }

  trait RaiseException extends Expression with Terminator with HasSuccessors {
    def exceptionValue: Value
    def exceptionBlock: Option[Block]
  }

  trait TryAllocate extends Allocate with Terminator with HasSuccessors {
  }

  trait TryApply extends Apply with Terminator with HasSuccessors {
  }

  trait TryBinary extends Binary with Terminator with HasSuccessors {
  }

  trait TryIntrinsic extends Intrinsic with Terminator with HasSuccessors {
  }

  trait TryInvoke extends Invoke with Terminator with HasSuccessors {
  }

  trait TryNumericCast extends NumericCast with Terminator with HasSuccessors {
  }

  trait TryRawArrayAllocate extends RawArrayAllocate with Terminator with HasSuccessors {
  }

  trait TrySpawn extends Spawn with Terminator with HasSuccessors {
  }

  trait TryUnary extends Unary with Terminator with HasSuccessors {
  }

  enum Attribute {
    case Static                 // Mark whether a member is a static one.
    case Public                 // Mark whether a member is a public one.
    case Private                // Mark whether a member is a private one.
    case Protected              // Mark whether a member is a protected one.

    case Abstract               // Mark whether a function is an abstract one.
    case Virtual                // Mark whether a declaration is in fact open (even if the user does not use `open` keyword).

    case Override               // Mark whether a declaration in fact overrides the inherited one (even if the user does not use `override` keyword).

    case Redef                  // Mark whether a declaration in fact overrides the inherited one (even if the user does not use `redef` keyword).

    case Sealed                 // Mark whether a declaration is a sealed one.
    case Foreign                // Mark whether a declaration is a foreign one.

    case Mut                    // Mark whether a declaration is a mutable one.
    case Final                  // Mark a func override a parent class's func, and this func self does not have VIRTUAL Attribute.
    case Operator               // Mark whether a declaration is a operator one.
    case Readonly               // 'let x = xxx', 'x' enable READONLY attribute
    case Const                  // correspond `const` keyword in Cangjie source code.
    case Imported               // Mark whether variable、func、enum、struct、class is imported from other package.
    case GenericInstantiated    // Mark whether a `GlobalVar/Function/Type` is instantiated.
    case NoDebugInfo            // Mark a `Value` doesn't contain debug info, like line/column number.
    case Generic                // Mark a declaration is generic
    case Internal               // GlobalVar/Function/Enum/Class/Struct/Interface is visible in current and sub package.
    case CompilerAdd            // Mark a `Value` is added by compiler, like "copied default func from interface".

    // compiler attribute
    case NoReflectInfo          // Mark a `Value` is't used by `reflect` feature.
    case NoInline               // Mark a Function can't be inlined.
    case NonRecompile           // only used in imported global var/func in incremental compilation, indicate this value is converted from a decl in current package that is not recompiled.
    case Unreachable            // Mark a Block is unreachable.
    case NoSideEffect           // Mark a Function does't have side effect.
    case Common                 // Mark whether it's common declaration.
    case Specific               // Mark whether it's specific declaration.
    case SkipAnalysis           // Mark node that is not used for analysis e.g. Node can be skiped if it has no body when creating 'common part'
    case Deserialized           // Node deserialized from .chir file
    case Initializer            // Mark nodes that related to initialization process.
                                // Marked functions are package initializer, file initializers, variable initializer or so.
                                // On the block is used to search for it among other blocks of the function.
    case Unsafe                 // Mark whether a function that was marked as `unsafe`
    // Native FFI attributes
    case JavaMirror             // Mark whether it's @JavaMirror declaration (binding for a java type).
    case JavaImpl               // Mark whether it's @JavaImpl declaration.
    case ObjCMirror             // Mark whether it's @ObjCMirror declaration (binding for an Objective-C type).
    case HasInitedField         // Mark whether a node is a special flag, which marks the class instance as initialized.
    case JavaHasDefault         // Mark whether JAVA_MIRROR interface has default method.
    case PreviouslyDeserialized // Mark that deserialization occurs not in the newly created node, but in an existing one.

    case ATTR_END
  }

  enum OverflowStrategy {
    case Na, Checked, Wrapping, Throwing, Saturating
  }
}
