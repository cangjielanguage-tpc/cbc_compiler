package com.huawei.excelsior.jet.compiler.chir


object CHIR {

  enum Version {
    case V1_0
  }

  def newPackage(version: Version, source: String): Package = version match {
    case Version.V1_0 => new v1_0.PackageImpl(source)
  }

  trait Package {
    def name(): String
    def typeDefs(): Seq[CustomTypeDef]
    def values(): Seq[Value]
    def function(idx: Int): Func
    def packageInitFunc(): Func
    def packageInitLiteralFunc(): Func
  }

  trait HasAnnotations {
    def annotations(): Seq[Annotation]
  }

  trait Annotation {
  }

  trait IsAutoEnvClass extends Annotation {
    def value(): Boolean
  }

  trait OverrideSrcFuncType extends Annotation {
    def tpe(): FuncType
  }

  trait WrappedRawMethod extends Annotation {
    def rawMethod(): Func
  }

  trait HasDeclaringDef {
    def declaringDef(): Option[CustomTypeDef]
  }

  trait HasAttributes {
    def attributes(): Seq[Attribute]
  }

  trait Value {
  }

  trait Func extends Value with HasDeclaringDef with HasAttributes {
    def tpe(): FuncType
    def id(): Long
    def identifier(): String
    def srcCodeIdentifier(): String
    def packageName(): String
    def kind(): Func.Kind
    def genericTypeParams(): Seq[GenericType]
    def body(): Option[BlockGroup]
    def params(): Seq[Parameter]
    def annotations(): Seq[Annotation]
    def retVal(): Option[LocalVar]
  }

  object Func {
    enum Kind {
      case Default,
      Getter,
      Setter,
      Lambda,
      ClassCtor,
      PrimalClassCtor,
      StructCtor,
      PrimalStructCtor,
      GlobalVarInit,
      Finalizer,
      MainEntry,
      AnnoFactory,
      Macro,
      DefaultParameter,
      InstanceVarInit
    }
  }

  trait BlockGroup extends Value {
    def blocks(): Seq[Block]
    def entryBlock(): Block = blocks().head
  }

  trait Block extends Value {
    def nonTerminatorExpressions(): Seq[Expression]
    def terminator(): Terminator
    def expressions(): Seq[Expression] = nonTerminatorExpressions() :+ terminator()
    def isLandingPadBlock(): Boolean
  }

  // static field or global var
  trait GlobalVar extends Value with HasDeclaringDef with HasAttributes {
    def id(): Long
    def identifier(): String
    def srcCodeIdentifier(): String
    def packageName(): String
    def tpe(): Type
    def initializer(): Option[Value]
    def annotations(): Seq[Annotation]
  }

  trait LocalVar extends Value {
    def tpe(): Type
    def associatedExpr(): Expression
  }

  trait Parameter extends Value {
    def tpe(): Type
  }

  object UnitLiteral extends Literal {
    def tpe(): Type = BuiltinType.Unit
  }

  trait NullLiteral extends Literal {
  }

  trait IntLiteral extends Literal {
    def value(): Long
  }

  trait FloatLiteral extends Literal {
    def value(): Double
  }

  trait BoolLiteral extends Literal {
    def value(): Boolean
  }

  trait RuneLiteral extends Literal {
    def value(): Long
  }

  trait StringLiteral extends Literal {
    def value(): String
  }

  trait InstanceVar extends HasAttributes {
    def tpe(): Type
    def name(): String
  }

  trait Type {
  }

  trait BoxType extends Type {
    def baseType(): Type
  }

  enum BuiltinType extends Type {
    case Rune,
    Boolean,
    Void,
    Unit,
    Nothing,
    Int8, Int16, Int32, Int64, IntNative,
    UInt8, UInt16, UInt32, UInt64, UIntNative, Float16, Float32, Float64,
    CString,
    This
  }

  trait ClassType extends CustomType {
    def typeDef(): ClassDef
  }

  trait CustomType extends Type {
    def typeDef(): CustomTypeDef
    def genericTypeParams(): Seq[Type]
  }

  trait CPointerType extends Type {
    def elementType(): Type
  }

  trait EnumType extends CustomType {
    def typeDef(): EnumDef
  }

  trait FuncType extends Type {
    def paramTypes(): Seq[Type]
    def receiverType(): Type
    def returnType(): Type
    def isC: Boolean
    def hasVarArg: Boolean
  }

  trait GenericType extends Type {
    def identifier(): String
    def upperBounds(): Seq[Type]
  }

  trait RefType extends Type {
    def baseType(): Type
  }

  trait RawArrayType extends Type {
    def elementType(): Type
    def dimension(): Long
  }

  trait StructType extends CustomType {
    def typeDef(): StructDef
  }

  trait TupleType extends Type {
    def fieldTypes(): Seq[Type]
  }

  trait VArrayType extends Type {
    def elementType(): Type
    def size(): Long
  }

  trait CustomTypeDef extends HasAttributes with HasAnnotations {
    def tpe(): CustomType
    def packageName(): String
    def identifier(): String
    def srcCodeIdentifier(): String
    def instanceVars(): Seq[InstanceVar]
    def staticVars(): Seq[GlobalVar]
    def methods(): Seq[Func]
    def vTables(): Seq[VTable]
    def implementedInterfaces(): Seq[ClassType]
  }

  trait EnumDef extends CustomTypeDef {
    def nonExhaustive(): Boolean
    def tpe(): EnumType
    def ctors(): Seq[EnumCtor]
  }

  trait ClassDef extends CustomTypeDef {
    def isClass(): Boolean
    def tpe(): ClassType
    def superClass(): Option[ClassType]
  }

  trait StructDef extends CustomTypeDef {
    def tpe(): StructType
  }

  trait ExtendDef extends CustomTypeDef {
    def genericTypeParams(): Seq[GenericType]
  }

  trait FuncSig {
    def name(): String
    def genericTypeParams(): Seq[Type]
  }

  trait VTable {
    def srcParentType(): ClassType
    def vMethods(): Seq[VMethod]
  }

  trait VMethod extends HasAttributes {
    def name(): String
    def sig(): FuncType
    def instance(): Func
    def genericTypeParams(): Seq[Type]
    def originalType(): FuncType
    def parentType(): Type
    def returnType(): Type
  }

  trait EnumCtor {
    def tpe(): FuncType
  }

  trait Expression {
  }

  trait Cast extends Expression with HasResultVar {
    def value(): Value
    def from(): Type
    def to(): Type
  }

  trait NumericCast extends Cast {
    def overflowStategy(): OverflowStrategy
  }

  trait StaticCast extends Cast {

  }

  trait UnBoxToRef extends Cast {

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
    def resultTpe(): CHIR.Type
  }

  trait UnaryExpression extends Expression with HasResultVar {
    def operand(): Value
    def kind(): UnaryExpression.Kind
  }

  object UnaryExpression {
    enum Kind {
      case Neg, Not, BitNot
    }
  }

  trait BinaryExpression extends Expression with HasResultVar {
    def kind(): BinaryExpression.Kind
    def overflowStrategy(): OverflowStrategy
    def leftOperand(): Value
    def rightOperand(): Value
  }

  object BinaryExpression {
    enum Kind {
      case Add, Sub, Mul, Div,
      Mod, Exp,
      LShift, RShift, And, Or, Xor,
      Lt, Gt, Le, Ge, Eq, NotEq
    }
  }

  trait AllocateExpression extends Expression with HasResultVar {
    def allocatedType(): Type
  }

  trait RawArrayAllocate extends Expression with HasResultVar {
    def elementType(): Type
    def size(): Value
  }

  trait GetElementRef extends Expression with HasResultVar {
    def base(): Value
    def path(): Seq[Long]
  }

  trait StoreElementRef extends Expression with HasResultVar {
    def value(): Value
    def location(): Value
    def path(): Seq[Long]
  }

  trait Field extends Expression with HasResultVar {
    def base(): Value
    def path(): Seq[Long]
  }

  trait Apply extends Expression with HasResultVar {
    def callee(): Func
    def thisType(): Option[Type]
    def instantiatedTypeArgs(): Seq[Type]
    def args(): Seq[Value]
  }

  trait Invoke extends Expression with HasResultVar {
    def callee(): Func
    def thisType(): Type
    def thisArg(): Value
    def instantiatedTypeArgs(): Seq[Type]
    def args(): Seq[Value]
  }

  trait GetRTTIStatic extends Expression with HasResultVar {

  }

  trait GetRTTI extends Expression with HasResultVar {

  }

  trait InstanceOf extends Expression with HasResultVar {
    def obj(): Value
    def testType(): Type
  }

  trait Intrinsic extends Expression with HasResultVar {
    def kind(): Intrinsic.Kind
    def args(): Seq[Value]
  }

  object Intrinsic {
    enum Kind {
      case
      Abs,
      ArrayAcquireRawData,
      ArrayGetUnchecked,
      ArrayGetRefUnchecked,
      ArrayGet,
      ArraySetUnchecked,
      ArraySet,
      ArraySize,
      ArrayBuiltinCopyTo,
      AtomicFetchAnd,
      AtomicFetchAdd,
      AtomicFetchOr,
      AtomicFetchSub,
      AtomicFetchXor,
      AtomicCAS,
      AtomicLoad,
      AtomicStore,
      AtomicSwap,
      BeginCatch,
      Preinitialize,
      CPointerRead,
      CPointerWrite,
      Sqrt,
    }
  }

  trait Spawn extends Expression with HasResultVar {
    def obj(): Value
    def executeClosure(): Option[Func]
  }

  trait Debug extends Expression {}

  trait Literal extends Value {
    def tpe(): Type
  }

  trait Constant extends Expression with HasResultVar {
    def literal(): Value
  }

  trait Load extends Expression {
    def location(): Value
  }

  trait Store extends Expression {
    def value(): Value
    def location(): Value
  }

  trait Tuple extends Expression with HasResultVar {
    def elementValues(): Seq[Value]
  }

  object GetException extends Expression {
  }

  trait RawArrayInitByValue extends Expression {
    def array(): Value
    def size(): Value
    def initValue(): Value
  }

  trait RawArrayLiteralInit extends Expression {
    def array(): Value
    def elementValues(): Seq[Value]
  }

  trait Terminator extends Expression {}

  trait HasSuccessors {
    def successors(): Seq[Block]
  }

  trait Branch extends Expression with Terminator with HasSuccessors {
    def condition(): Value
    def trueBlock(): Block
    def falseBlock(): Block
    def successors(): Seq[Block] = Seq(trueBlock(), falseBlock())
  }

  trait Exit extends Expression with Terminator {
  }

  trait Goto extends Expression with Terminator with HasSuccessors {
    def destination(): Block
    def successors(): Seq[Block] = Seq(destination())
  }

  trait MultiBranch extends Expression with Terminator with HasSuccessors {
    def condition(): Block
    def defaultBlock(): Block
    def normalBlocks(): Seq[Block]
    def caseValues(): Seq[Long]
    def successors(): Seq[Block] = defaultBlock() +: normalBlocks()
  }

  trait RaiseException extends Expression with Terminator with HasSuccessors {
    def exceptionValue(): Value
    def exceptionBlock(): Option[Block]
    def successors(): Seq[Block] = exceptionBlock().toSeq
  }

  trait TryAllocate extends AllocateExpression with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  trait TryApply extends Apply with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  trait TryBinaryExpression extends BinaryExpression with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  trait TryIntrinsic extends Intrinsic with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  trait TryInvoke extends Invoke with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  trait TryInvokeStatic extends Expression with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  trait TryNumericCast extends NumericCast with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  trait TryRawArrayAllocate extends RawArrayAllocate with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  trait TrySpawn extends Spawn with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  trait TryUnaryExpression extends UnaryExpression with Terminator with HasSuccessors {
    def succBlock(): Block
    def errBlock(): Block
    def successors(): Seq[Block] = Seq(succBlock(), errBlock())
  }

  enum Attribute {
    case Static // Mark whether a member is a static one.
    case Public // Mark whether a member is a public one.
    case Private // Mark whether a member is a private one.
    case Protected // Mark whether a member is a protected one.

    case Abstract // Mark whether a function is an abstract one.
    case Virtual // Mark whether a declaration is in fact open (even if the user does not use `open` keyword).

    case Override // Mark whether a declaration in fact overrides the inherited one (even if the user does not use `override` keyword).

    case Redef // Mark whether a declaration in fact overrides the inherited one (even if the user does not use `redef` keyword).

    case Sealed // Mark whether a declaration is a sealed one.
    case Foreign // Mark whether a declaration is a foreign one.

    case Mut // Mark whether a declaration is a mutable one.
    case Final // Mark a func override a parent class's func, and this func self does not have VIRTUAL Attribute.
    case Operator // Mark whether a declaration is a operator one.
    case Readonly // 'let x = xxx', 'x' enable READONLY attribute
    case Const // correspond `const` keyword in Cangjie source code.
    case Imported // Mark whether variable、func、enum、struct、class is imported from other package.
    case GenericInstantiated // Mark whether a `GlobalVar/Function/Type` is instantiated.
    case NoDebugInfo // Mark a `Value` doesn't contain debug info, like line/column number.
    case Generic // Mark a declaration is generic
    case Internal // GlobalVar/Function/Enum/Class/Struct/Interface is visible in current and sub package.
    case CompilerAdd // Mark a `Value` is added by compiler, like "copied default func from interface".

    // compiler attribute
    case NoReflectInfo // Mark a `Value` is't used by `reflect` feature.
    case NoInline // Mark a Function can't be inlined.
    case NonRecompile // only used in imported global var/func in incremental compilation, indicate this value is converted from a decl in current package that is not recompiled.
    case Unreachable // Mark a Block is unreachable.
    case NoSideEffect // Mark a Function does't have side effect.
    case Common // Mark whether it's common declaration.
    case Specific // Mark whether it's specific declaration.
    case SkipAnalysis // Mark node that is not used for analysis e.g. Node can be skiped if it has no body when creating 'common part'
    case Deserialized // Node deserialized from .chir file
    case Initializer // Mark nodes that related to initialization process.
    // Marked functions are package initializer, file initializers, variable initializer or so.
    // On the block is used to search for it among other blocks of the function.
    case Unsafe // Mark whether a function that was marked as `unsafe`
    // Native FFI attributes
    case JavaMirror // Mark whether it's @JavaMirror declaration (binding for a java type).
    case JavaImpl // Mark whether it's @JavaImpl declaration.
    case ObjCMirror // Mark whether it's @ObjCMirror declaration (binding for an Objective-C type).
    case HasInitedField // Mark whether a node is a special flag, which marks the class instance as initialized.
    case JavaHasDefault // Mark whether JAVA_MIRROR interface has default method.
    case PreviouslyDeserialized // Mark that deserialization occurs not in the newly created node, but in an existing one.

    case ATTR_END

    def name: String = this match {
      case Readonly => "readOnly"
      case Const => "compileTimeVal"
      case CompilerAdd => "compilerAdd"
      case NonRecompile => "nonRecompile"
      case NoReflectInfo => "noReflectInfo"
      case NoDebugInfo => "noDebugInfo"
      case NoInline => "noInline"
      case NoSideEffect => "noSideEffect"
      case JavaMirror => "javaMirror"
      case JavaImpl => "javaImpl"
      case _ => toString.toLowerCase
    }
  }

  enum OverflowStrategy {
    case Na,
    Wrapping,
    Throwing,
    Saturating,
  }
}
