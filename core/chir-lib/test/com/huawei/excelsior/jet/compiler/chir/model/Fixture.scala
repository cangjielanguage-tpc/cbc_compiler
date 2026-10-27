/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir.model

import scala.collection.mutable.ArrayBuffer

/** Programmatically-built CHIR package fixture, shaped like a deserialized OPT-phase
  * package: builtin/composite/func/custom types, struct/class/enum/extend defs (incl.
  * an imported one), global vars (one with initializer), imported + current-pkg funcs,
  * a rich main body (all literal kinds, try-apply, numeric cast, branch/multibranch,
  * landing pad, lambda), annotations and debug locations.
  *
  * Shared by the round-trip specs (chir-testkit, both schema versions) and the
  * cbc-patchgen ChirOps/Clone specs (via chir-lib's `test->test` exports).
  *
  * Fixture ids are left at 0 (the materializer assigns ids on load; the serializer
  * renumbers — see ModelCompare's `compareIds` doc).
  */
object Fixture {

  private def annos(as: Annotation*): ArrayBuffer[Annotation] = ArrayBuffer(as*)
  /** Serializer always writes the AnnoInfo table (mirrors C++), so model values
    * deserialized from a real file carry Some(empty) rather than None. */
  private def noAnno(): Option[AnnoInfo] = Some(AnnoInfo("", ArrayBuffer()))
  private def noLoc(): Option[DebugLocation] =
    Some(DebugLocation("", 0, Some(Pos(0, 0)), Some(Pos(0, 0)), ArrayBuffer()))
  private def blocks(bs: Block*): ArrayBuffer[Block] = ArrayBuffer(bs*)

  /** Convenience accessor: the single function body block group. */
  def bodyOf(f: Func): BlockGroup = f.body.get

  def build(): Package = {
    // ---------------------------------------------------------------- types
    val tI32 = TypeImpl(TypeKind.Int32)
    val tI64 = TypeImpl(TypeKind.Int64)
    val tF64 = TypeImpl(TypeKind.Float64)
    val tBool = TypeImpl(TypeKind.Boolean)
    val tRune = TypeImpl(TypeKind.Rune)
    val tUnit = TypeImpl(TypeKind.Unit)
    val tStr = TypeImpl(TypeKind.CString)
    val tRefI32 = TypeImpl(TypeKind.Ref, ArrayBuffer(tI32))
    val tBoxI32 = TypeImpl(TypeKind.Box, ArrayBuffer(tI32))
    val tTuple = TypeImpl(TypeKind.Tuple, ArrayBuffer(tI32, tI64))
    val tRawArray = RawArrayType(tI32, 2)
    val tVArray = VArrayType(tI32, 4)
    val tGeneric = GenericType("T", "T", ArrayBuffer(tRefI32))
    // param types :+ return type
    val tMainFunc = FuncType(false, false, ArrayBuffer(tRefI32, tI32, tI64))
    val tHelperFunc = FuncType(false, false, ArrayBuffer(tRefI32, tI32, tUnit))

    // ---------------------------------------------------------------- defs
    // imported struct (IMPORTED attr); current-pkg struct, class, enum, extend
    val importedStruct = StructDef(
      attributes = Attribute.Imported | Attribute.Internal,
      identifier = "@N.Imp.ImpSt", srcCodeIdentifier = "ImpSt", packageName = "N.Imp",
      annoInfo = noAnno())
    val structTy = CustomType(Some(importedStruct))

    val structDef = StructDef(
      loc = Some(DebugLocation("S.cj", 1, Some(Pos(2, 1)), Some(Pos(6, 2)), ArrayBuffer())),
      attributes = Attribute.Public,
      identifier = "@N.Pkg.St", srcCodeIdentifier = "St", packageName = "N.Pkg",
      annoInfo = noAnno())
    val structInstTy = CustomType(Some(structDef))
    structInstTy.argTys += tI32

    val method = Func(
      attributes = Attribute.Public, tpe = Some(tHelperFunc),
      identifier = "@N.Pkg.St.m", srcCodeIdentifier = "m", rawMangledName = "@N.Pkg.St.m",
      packageName = "N.Pkg", annoInfo = noAnno(),
      propLoc = noLoc(), localId = 3L, blockId = 1L, blockGroupId = 1L)
    val memberVar = MemberVarInfo("x", "@N.Pkg.St.x", tI32, attributes = 0L, loc = noLoc(),
      annoInfo = noAnno(), outerDef = Some(structDef))
    val classDef = ClassDef(
      attributes = Attribute.Public,
      identifier = "@N.Pkg.Cl", srcCodeIdentifier = "Cl", packageName = "N.Pkg",
      methods = ArrayBuffer(method), instanceMemberVars = ArrayBuffer(memberVar),
      annoInfo = noAnno(),
      vtable = ArrayBuffer(VTableInType(
        structInstTy,
        ArrayBuffer(VirtualMethodInfo("m", tHelperFunc, ArrayBuffer(), Some(method), 0L,
          tHelperFunc, structInstTy, tUnit)))),
      isClass = true)
    val classTy = CustomType(Some(classDef))

    val enumDef = EnumDef(
      attributes = Attribute.Public,
      identifier = "@N.Pkg.Color", srcCodeIdentifier = "Color", packageName = "N.Pkg",
      annoInfo = noAnno(),
      ctors = ArrayBuffer(EnumCtorInfo("Red", "@N.Pkg.Color.Red", tHelperFunc, None)))

    val extendDef = ExtendDef(
      attributes = Attribute.Public,
      identifier = "@N.Pkg.Ext", srcCodeIdentifier = "Ext", packageName = "N.Pkg",
      annoInfo = noAnno(),
      extendedType = Some(tRefI32))

    // ---------------------------------------------------------------- values
    val srcLoc = DebugLocation("Main.cj", 1, Some(Pos(3, 5)), Some(Pos(3, 20)), ArrayBuffer(1, 2, 3))

    val p0 = Parameter(tpe = Some(tRefI32), identifier = "%0", srcCodeIdentifier = "a",
      annoInfo = noAnno())
    val p1 = Parameter(tpe = Some(tI32), identifier = "%1", srcCodeIdentifier = "b",
      annoInfo = noAnno())

    // all literal kinds. NOTE: like the deserializer, each occurrence gets a
    // fresh literal object (shared literals would be deduped by the serializer
    // on the first pass and break byte stability).
    def boolLit(): BoolLiteral = BoolLiteral(tpe = Some(tBool), value = true)
    def runeLit(): RuneLiteral = RuneLiteral(tpe = Some(tRune), value = 65L)
    def strLit(): StringLiteral = StringLiteral(tpe = Some(tStr), value = "hi")
    def intLit(): IntLiteral = IntLiteral(tpe = Some(tI32), value = 1L)
    def floatLit(): FloatLiteral = FloatLiteral(tpe = Some(tF64), value = 1.5)
    def unitLit(): UnitLiteral = UnitLiteral(tpe = Some(tUnit))
    def nullLit(): NullLiteral = NullLiteral(tpe = Some(tRefI32))
    val litInt = intLit()
    val litFloat = floatLit()
    val litUnit = unitLit()
    val litNull = nullLit()

    val gv0 = GlobalVar(
      attributes = Attribute.Imported | Attribute.Internal, tpe = Some(tI32),
      identifier = "@N.Imp.gv", rawMangledName = "@N.Imp.gv", srcCodeIdentifier = "gv",
      packageName = "N.Imp", annoInfo = noAnno())
    val gv1 = GlobalVar(
      loc = Some(DebugLocation("Main.cj", 1, Some(Pos(3, 5)), Some(Pos(3, 20)), ArrayBuffer(1, 2, 3))),
      attributes = Attribute.Internal, tpe = Some(tI32),
      identifier = "@N.Pkg.gv", rawMangledName = "@N.Pkg.gv", srcCodeIdentifier = "gv",
      packageName = "N.Pkg", features = ArrayBuffer("k"), annoInfo = noAnno(),
      initializer = Some(litInt))

    // ---------------------------------------------------------------- main body
    val mainFunc = Func(
      attributes = Attribute.Public, tpe = Some(tMainFunc),
      identifier = "@N.Pkg.main", rawMangledName = "@N.Pkg.main", srcCodeIdentifier = "main",
      packageName = "N.Pkg", annoInfo = noAnno(), funcKind = FuncKind.MainEntry,
      params = ArrayBuffer(p0, p1), propLoc = noLoc(),
      localId = 20L, blockId = 4L, blockGroupId = 1L)
    p0.ownedFunc = Some(mainFunc)
    p1.ownedFunc = Some(mainFunc)

    val bg = BlockGroup(identifier = "0", ownedFunc = Some(mainFunc))
    val b0 = Block(identifier = "0", parentGroup = Some(bg))
    val b0c = Block(identifier = "4", parentGroup = Some(bg))
    val b1 = Block(identifier = "1", parentGroup = Some(bg))
    val b2 = Block(identifier = "2", parentGroup = Some(bg))
    val lp = Block(identifier = "3", parentGroup = Some(bg), isLandingPadBlock = true,
      exceptionCatchList = ArrayBuffer(tI32))
    bg.entryBlock = Some(b0)
    bg.blocks ++= blocks(b0, b0c, b1, b2, lp)
    b0.predecessors.clear() // entry
    b0c.predecessors ++= blocks(b0)
    b1.predecessors ++= blocks(b0c)
    b2.predecessors ++= blocks(b0c, b1)
    lp.predecessors ++= blocks(b0, b2)

    // b0: allocate, store, try-apply (block-final; handlers = [normal, exception])
    val lvAlloc = LocalVar(loc = Some(srcLoc), tpe = Some(tRefI32), identifier = "%2",
      isRetVal = true, srcCodeIdentifier = "r")
    val alloc = AllocateExpr(owner = Some(b0), resultVar = Some(lvAlloc), allocatedType = tI32)
    lvAlloc.associatedExpr = Some(alloc)

    val store = StoreExpr(
      owner = Some(b0),
      resultVar = Some(LocalVar(tpe = Some(tUnit), identifier = "%3")),
      value0 = intLit(), location0 = lvAlloc)

    val helper = Func(
      annos(LinkTypeInfo(Linkage.Internal)), attributes = Attribute.Internal,
      tpe = Some(tHelperFunc),
      identifier = "@N.Pkg.helper", rawMangledName = "@N.Pkg.helper", srcCodeIdentifier = "helper",
      packageName = "N.Pkg", annoInfo = noAnno(),
      propLoc = noLoc(), localId = 5L, blockId = 1L, blockGroupId = 1L)

    val lvApply = LocalVar(tpe = Some(tI64), identifier = "%4")
    val tryApply = ApplyExpr(
      owner = Some(b0), resultVar = Some(lvApply),
      instantiatedTypeArgs = ArrayBuffer(tI32), objType = Some(tRefI32),
      callee0 = helper, args0 = ArrayBuffer(p0, p1),
      isTry = true, handlerBlocks0 = ArrayBuffer(b0c, lp))

    b0.exprs ++= ArrayBuffer(alloc, store, tryApply)

    // b0c: numeric cast (throws), then multibranch
    val lvCast = LocalVar(annos(EnumCaseIndex(7L)), tpe = Some(tI64), identifier = "%5")
    val cast = NumericCastExpr(
      annos(NeedCheckCast(true)), owner = Some(b0c), resultVar = Some(lvCast),
      overflowStrategy = OverflowStrategy.Throwing, value0 = p1)

    val lvCond = LocalVar(tpe = Some(tBool), identifier = "%6")
    val mbranch = MultiBranchExpr(
      owner = Some(b0c), resultVar = Some(lvCond),
      condition0 = lvCast, caseValues = ArrayBuffer(0L, 1L),
      defaultBlock0 = b2, normalBlocks0 = ArrayBuffer(b1, b2))

    b0c.exprs ++= ArrayBuffer(cast, mbranch)

    // b1: constants of every literal kind, get-element, load, debug, goto
    val constBool = ConstantExpr(owner = Some(b1),
      resultVar = Some(LocalVar(tpe = Some(tBool), identifier = "%7")), literal0 = boolLit())
    val constRune = ConstantExpr(owner = Some(b1),
      resultVar = Some(LocalVar(tpe = Some(tRune), identifier = "%8")), literal0 = runeLit())
    val constStr = ConstantExpr(owner = Some(b1),
      resultVar = Some(LocalVar(tpe = Some(tStr), identifier = "%9")), literal0 = strLit())
    val constInt = ConstantExpr(owner = Some(b1),
      resultVar = Some(LocalVar(tpe = Some(tI32), identifier = "%10")), literal0 = litInt)
    val constFloat = ConstantExpr(owner = Some(b1),
      resultVar = Some(LocalVar(tpe = Some(tF64), identifier = "%11")), literal0 = litFloat)
    val constUnit = ConstantExpr(owner = Some(b1),
      resultVar = Some(LocalVar(tpe = Some(tUnit), identifier = "%12")), literal0 = litUnit)
    val constNull = ConstantExpr(owner = Some(b1),
      resultVar = Some(LocalVar(tpe = Some(tRefI32), identifier = "%13")), literal0 = nullLit())
    val elemRef = GetElementRefExpr(owner = Some(b1),
      resultVar = Some(LocalVar(tpe = Some(tRefI32), identifier = "%14")),
      base0 = lvAlloc, path = ArrayBuffer(0L, 1L))
    val load = LoadExpr(owner = Some(b1),
      resultVar = Some(LocalVar(tpe = Some(tI32), identifier = "%15")),
      location0 = elemRef.resultVar.get)
    val lvInvoke = LocalVar(tpe = Some(tI64), identifier = "%25")
    val invoke = InvokeExpr(owner = Some(b1), resultVar = Some(lvInvoke),
      objType = Some(structInstTy), callee0 = method, caller0 = lvAlloc,
      args0 = ArrayBuffer(p0, litInt), overflowStrategy = OverflowStrategy.Na)
    val dbg = DebugExpr(owner = Some(b1), srcCodeName = "b", value0 = load.resultVar.get)
    val goto = GotoExpr(owner = Some(b1), destination0 = b2)
    b1.exprs ++= ArrayBuffer(constBool, constRune, constStr, constInt, constFloat, constUnit,
      constNull, elemRef, load, invoke, dbg, goto)

    // b2: tuple, lambda (with own body), branch back into b1
    val tuple = TupleExpr(owner = Some(b2),
      resultVar = Some(LocalVar(tpe = Some(tTuple), identifier = "%16")),
      elementValues0 = ArrayBuffer(p1, intLit()))

    val lambda = LambdaExpr(owner = Some(b2),
      resultVar = Some(LocalVar(tpe = Some(tHelperFunc), identifier = "%17")),
      funcTy = tHelperFunc, isLocalFunc = true, identifier = "@N.Pkg.main.lambda.1",
      srcCodeName = "<closure>", genericTypeParams = ArrayBuffer(tGeneric))
    val lambdaBg = BlockGroup(identifier = "0", ownedExpression = Some(lambda))
    val lb0 = Block(identifier = "0", parentGroup = Some(lambdaBg))
    lambdaBg.entryBlock = Some(lb0)
    lambdaBg.blocks ++= blocks(lb0)
    val lP0 = Parameter(attributes = Attribute.Readonly, tpe = Some(tRefI32), identifier = "%18",
      ownedLambda = Some(lambda), srcCodeIdentifier = "x", annoInfo = noAnno())
    lambda.params += lP0
    val lvLambdaAlloc = LocalVar(tpe = Some(tRefI32), identifier = "%19", isRetVal = true)
    val lambdaAlloc = AllocateExpr(owner = Some(lb0), resultVar = Some(lvLambdaAlloc),
      allocatedType = tI32)
    lvLambdaAlloc.associatedExpr = Some(lambdaAlloc)
    val lambdaCall = ApplyExpr(owner = Some(lb0),
      resultVar = Some(LocalVar(tpe = Some(tI64), identifier = "%20")),
      callee0 = lambda.resultVar.get, args0 = ArrayBuffer(lP0, intLit()))
    lb0.exprs ++= ArrayBuffer(lambdaAlloc, lambdaCall)
    lambda.body = Some(lambdaBg)
    lambda.retVal = Some(lvLambdaAlloc)

    val lvLt = LocalVar(tpe = Some(tBool), identifier = "%26")
    val lt = BinaryExpr(owner = Some(b2), resultVar = Some(lvLt), op = BinaryOp.Lt,
      overflowStrategy = OverflowStrategy.Wrapping, leftOperand0 = p1, rightOperand0 = intLit())
    val lvNot = LocalVar(tpe = Some(tBool), identifier = "%27")
    val not = UnaryExpr(owner = Some(b2), resultVar = Some(lvNot), op = UnaryOp.Not,
      overflowStrategy = OverflowStrategy.Saturating, operand0 = lvLt)
    val lvBox = LocalVar(tpe = Some(tBoxI32), identifier = "%28")
    val boxCast = CastExpr(annos(NeedCheckCast(true)), owner = Some(b2), resultVar = Some(lvBox),
      op = CastOp.StaticCast, value0 = lvAlloc)

    val branch = BranchExpr(owner = Some(b2), condition0 = boolLit(), trueBlock0 = b1,
      falseBlock0 = b2, sourceExpr = SourceExprTag.IfExpr)
    b2.exprs ++= ArrayBuffer(tuple, lambda, lt, not, boxCast, branch)

    // lp: landing pad — get exception + begin-catch intrinsic + raise
    val lvExc = LocalVar(tpe = Some(tRefI32), identifier = "%21")
    val getExc = GetExceptionExpr(owner = Some(lp), resultVar = Some(lvExc))
    val lvCatch = LocalVar(tpe = Some(tUnit), identifier = "%22")
    val beginCatch = IntrinsicExpr(owner = Some(lp), resultVar = Some(lvCatch),
      intrinsicKind = IntrinsicKind.BeginCatch, args0 = ArrayBuffer(lvExc))
    val raise = RaiseExceptionExpr(owner = Some(lp),
      exceptionValue0 = lvExc, exceptionBlock0 = Some(lp))
    lp.exprs ++= ArrayBuffer(getExc, beginCatch, raise)

    mainFunc.body = Some(bg)
    mainFunc.retVal = Some(lvAlloc)

    // helper body: loads the global var and returns
    val helperBg = BlockGroup(identifier = "0", ownedFunc = Some(helper))
    val hb0 = Block(identifier = "0", parentGroup = Some(helperBg))
    helperBg.entryBlock = Some(hb0)
    helperBg.blocks ++= blocks(hb0)
    val hP0 = Parameter(tpe = Some(tRefI32), identifier = "%0", srcCodeIdentifier = "x",
      annoInfo = noAnno())
    helper.params += hP0
    val hLoad = LoadExpr(owner = Some(hb0),
      resultVar = Some(LocalVar(tpe = Some(tI32), identifier = "%1")), location0 = gv1)
    val hRet = StoreExpr(owner = Some(hb0),
      resultVar = Some(LocalVar(tpe = Some(tUnit), identifier = "%2")),
      value0 = hLoad.resultVar.get, location0 = gv1)
    hb0.exprs ++= ArrayBuffer(hLoad, hRet)
    helper.body = Some(helperBg)
    hP0.ownedFunc = Some(helper)
    val helperRetLv = LocalVar(tpe = Some(tI32), identifier = "%1", isRetVal = true)
    helperRetLv.associatedExpr = Some(hLoad)
    helper.retVal = Some(helperRetLv)

    // package-init func: minimal body
    val initFunc = Func(
      attributes = Attribute.CompilerAdd | Attribute.NoReflectInfo | Attribute.NoInline,
      tpe = Some(FuncType(false, false, ArrayBuffer(tUnit))),
      identifier = "@N.Pkg.main$init", rawMangledName = "@N.Pkg.main$init",
      srcCodeIdentifier = "main$init", packageName = "N.Pkg",
      funcKind = FuncKind.GlobalVarInit, annoInfo = noAnno(), propLoc = noLoc(),
      localId = 1L, blockId = 1L, blockGroupId = 1L)
    val initBg = BlockGroup(identifier = "0", ownedFunc = Some(initFunc))
    val ib0 = Block(identifier = "0", parentGroup = Some(initBg))
    initBg.entryBlock = Some(ib0)
    initBg.blocks ++= blocks(ib0)
    val initExit = ExitExpr(owner = Some(ib0))
    ib0.exprs += initExit
    initFunc.body = Some(initBg)

    // ---------------------------------------------------------------- package
    val pkg = new Package()
    pkg.name = "default"
    pkg.accessLevel = PackageAccessLevel.Public
    pkg.phase = Phase.Opt
    pkg.packageInitFunc = Some(initFunc)
    pkg.globalVars ++= ArrayBuffer(gv0, gv1)
    pkg.globalFuncs ++= ArrayBuffer(mainFunc, helper, initFunc, method)
    pkg.importedStructs += importedStruct
    pkg.structs += structDef
    pkg.classes += classDef
    pkg.enums += enumDef
    pkg.extendsDefs += extendDef
    pkg
  }
}
