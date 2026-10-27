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
      annos(), None, Attribute.Imported | Attribute.Internal, "ImpSt", "@N.Imp.ImpSt", "N.Imp",
      None, None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), noAnno(), ArrayBuffer(), None,
      isCStruct = false)
    val structTy = CustomType(Some(importedStruct))

    val structDef = StructDef(
      annos(), Some(DebugLocation("S.cj", 1, Some(Pos(2, 1)), Some(Pos(6, 2)), ArrayBuffer())), Attribute.Public,
      "St", "@N.Pkg.St", "N.Pkg", None, None, ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), ArrayBuffer(),
      noAnno(), ArrayBuffer(), None, isCStruct = false)
    val structInstTy = CustomType(Some(structDef))
    structInstTy.argTys += tI32

    val method = Func(
      annos(), None, Attribute.Public, Some(tHelperFunc), "@N.Pkg.St.m", "m", "@N.Pkg.St.m", "N.Pkg",
      None, ArrayBuffer(), noAnno(), None, FuncKind.Default, false, false, None,
      ArrayBuffer(), None, None, ArrayBuffer(), None, noLoc(), 3L, 1L, 1L)
    val memberVar = MemberVarInfo("x", "@N.Pkg.St.x", tI32, 0L, noLoc(), noAnno(), None, Some(structDef))
    val classDef = ClassDef(
      annos(), None, Attribute.Public, "Cl", "@N.Pkg.Cl", "N.Pkg", None, None,
      ArrayBuffer(method), ArrayBuffer(), ArrayBuffer(memberVar), ArrayBuffer(), noAnno(),
      ArrayBuffer(VTableInType(
        structInstTy,
        ArrayBuffer(VirtualMethodInfo("m", tHelperFunc, ArrayBuffer(), Some(method), 0L,
          tHelperFunc, structInstTy, tUnit)))),
      None, isClass = true, isAnnotation = false, ArrayBuffer(), None)
    val classTy = CustomType(Some(classDef))

    val enumDef = EnumDef(
      annos(), None, Attribute.Public, "Color", "@N.Pkg.Color", "N.Pkg", None, None,
      ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), noAnno(), ArrayBuffer(), None,
      ArrayBuffer(EnumCtorInfo("Red", "@N.Pkg.Color.Red", tHelperFunc, None)), nonExhaustive = false)

    val extendDef = ExtendDef(
      annos(), None, Attribute.Public, "Ext", "@N.Pkg.Ext", "N.Pkg", None, None,
      ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), ArrayBuffer(), noAnno(), ArrayBuffer(), None,
      Some(tRefI32), ArrayBuffer())

    // ---------------------------------------------------------------- values
    val srcLoc = DebugLocation("Main.cj", 1, Some(Pos(3, 5)), Some(Pos(3, 20)), ArrayBuffer(1, 2, 3))

    val p0 = Parameter(annos(), None, 0L, Some(tRefI32), "%0", None, None, "a", noAnno())
    val p1 = Parameter(annos(), None, 0L, Some(tI32), "%1", None, None, "b", noAnno())

    // all literal kinds. NOTE: like the deserializer, each occurrence gets a
    // fresh literal object (shared literals would be deduped by the serializer
    // on the first pass and break byte stability).
    def boolLit(): BoolLiteral = BoolLiteral(annos(), None, 0L, Some(tBool), "", true)
    def runeLit(): RuneLiteral = RuneLiteral(annos(), None, 0L, Some(tRune), "", 65L)
    def strLit(): StringLiteral = StringLiteral(annos(), None, 0L, Some(tStr), "", "hi")
    def intLit(): IntLiteral = IntLiteral(annos(), None, 0L, Some(tI32), "", 1L)
    def floatLit(): FloatLiteral = FloatLiteral(annos(), None, 0L, Some(tF64), "", 1.5)
    def unitLit(): UnitLiteral = UnitLiteral(annos(), None, 0L, Some(tUnit), "")
    def nullLit(): NullLiteral = NullLiteral(annos(), None, 0L, Some(tRefI32), "")
    val litInt = intLit()
    val litFloat = floatLit()
    val litUnit = unitLit()
    val litNull = nullLit()

    val gv0 = GlobalVar(annos(), None, Attribute.Imported | Attribute.Internal, Some(tI32),
      "@N.Imp.gv", "gv", "@N.Imp.gv", "N.Imp", None, ArrayBuffer(), noAnno(), None)
    val gv1 = GlobalVar(annos(),
      Some(DebugLocation("Main.cj", 1, Some(Pos(3, 5)), Some(Pos(3, 20)), ArrayBuffer(1, 2, 3))), Attribute.Internal, Some(tI32),
      "@N.Pkg.gv", "gv", "@N.Pkg.gv", "N.Pkg", None, ArrayBuffer("k"), noAnno(), Some(litInt))

    // ---------------------------------------------------------------- main body
    val mainFunc = Func(
      annos(), None, Attribute.Public, Some(tMainFunc), "@N.Pkg.main", "main", "@N.Pkg.main",
      "N.Pkg", None, ArrayBuffer(), noAnno(), None, FuncKind.MainEntry, false, false, None,
      ArrayBuffer(), None, None, ArrayBuffer(p0, p1), None, noLoc(), 20L, 4L, 1L)
    p0.ownedFunc = Some(mainFunc)
    p1.ownedFunc = Some(mainFunc)

    val bg = BlockGroup(annos(), None, 0L, None, "0", None, ArrayBuffer(), Some(mainFunc), None)
    val b0 = Block(annos(), None, 0L, None, "0", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    val b0c = Block(annos(), None, 0L, None, "4", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    val b1 = Block(annos(), None, 0L, None, "1", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    val b2 = Block(annos(), None, 0L, None, "2", Some(bg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    val lp = Block(annos(), None, 0L, None, "3", Some(bg), ArrayBuffer(), ArrayBuffer(), true, ArrayBuffer(tI32))
    bg.entryBlock = Some(b0)
    bg.blocks ++= blocks(b0, b0c, b1, b2, lp)
    b0.predecessors.clear() // entry
    b0c.predecessors ++= blocks(b0)
    b1.predecessors ++= blocks(b0c)
    b2.predecessors ++= blocks(b0c, b1)
    lp.predecessors ++= blocks(b0, b2)

    // b0: allocate, store, try-apply (block-final; handlers = [normal, exception])
    val lvAlloc = LocalVar(annos(), Some(srcLoc), 0L, Some(tRefI32), "%2", None, isRetVal = true, "r")
    val alloc = AllocateExpr(annos(), None, 0L, Some(b0), ArrayBuffer(), Some(lvAlloc), tI32,
      isTry = false, ArrayBuffer())
    lvAlloc.associatedExpr = Some(alloc)

    val store = StoreExpr(annos(DebugLocationInfoForWarning(
      DebugLocation("Main.cj", 1, Some(Pos(3, 5)), Some(Pos(3, 20)), ArrayBuffer(1, 2, 3)))), None, 0L, Some(b0), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tUnit), "%3", None, isRetVal = false, "")),
      intLit(), lvAlloc)

    val helper = Func(
      annos(LinkTypeInfo(Linkage.Internal)), None, Attribute.Internal, Some(tHelperFunc),
      "@N.Pkg.helper", "helper", "@N.Pkg.helper", "N.Pkg", None, ArrayBuffer(), noAnno(), None, FuncKind.Default, false, false, None,
      ArrayBuffer(), None, None, ArrayBuffer(), None, noLoc(), 5L, 1L, 1L)

    val lvApply = LocalVar(annos(), None, 0L, Some(tI64), "%4", None, isRetVal = false, "")
    val tryApply = ApplyExpr(annos(), None, 0L, Some(b0), ArrayBuffer(), Some(lvApply),
      ArrayBuffer(tI32), Some(tRefI32), helper, ArrayBuffer(p0, p1), isSuperCall = false,
      isTry = true, ArrayBuffer(b0c, lp))

    b0.exprs ++= ArrayBuffer(alloc, store, tryApply)

    // b0c: numeric cast (throws), then multibranch
    val lvCast = LocalVar(annos(EnumCaseIndex(7L)), None, 0L, Some(tI64), "%5", None, isRetVal = false, "")
    val cast = NumericCastExpr(annos(NeedCheckCast(true)), None, 0L, Some(b0c), ArrayBuffer(), Some(lvCast),
      OverflowStrategy.Throwing, p1, isTry = false, ArrayBuffer())

    val lvCond = LocalVar(annos(), None, 0L, Some(tBool), "%6", None, isRetVal = false, "")
    val mbranch = MultiBranchExpr(annos(), None, 0L, Some(b0c), ArrayBuffer(), Some(lvCond),
      lvCast, ArrayBuffer(0L, 1L), b2, ArrayBuffer(b1, b2))

    b0c.exprs ++= ArrayBuffer(cast, mbranch)

    // b1: constants of every literal kind, get-element, load, debug, goto
    val constBool = ConstantExpr(annos(), None, 0L, Some(b1), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tBool), "%7", None, isRetVal = false, "")), boolLit())
    val constRune = ConstantExpr(annos(), None, 0L, Some(b1), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tRune), "%8", None, isRetVal = false, "")), runeLit())
    val constStr = ConstantExpr(annos(), None, 0L, Some(b1), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tStr), "%9", None, isRetVal = false, "")), strLit())
    val constInt = ConstantExpr(annos(), None, 0L, Some(b1), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tI32), "%10", None, isRetVal = false, "")), litInt)
    val constFloat = ConstantExpr(annos(), None, 0L, Some(b1), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tF64), "%11", None, isRetVal = false, "")), litFloat)
    val constUnit = ConstantExpr(annos(), None, 0L, Some(b1), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tUnit), "%12", None, isRetVal = false, "")), unitLit())
    val constNull = ConstantExpr(annos(), None, 0L, Some(b1), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tRefI32), "%13", None, isRetVal = false, "")), nullLit())
    val elemRef = GetElementRefExpr(annos(), None, 0L, Some(b1), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tRefI32), "%14", None, isRetVal = false, "")),
      lvAlloc, ArrayBuffer(0L, 1L))
    val load = LoadExpr(annos(), None, 0L, Some(b1), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tI32), "%15", None, isRetVal = false, "")), elemRef.resultVar.get)
    val lvInvoke = LocalVar(annos(), None, 0L, Some(tI64), "%25", None, isRetVal = false, "")
    val invoke = InvokeExpr(annos(), None, 0L, Some(b1), ArrayBuffer(), Some(lvInvoke),
      ArrayBuffer(), Some(structInstTy), method, lvAlloc, ArrayBuffer(p0, litInt),
      OverflowStrategy.Na, isTry = false, ArrayBuffer())
    val dbg = DebugExpr(annos(), None, 0L, Some(b1), ArrayBuffer(), None, "b", load.resultVar.get)
    val goto = GotoExpr(annos(), None, 0L, Some(b1), ArrayBuffer(), None, b2)
    b1.exprs ++= ArrayBuffer(constBool, constRune, constStr, constInt, constFloat, constUnit,
      constNull, elemRef, load, invoke, dbg, goto)

    // b2: tuple, lambda (with own body), branch back into b1
    val tuple = TupleExpr(annos(), None, 0L, Some(b2), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tTuple), "%16", None, isRetVal = false, "")),
      ArrayBuffer(p1, intLit()))

    val lambda = LambdaExpr(annos(), None, 0L, Some(b2), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tHelperFunc), "%17", None, isRetVal = false, "")),
      tHelperFunc, isLocalFunc = true, "@N.Pkg.main.lambda.1", "<closure>",
      ArrayBuffer(), ArrayBuffer(tGeneric), None, None, isCompileTimeValue = false)
    val lambdaBg = BlockGroup(annos(), None, 0L, None, "0", None, ArrayBuffer(), None, Some(lambda))
    val lb0 = Block(annos(), None, 0L, None, "0", Some(lambdaBg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    lambdaBg.entryBlock = Some(lb0)
    lambdaBg.blocks ++= blocks(lb0)
    val lP0 = Parameter(annos(), None, Attribute.Readonly, Some(tRefI32), "%18", None, Some(lambda), "x", noAnno())
    lambda.params += lP0
    val lvLambdaAlloc = LocalVar(annos(), None, 0L, Some(tRefI32), "%19", None, isRetVal = true, "")
    val lambdaAlloc = AllocateExpr(annos(), None, 0L, Some(lb0), ArrayBuffer(), Some(lvLambdaAlloc),
      tI32, isTry = false, ArrayBuffer())
    lvLambdaAlloc.associatedExpr = Some(lambdaAlloc)
    val lambdaCall = ApplyExpr(annos(), None, 0L, Some(lb0), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tI64), "%20", None, isRetVal = false, "")),
      ArrayBuffer(), None, lambda.resultVar.get, ArrayBuffer(lP0, intLit()), isSuperCall = false,
      isTry = false, ArrayBuffer())
    lb0.exprs ++= ArrayBuffer(lambdaAlloc, lambdaCall)
    lambda.body = Some(lambdaBg)
    lambda.retVal = Some(lvLambdaAlloc)

    val lvLt = LocalVar(annos(), None, 0L, Some(tBool), "%26", None, isRetVal = false, "")
    val lt = BinaryExpr(annos(), None, 0L, Some(b2), ArrayBuffer(), Some(lvLt), BinaryOp.Lt,
      OverflowStrategy.Wrapping, p1, intLit(), isTry = false, ArrayBuffer())
    val lvNot = LocalVar(annos(), None, 0L, Some(tBool), "%27", None, isRetVal = false, "")
    val not = UnaryExpr(annos(), None, 0L, Some(b2), ArrayBuffer(), Some(lvNot), UnaryOp.Not,
      OverflowStrategy.Saturating, lvLt, isTry = false, ArrayBuffer())
    val lvBox = LocalVar(annos(), None, 0L, Some(tBoxI32), "%28", None, isRetVal = false, "")
    val boxCast = CastExpr(annos(NeedCheckCast(true)), None, 0L, Some(b2), ArrayBuffer(), Some(lvBox),
      CastOp.StaticCast, lvAlloc)

    val branch = BranchExpr(annos(), None, 0L, Some(b2), ArrayBuffer(), None, boolLit(), b1, b2,
      SourceExprTag.IfExpr)
    b2.exprs ++= ArrayBuffer(tuple, lambda, lt, not, boxCast, branch)

    // lp: landing pad — get exception + begin-catch intrinsic + raise
    val lvExc = LocalVar(annos(), None, 0L, Some(tRefI32), "%21", None, isRetVal = false, "")
    val getExc = GetExceptionExpr(annos(), None, 0L, Some(lp), ArrayBuffer(), Some(lvExc))
    val lvCatch = LocalVar(annos(), None, 0L, Some(tUnit), "%22", None, isRetVal = false, "")
    val beginCatch = IntrinsicExpr(annos(), None, 0L, Some(lp), ArrayBuffer(), Some(lvCatch),
      ArrayBuffer(), None, IntrinsicKind.BeginCatch, ArrayBuffer(lvExc), isTry = false, ArrayBuffer())
    val raise = RaiseExceptionExpr(annos(), None, 0L, Some(lp), ArrayBuffer(), None,
      lvExc, Some(lp))
    lp.exprs ++= ArrayBuffer(getExc, beginCatch, raise)

    mainFunc.body = Some(bg)
    mainFunc.retVal = Some(lvAlloc)

    // helper body: loads the global var and returns
    val helperBg = BlockGroup(annos(), None, 0L, None, "0", None, ArrayBuffer(), Some(helper), None)
    val hb0 = Block(annos(), None, 0L, None, "0", Some(helperBg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    helperBg.entryBlock = Some(hb0)
    helperBg.blocks ++= blocks(hb0)
    val hP0 = Parameter(annos(), None, 0L, Some(tRefI32), "%0", None, None, "x", noAnno())
    helper.params += hP0
    val hLoad = LoadExpr(annos(), None, 0L, Some(hb0), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tI32), "%1", None, isRetVal = false, "")), gv1)
    val hRet = StoreExpr(annos(), None, 0L, Some(hb0), ArrayBuffer(),
      Some(LocalVar(annos(), None, 0L, Some(tUnit), "%2", None, isRetVal = false, "")),
      hLoad.resultVar.get, gv1)
    hb0.exprs ++= ArrayBuffer(hLoad, hRet)
    helper.body = Some(helperBg)
    hP0.ownedFunc = Some(helper)
    val helperRetLv = LocalVar(annos(), None, 0L, Some(tI32), "%1", None, isRetVal = true, "")
    helperRetLv.associatedExpr = Some(hLoad)
    helper.retVal = Some(helperRetLv)

    // package-init func: minimal body
    val initFunc = Func(
      annos(), None, Attribute.CompilerAdd | Attribute.NoReflectInfo | Attribute.NoInline,
      Some(FuncType(false, false, ArrayBuffer(tUnit))), "@N.Pkg.main$init", "main$init",
      "@N.Pkg.main$init", "N.Pkg", None, ArrayBuffer(), noAnno(), None, FuncKind.GlobalVarInit, false, false, None, ArrayBuffer(), None, None,
      ArrayBuffer(), None, noLoc(), 1L, 1L, 1L)
    val initBg = BlockGroup(annos(), None, 0L, None, "0", None, ArrayBuffer(), Some(initFunc), None)
    val ib0 = Block(annos(), None, 0L, None, "0", Some(initBg), ArrayBuffer(), ArrayBuffer(), false, ArrayBuffer())
    initBg.entryBlock = Some(ib0)
    initBg.blocks ++= blocks(ib0)
    val initExit = ExitExpr(annos(), None, 0L, Some(ib0), ArrayBuffer(), None)
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
