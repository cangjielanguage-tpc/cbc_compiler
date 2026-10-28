/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.patchgen

import com.huawei.excelsior.jet.compiler.chir.model.*
import scala.collection.mutable.{ArrayBuffer, LinkedHashMap, LinkedHashSet}

class PatchGenException(msg: String) extends RuntimeException(msg)

/** Port of C++ `PatchGen.{h,cpp}` (namespace PatchGenerator) against the
  * version-agnostic CHIR model.
  */
final class PatchGen(fileName: String, sourcePackage: Option[Package],
                     val patchedPackage: Package) {
  import PatchGen.*
  import PatchableName.GuardMethodKind
  import PatchableName.PackageInitAccessorKind

  private val markContext = new MarkContext

  private val guardVarsInitializer: Option[Func] = PatchGen.findGuardVarsInitializer(patchedPackage)
  private val guardClass: Option[ClassDef] = PatchGen.findGuardClass(patchedPackage)
  private val guardVar: Option[GlobalVar] = PatchGen.findGuardVar(patchedPackage)

  private var exceptionDef: Option[ClassDef] = None
  private var exceptionInitDef: Option[Func] = None
  private var patchClass: Option[ClassDef] = None

  locally {
    val tripleSet = guardVarsInitializer.isDefined && guardClass.isDefined && guardVar.isDefined
    val tripleUnset = guardVarsInitializer.isEmpty && guardClass.isEmpty && guardVar.isEmpty
    error(tripleSet || tripleUnset,
      "triple (guard class, guard var and guard vars initializer) is expected to be initialized or not initialized")

    for (importedClass <- patchedPackage.importedClasses if importedClass.srcCodeIdentifier == "Exception") {
      exceptionDef = Some(importedClass)
      for (method <- importedClass.methods
           if isConstructor(method) && method.params.size == 2 && isStringType(method.params(1).tpe)) {
        exceptionInitDef = Some(method)
      }
    }
    error(exceptionDef.isDefined, "unable to find Exception definition")
    error(exceptionInitDef.isDefined, "unable to find Exception.<init>(String) definition")
  }

  def createPatch(): Unit = {
    mark()
    markInternalFuncsAsNonImportedAreReachableFromPatchable()
    genPatch()
  }

  // =================================================================================
  // mark phase
  // =================================================================================

  private def mark(): Unit = {
    val patchedGlobalFuncs = patchedPackage.globalFuncs
    sourcePackage match {
      case Some(source) =>
        error(source.name == patchedPackage.name, "packages differ")

        markContext.markAsPatchable(patchedPackage.packageInitFunc)
        markContext.markAsPatchable(patchedPackage.packageLiteralInitFunc)

        val patchClassName = guardClass.map(g => PatchableName(g).genPatchClassName)

        val patchedTypeDefsSet = LinkedHashMap[Equality.CustomDefKey, CustomTypeDef]()
        for (typeDef <- patchedPackage.curPkgCustomTypeDefs) {
          patchedTypeDefsSet.update(Equality.customDefKey(typeDef), typeDef)
        }
        for (sourceTypeDef <- source.curPkgCustomTypeDefs) {
          val customKind = sourceTypeDef.kind

          // TODO support enums and extends later
          if (customKind == CustomDefKind.Class || customKind == CustomDefKind.Struct) {
            val patchedTypeDef = patchedTypeDefsSet.get(Equality.customDefKey(sourceTypeDef))
            val typeKind = if (customKind == CustomDefKind.Struct) "struct" else "class"
            Debug.log(typeKind + " " + DebugDump.withoutPrefix(sourceTypeDef.identifier))
            error(patchedTypeDef.isDefined,
              typeKind + " " + sourceTypeDef.srcCodeIdentifier + " was not found in patched version")

            if (patchClassName.isEmpty || patchClassName.get != sourceTypeDef.srcCodeIdentifier) {
              markContext.markTypeAsImported(sourceTypeDef.identifier.drop(1))
            }
            // C++ only logs the instance var names here ("TODO check if var was not removed")
            for (iv <- sourceTypeDef.instanceMemberVars) {
              Debug.log("instance var " + iv.name)
            }
          } else {
            Debug.log("Only class or struct type def are supported now, but " +
              DebugDump.withoutPrefix(sourceTypeDef.identifier) + " is not one of them")
          }
        }

        val sourceGlobalFuncs = source.globalFuncs
        val patchedGlobalFuncsSet = LinkedHashMap[Equality.FunctionKey, Func]()
        for (f <- patchedGlobalFuncs) {
          patchedGlobalFuncsSet.update(Equality.functionKey(f), f)
        }

        val packageName = patchedPackage.name
        // class instance/static methods are also included here
        val patchableGuardVarsInitializer = PatchableName(patchedPackage).genGuardVarInitializedName
        for (sourceFunc <- sourceGlobalFuncs) {
          val parentTypeDef = sourceFunc.declaredParent
          Debug.log {
            val parentStr = parentTypeDef match {
              case Some(_) => if ((sourceFunc.attributes & Attribute.Static) != 0L) "static " else ""
              case None    => "global "
            }
            "source " + parentStr + "func " + DebugDump.withoutPrefix(sourceFunc.identifier)
          }
          // All these functions are called in package inits, so no need to create diff for them.
          val funcKind = sourceFunc.funcKind
          if (packageName == sourceFunc.packageName &&
              (funcKind == FuncKind.GlobalVarInit || funcKind == FuncKind.AnnoFactory ||
                sourceFunc.srcCodeIdentifier == patchableGuardVarsInitializer)) {
            Debug.log("ignoring as one is global var initializer or annotation factory function " +
              "that belongs to the current package")
          } else {
            if (parentTypeDef.exists(d => patchClassName.contains(d.srcCodeIdentifier))) {
              // no need to compare patch stubs during mark phase
            } else {
              val patchedGlobalFunc = patchedGlobalFuncsSet.get(Equality.functionKey(sourceFunc))
              error(patchedGlobalFunc.isDefined,
                "func " + sourceFunc.identifier.drop(1) + " was not found in patched version")
              markFunc(sourceFunc, patchedGlobalFunc.get)
            }
          }
        }

        markFuncsAsNonPatchable(patchedGlobalFuncs, sourceGlobalFuncs.map(Equality.functionKey).toSet)

        val patchedGlobalVarsSet = LinkedHashMap[Equality.GlobalVarKey, GlobalVar]()
        for (v <- patchedPackage.globalVars) {
          patchedGlobalVarsSet.update(Equality.globalVarKey(v), v)
        }
        // class static vars are also included here
        for (sourceVar <- source.globalVars) {
          val varKind = if (sourceVar.declaredParent.isDefined) "static" else "global"
          Debug.log(varKind + " var " + DebugDump.withoutPrefix(sourceVar.identifier))
          val patchedGlobalVar = patchedGlobalVarsSet.get(Equality.globalVarKey(sourceVar))
          error(patchedGlobalVar.isDefined,
            varKind + " var " + sourceVar.identifier.drop(1) + " was not found in patched version")
          markContext.markVarAsImported(patchedGlobalVar.get)
        }

        // TODO provide other checks related to new method/field addition, etc.
      case None =>
        markFuncsAsNonPatchable(patchedGlobalFuncs, Set.empty)
    }
  }

  private def markFunc(sourceFunc: Func, patchedFunc: Func): Unit = {
    if (!sourceFunc.body.isDefined) {
      if (!sourceFunc.isImported) {
        markContext.markFuncAsImported(patchedFunc)
      }
      return
    }

    val isPatchableSourceFunc = isPatchable(sourceFunc)
    error(!(isPatchableSourceFunc ^ isPatchable(patchedFunc)),
      "function " + sourceFunc.srcCodeIdentifier + " must be patchable both in source and patched versions")
    error(!(sourceFunc.body.isDefined ^ patchedFunc.body.isDefined),
      "function " + sourceFunc.srcCodeIdentifier + " must have body both in source and patched versions")

    if (sourceFunc.attributes != patchedFunc.attributes) {
      error("attributes of non-patchable function " + sourceFunc.srcCodeIdentifier +
        " must stay unchanged in patched version")
    }

    if (patchedFunc.isLambda) {
      markContext.markFuncAsDeleted(patchedFunc)
    } else {
      markContext.markFuncAsImported(patchedFunc)
    }

    if (new Equality.FuncCodeEquality().areEqual(sourceFunc.body.get, patchedFunc.body.get)) {
      return
    }

    if (isPatchableSourceFunc) {
      markContext.markAsPatchable(patchedFunc)
    } else {
      error("non-patchable function " + sourceFunc.srcCodeIdentifier +
        " must stay unchanged in patched version")
    }
  }

  private def markFuncsAsNonPatchable(patchedGlobalFuncs: ArrayBuffer[Func],
                                      sourceGlobalFuncs: Set[Equality.FunctionKey]): Unit = {
    for (patchedFunc <- patchedGlobalFuncs) {
      Debug.log {
        val parentStr = patchedFunc.declaredParent match {
          case Some(_) => if ((patchedFunc.attributes & Attribute.Static) != 0L) "static " else ""
          case None    => "global "
        }
        "patched " + parentStr + "func " + DebugDump.withoutPrefix(patchedFunc.identifier)
      }
      if (isPatchable(patchedFunc) && !sourceGlobalFuncs.contains(Equality.functionKey(patchedFunc))) {
        markContext.markFuncToMakeNonPatchable(patchedFunc)
      }
    }
  }

  private def markInternalFuncsAsNonImportedAreReachableFromPatchable(): Unit = {
    val visited = LinkedHashSet[Func]()
    for (patchable <- markContext.patchables) {
      markInternalFuncAsNonImportedAreReachableFromPatchable(patchable, visited)
    }
  }

  private def markInternalFuncAsNonImportedAreReachableFromPatchable(func: Func,
                                                                     visited: LinkedHashSet[Func]): Unit = {
    ChirOps.visitFuncExprs(func) { e =>
      e match {
        case apply: ApplyExpr =>
          apply.callee match {
            case callee: Func if linkageOf(callee).contains(Linkage.Internal) =>
              if (callee.isLambda) {
                markContext.unmarkFuncAsDeleted(callee)
              } else {
                markContext.unmarkFuncAsImported(callee)
              }
              if (visited.add(callee)) {
                markInternalFuncAsNonImportedAreReachableFromPatchable(callee, visited)
              }
            case _ => ()
          }
        case _ => ()
      }
    }
  }

  // =================================================================================
  // gen patch phase
  // =================================================================================

  private def genPatch(): Unit = {
    Debug.log("======================= remove unused code phase =======================")
    makeNewPatchableFuncsAsNonPatchable()

    if (sourcePackage.isDefined) {
      Debug.log("======================= gen patch phase =======================")
      val patches = genPatches()
      importFuncs(patches)
      importVars()
      importTypes()
      removeRedundantFunctions()
      updatePackageInits()
    }

    // TODO if no patches are generated, remove guard classes, getters/setters,
    // guard var initializer (see $GF, $GVI, $GV in newFunc1 test)
  }

  /**
    * The function removes redundant functions from the package:
    *
    * - Removes empty initializers located in the current package. These kinds of
    *   initializers are removed during LLVM-bitcode codegen, but remain in CHIR.
    * - Removes the functions marked to be deleted, e.g. lambdas unreachable from
    *   patchable functions.
    */
  private def removeRedundantFunctions(): Unit = {
    def isEmptyInitializer(func: Func): Boolean = {
      if ((func.attributes & Attribute.Initializer) == 0L || func.isImported || !func.body.isDefined ||
          !linkageOf(func).contains(Linkage.Internal)) {
        return false
      }
      val blocks = func.body.get.blocks
      if (blocks.size != 1) {
        return false
      }
      val entryBlock = blocks.head
      if (nonTerminators(entryBlock).nonEmpty) {
        return false
      }
      terminatorOf(entryBlock).exists(_.isInstanceOf[ExitExpr])
    }

    def removeEmptyInitializersCalls(packageInit: Option[Func]): LinkedHashSet[String] = {
      val initializersToRemove = LinkedHashSet[String]()
      packageInit.foreach { init =>
        ChirOps.visitFuncExprs(init) { e =>
          e match {
            case apply: ApplyExpr =>
              apply.callee match {
                case func: Func if isEmptyInitializer(func) =>
                  initializersToRemove += func.identifier.drop(1)
                  ChirOps.removeSelfFromBlock(apply)
                case _ => ()
              }
            case _ => ()
          }
        }
      }
      initializersToRemove
    }

    val initializersToRemove = removeEmptyInitializersCalls(patchedPackage.packageInitFunc)
    initializersToRemove ++= removeEmptyInitializersCalls(patchedPackage.packageLiteralInitFunc)
    initializersToRemove ++= markContext.funcsToDelete

    val newGlobalFunctions = patchedPackage.globalFuncs.filter { func =>
      !initializersToRemove.contains(func.identifier.drop(1))
    }
    patchedPackage.globalFuncs.clear()
    patchedPackage.globalFuncs ++= newGlobalFunctions
  }

  /** Replace access to package init flags with the corresponding accessors' calls. */
  private def updatePackageInits(): Unit = {
    var packageInitFlag: Option[GlobalVar] = None
    var packageInitLiteralFlag: Option[GlobalVar] = None
    for (globalVar <- patchedPackage.globalVars
         if packageInitFlag.isEmpty || packageInitLiteralFlag.isEmpty) {
      val name = globalVar.srcCodeIdentifier
      if (name == GvPkgInitOnceFlag) {
        packageInitFlag = Some(globalVar)
      } else if (name == "has_invoked_pkg_init_literal") {
        packageInitLiteralFlag = Some(globalVar)
      }
    }
    error(packageInitFlag.isDefined, "unable to find package init flag")
    error(packageInitLiteralFlag.isDefined, "unable to find package init literal flag")

    val packagePatchableName = PatchableName(patchedPackage)

    var packageInitFlagGetter: Option[Func] = None
    var packageInitFlagSetter: Option[Func] = None
    var packageInitLiteralFlagGetter: Option[Func] = None
    var packageInitLiteralFlagSetter: Option[Func] = None
    for (globalFunc <- patchedPackage.globalFuncs
         if packageInitFlagGetter.isEmpty || packageInitFlagSetter.isEmpty ||
             packageInitLiteralFlagGetter.isEmpty || packageInitLiteralFlagSetter.isEmpty) {
      val name = globalFunc.srcCodeIdentifier
      if (name == packagePatchableName.genPackageInitFlagAccessor(PackageInitAccessorKind.PackageInit, isGetter = true)) {
        packageInitFlagGetter = Some(globalFunc)
      } else if (name == packagePatchableName.genPackageInitFlagAccessor(PackageInitAccessorKind.PackageInit, isGetter = false)) {
        packageInitFlagSetter = Some(globalFunc)
      } else if (name == packagePatchableName.genPackageInitFlagAccessor(PackageInitAccessorKind.PackageLiteralInit, isGetter = true)) {
        packageInitLiteralFlagGetter = Some(globalFunc)
      } else if (name == packagePatchableName.genPackageInitFlagAccessor(PackageInitAccessorKind.PackageLiteralInit, isGetter = false)) {
        packageInitLiteralFlagSetter = Some(globalFunc)
      }
    }
    error(packageInitFlagGetter.isDefined, "unable to find package init flag getter")
    error(packageInitFlagSetter.isDefined, "unable to find package init flag setter")
    error(packageInitLiteralFlagGetter.isDefined, "unable to find package init literal flag getter")
    error(packageInitLiteralFlagSetter.isDefined, "unable to find package init literal flag setter")

    def replaceFlagAccessToFlagAccessorCall(packageInit: Option[Func], flag: GlobalVar,
                                            getter: Option[Func], setter: Option[Func]): Unit =
      packageInit.foreach { init =>
        ChirOps.visitFuncExprs(init) { e =>
          e match {
            case load: LoadExpr if load.location eq flag =>
              val g = getter.get
              val replacement = newApplyCall(init, g, Seq.empty)
              ChirOps.replaceWith(load, replacement, patchedPackage)
            case store: StoreExpr if store.location eq flag =>
              val s = setter.get
              val replacement = newApplyCall(init, s, Seq(store.value))
              ChirOps.replaceWith(store, replacement, patchedPackage)
            case _ => ()
          }
        }
      }

    replaceFlagAccessToFlagAccessorCall(patchedPackage.packageInitFunc, packageInitFlag.get,
      packageInitFlagGetter, packageInitFlagSetter)
    replaceFlagAccessToFlagAccessorCall(patchedPackage.packageLiteralInitFunc, packageInitLiteralFlag.get,
      packageInitLiteralFlagGetter, packageInitLiteralFlagSetter)
  }

  /** Creates an `Apply` call expression (with a minted result) that is NOT yet
    * appended to any block (port of `builder->CreateExpression<Apply>` used
    * together with `ReplaceWith`).
    */
  private def newApplyCall(init: Func, callee: Func, args: Seq[Value]): ApplyExpr = {
    val topLevel = ChirOps.topLevelFunc(init.body.get)
      .getOrElse(throw new IllegalStateException("no top-level function for package init"))
    val block = topLevel.body.get.entryBlock.getOrElse(
      throw new IllegalStateException("package init has no entry block"))
    val retTy = callee.tpe.map(_.asInstanceOf[FuncType].returnType)
      .getOrElse(throw new IllegalStateException("callee without func type"))
    val replacement = ApplyExpr(owner = Some(block), callee0 = callee,
      args0 = ArrayBuffer.from(args))
    replacement.resultVar = Some(ChirOps.mintResult(topLevel, retTy, replacement))
    replacement
  }

  private def removeGuardChecks(patchable: Func): Unit = {
    Debug.log("patchable before guard checks removal: ")
    Debug.log(DebugDump.func(patchable))
    val patchableBody = patchable.body.get.entryBlock.get

    val patchableBlockTerminator = terminatorOf(patchableBody)
    error(patchableBlockTerminator.exists(_.isInstanceOf[BranchExpr]), "guard var flag check was not found")
    val entryBlockTerminator = patchableBlockTerminator.get.asInstanceOf[BranchExpr]

    val falseBlock = entryBlockTerminator.falseBlock
    val falseBlockTerminator = terminatorOf(falseBlock)
    error(falseBlockTerminator.exists(_.isInstanceOf[BranchExpr]), "guard check was not found")
    val falseBlockBranchTerminator = falseBlockTerminator.get.asInstanceOf[BranchExpr]

    val trueBlock = falseBlockBranchTerminator.trueBlock
    if (isPackageInit(patchable)) {
      ChirOps.setEntryBlock(patchable.body.get, entryBlockTerminator.trueBlock)
      ChirOps.removeSelfFromBlockGroup(patchableBody)

      val t = terminatorOf(trueBlock)
      error(t.exists(_.isInstanceOf[GotoExpr]), "guard check was not found")
      ChirOps.removeSelfFromBlockGroup(t.get.asInstanceOf[GotoExpr].destination)
      ChirOps.removeSelfFromBlockGroup(trueBlock)
    } else {
      ChirOps.removeSelfFromBlockGroup(trueBlock)

      val patchableName = PatchableName(patchable)

      val guardVarFlagName = patchableName.genGuardVarFlagName(forPackageInit = false)
      val guardVarFlag = patchedPackage.globalVars.find(_.srcCodeIdentifier == guardVarFlagName)
      error(guardVarFlag.isDefined, "guard var flag is expected to be initialized")

      val patchableExpressions = nonTerminators(patchableBody)
      val loadGuardVarCandidate = patchableExpressions(patchableExpressions.size - 2)
      val loadGuardVarOk = loadGuardVarCandidate match {
        case load: LoadExpr if load.location eq guardVarFlag.get => Some(load)
        case _ => None
      }
      loadGuardVarOk match {
        case Some(loadGuardVar) =>
          ChirOps.removeSelfFromBlock(loadGuardVar)
          val notExprCandidate = patchableExpressions.last
          val notExprOk = notExprCandidate match {
            case notExpr: UnaryExpr if notExpr.op == UnaryOp.Not &&
                (notExpr.operand eq loadGuardVar.resultVar.get) => Some(notExpr)
            case _ => None
          }
          notExprOk match {
            case Some(notExpr) => ChirOps.removeSelfFromBlock(notExpr)
            case None =>
              error("expected to delete NOT(Load(guardVar)) expr, but '" +
                DebugDump.expr(notExprCandidate) + "' was found")
          }
        case None =>
          error("expected to delete Load(guardVar) expr, but '" + DebugDump.expr(loadGuardVarCandidate) +
            "' was found")
      }

      val usersCodeBlock = entryBlockTerminator.trueBlock
      for (expr <- nonTerminators(usersCodeBlock)) {
        ChirOps.moveBefore(expr, entryBlockTerminator)
      }
      ChirOps.removeSelfFromBlock(entryBlockTerminator)
      terminatorOf(usersCodeBlock).foreach(t => ChirOps.moveTo(t, patchableBody))
      ChirOps.removeSelfFromBlockGroup(usersCodeBlock)
    }

    ChirOps.removeSelfFromBlockGroup(falseBlockBranchTerminator.falseBlock)
    ChirOps.removeSelfFromBlockGroup(falseBlock)

    Debug.log("patchable after guard checks removal: ")
    Debug.log(DebugDump.func(patchable))
  }

  /**
    * - Removes guard checks for the function
    * - Removes the corresponding guard var
    * - Removes the corresponding methods in guard/patch classes
    * - Marks the corresponding global functions as unreachable
    */
  private def makeNewPatchableFuncsAsNonPatchable(): Unit = {
    if (guardClass.isEmpty) {
      Debug.log("no guard class was found")
      if (Debug.enabled) {
        // C++ has this check inside #if DEBUG
        error(markContext.funcsToMakeNonPatchable.isEmpty,
          "patchable functions are found, but guard class wasn't initialized")
      }
      return
    }

    patchClass = PatchGen.findPatchClass(patchedPackage, guardClass.get)
    error(patchClass.isDefined, "patch class is expected to be initialized")
    Debug.log("patch class:")
    Debug.log(DebugDump.customDef(patchClass.get))

    for (func <- markContext.funcsToMakeNonPatchable.values) {
      removeGuardChecks(func)
      val patchableName = PatchableName(func)

      val guardVarFlagName = patchableName.genGuardVarFlagName(forPackageInit = false)
      val newGlobalVars = patchedPackage.globalVars.filter(_.srcCodeIdentifier != guardVarFlagName)
      patchedPackage.globalVars.clear()
      patchedPackage.globalVars ++= newGlobalVars

      val guardMethodName = patchableName.genGuardMethodName(GuardMethodKind.Plain)

      guardClass.get.methods = guardClass.get.methods.filter(_.srcCodeIdentifier != guardMethodName)
      patchClass.get.methods = patchClass.get.methods.filter(_.srcCodeIdentifier != guardMethodName)

      // Here we can't remove redundant global functions, as their references exist in vtable.
      // So marking these functions as unreachable lets cbc-compiler recognize and remove them.
      for (globalFunc <- patchedPackage.globalFuncs if globalFunc.srcCodeIdentifier == guardMethodName) {
        globalFunc.attributes = globalFunc.attributes | Attribute.Unreachable
      }
    }
  }

  private def genPatches(): LinkedHashSet[Func] = {
    val patches = LinkedHashSet[Func]()
    for (patchable <- markContext.patchables) {
      Debug.log("patchable before transformation: ")
      Debug.log(DebugDump.func(patchable))
      removeGuardChecks(patchable)

      if (isPackageInit(patchable)) {
        // nothing to patch: the package init is guarded as a whole
      } else {
        if (patchClass.isEmpty) {
          error(guardClass.isDefined, "guard class is expected be initialized")
          patchClass = PatchGen.findPatchClass(patchedPackage, guardClass.get)
          error(patchClass.isDefined, "patch class is expected to be initialized")
        }
        Debug.log("patch class:")
        Debug.log(DebugDump.customDef(patchClass.get))

        val patchableName = PatchableName(patchable)

        val patchMethodName = patchableName.genGuardMethodName(GuardMethodKind.Plain)
        val patchMethod = patchClass.get.methods.find(_.srcCodeIdentifier == patchMethodName)
        error(patchMethod.isDefined, "patch method is expected to be initialized")
        Debug.log("patch method before: ")
        Debug.log(DebugDump.func(patchMethod.get))

        val patchableBody = patchable.body.get.entryBlock.get

        val currRetVal = patchable.retVal
        val needRetVal = patchable.retVal.isDefined
        var retValIdx = -1
        var exprIdx = -1
        for (expr <- nonTerminators(patchableBody) if retValIdx < 0) {
          exprIdx += 1
          if (needRetVal && expr.resultVar.exists(_ eq currRetVal.get)) {
            retValIdx = exprIdx
          }
        }

        ChirOps.replaceBody(patchMethod.get, patchable.body.get)

        val patchEntryBlock = patchMethod.get.body.get.entryBlock.get
        val patchableKind = patchable.funcKind
        val isStructCtor = patchableKind == FuncKind.StructCtor || patchableKind == FuncKind.PrimalStructCtor

        val patchedFuncParams = funcParamsOf(patchEntryBlock.parentGroup.get)
        // We need to regenerate all the values' identifiers, otherwise there might be some
        // local var duplicates after patch generation. The only known approach is to clone
        // the existing expressions.
        val clonedTerminators = LinkedHashMap[Value, Value]()
        ChirOps.visitFuncExprs(patchMethod.get) { e =>
          var clonedExpr = e
          if (e.isTerminator) {
            // We have to handle all expression-like terminators as they might be used as
            // operands in other expressions.
            e.resultVar match {
              case Some(result) =>
                clonedExpr = ExprBuilder.cloneExpr(e, e.owner.get)
                ExprBuilder.copyBaseInfoFrom(clonedExpr, e)
                ChirOps.removeSelfFromBlock(e)
                clonedTerminators.update(result, clonedExpr.resultVar.get)
              case None => ()
            }
          } else {
            clonedExpr = ExprBuilder.cloneExpr(e, e.owner.get)
            ExprBuilder.copyBaseInfoFrom(clonedExpr, e)
            ChirOps.replaceWith(e, clonedExpr, patchedPackage)
          }
          for (operand <- ChirOps.operandsOf(clonedExpr).toList) {
            operand match {
              case op: Parameter =>
                // We have to update param indices as the patch function has its own receiver.
                var i = 0
                while (i < patchedFuncParams.size) {
                  if (patchedFuncParams(i).identifier == op.identifier) {
                    val newOperand = if (isStructCtor) patchedFuncParams(i) else patchedFuncParams(i + 1)
                    ChirOps.replaceOperand(clonedExpr, op, newOperand)
                    i = patchedFuncParams.size // break
                  } else {
                    i += 1
                  }
                }
              case _ =>
                clonedTerminators.get(operand).foreach { cloned =>
                  ChirOps.replaceOperand(clonedExpr, operand, cloned)
                }
            }
          }
        }

        if (isStructCtor) {
          val returnType = patchMethod.get.tpe.get.asInstanceOf[FuncType].returnType

          val topLevel = ChirOps.topLevelFunc(patchEntryBlock.parentGroup.get).get
          val refTy = TypeImpl(TypeKind.Ref, ArrayBuffer(returnType))
          val allocStruct = AllocateExpr(owner = Some(patchEntryBlock), allocatedType = returnType)
          allocStruct.resultVar = Some(ChirOps.mintResult(topLevel, refTy, allocStruct))
          ChirOps.moveBefore(allocStruct, patchEntryBlock.exprs.head)
          val allocatedRetVal = allocStruct.resultVar.get
          patchMethod.get.retVal = Some(allocatedRetVal)

          val thisParam = patchedFuncParams.head
          // update parameter indices in expression operands, as the patch is generated in a
          // separate class with its own receiver
          for (expr <- usersOf(thisParam) if !expr.isInstanceOf[DebugExpr]) {
            ChirOps.replaceOperand(expr, thisParam, allocatedRetVal)
          }
        } else {
          if (retValIdx >= 0) {
            val newRetVal = patchEntryBlock.exprs(retValIdx).resultVar.get
            patchMethod.get.retVal = Some(newRetVal)
          } else if (patchableKind == FuncKind.ClassCtor || patchableKind == FuncKind.PrimalStructCtor) {
            val topLevel = ChirOps.topLevelFunc(patchEntryBlock.parentGroup.get).get
            val unitTy = TypeImpl(TypeKind.Unit)
            val refTy = TypeImpl(TypeKind.Ref, ArrayBuffer(unitTy))
            val allocUnit = AllocateExpr(owner = Some(patchEntryBlock), allocatedType = unitTy)
            allocUnit.resultVar = Some(ChirOps.mintResult(topLevel, refTy, allocUnit))
            terminatorOf(patchEntryBlock).foreach(t => ChirOps.moveBefore(allocUnit, t))
            patchMethod.get.retVal = Some(allocUnit.resultVar.get)
          }
        }

        enableGuardVarFlag(patchableName)

        Debug.log("patch method after: ")
        Debug.log(DebugDump.func(patchMethod.get))
        patches += patchMethod.get
      }
    }
    if (patches.nonEmpty) {
      instantiatePatchClass()
    }
    patches
  }

  /** Initializes the patch class instance in the guard vars initializer. */
  private def instantiatePatchClass(): Unit = {
    val guardVarsInitFuncBody = guardVarsInitializer.get.body.get.entryBlock.get
    val guardVarsInitFuncTerminator = terminatorOf(guardVarsInitFuncBody)
      .getOrElse(throw new IllegalStateException("no terminator in guard vars initializer"))
    val topLevel = ChirOps.topLevelFunc(guardVarsInitFuncBody.parentGroup.get).get

    val patchClassType = patchClass.get.tpe.get
    val patchClassRefTy = TypeImpl(TypeKind.Ref, ArrayBuffer(patchClassType))
    val alloc = AllocateExpr(owner = Some(guardVarsInitFuncBody), allocatedType = patchClassType)
    alloc.resultVar = Some(ChirOps.mintResult(topLevel, patchClassRefTy, alloc))
    ChirOps.moveBefore(alloc, guardVarsInitFuncTerminator)

    val guardClassType = guardClass.get.tpe.get
    val guardClassRefTy = TypeImpl(TypeKind.Ref, ArrayBuffer(guardClassType))
    // CastOp.StaticCast round-trips as C++ ClassStaticCast (the serializer writes
    // CHIRExprKind_StaticCast and the deserializer reads it back as ClassStaticCast)
    val typeCast = CastExpr(owner = Some(guardVarsInitFuncBody), op = CastOp.StaticCast,
      value0 = alloc.resultVar.get)
    typeCast.resultVar = Some(ChirOps.mintResult(topLevel, guardClassRefTy, typeCast))
    ChirOps.moveAfter(typeCast, alloc)

    val boolTy = TypeImpl(TypeKind.Boolean)
    val falseExpr = ConstantExpr(owner = Some(guardVarsInitFuncBody),
      literal0 = BoolLiteral(tpe = Some(boolTy), value = false))
    falseExpr.resultVar = Some(ChirOps.mintResult(topLevel, boolTy, falseExpr))
    ChirOps.moveAfter(falseExpr, typeCast)

    val guardVarType = guardVar.get.tpe.get match {
      case t: TypeImpl if t.kind == TypeKind.Ref => t.argTys.head
      case t => t
    }
    val tuple = TupleExpr(owner = Some(guardVarsInitFuncBody),
      elementValues0 = ArrayBuffer[Value](falseExpr.resultVar.get, typeCast.resultVar.get))
    tuple.resultVar = Some(ChirOps.mintResult(topLevel, guardVarType, tuple))
    ChirOps.moveAfter(tuple, falseExpr)

    val unitTy = TypeImpl(TypeKind.Unit)
    val storeGuardVar = StoreExpr(owner = Some(guardVarsInitFuncBody),
      value0 = tuple.resultVar.get, location0 = guardVar.get)
    storeGuardVar.resultVar = Some(ChirOps.mintResult(topLevel, unitTy, storeGuardVar))
    ChirOps.moveAfter(storeGuardVar, tuple)
  }

  /** Sets the guard var flag to true in the guard vars initializer. */
  private def enableGuardVarFlag(patchableName: PatchableName): Unit = {
    val guardVarFlagName = patchableName.genGuardVarFlagName(forPackageInit = false)
    val guardVarFlag = patchedPackage.globalVars.find(_.srcCodeIdentifier == guardVarFlagName)
    error(guardVarFlag.isDefined, "guard var flag is expected to be initialized")

    val guardVarsInitFuncBody = guardVarsInitializer.get.body.get.entryBlock.get
    val guardVarsInitFuncTerminator = terminatorOf(guardVarsInitFuncBody)
      .getOrElse(throw new IllegalStateException("no terminator in guard vars initializer"))
    val topLevel = ChirOps.topLevelFunc(guardVarsInitFuncBody.parentGroup.get).get

    val boolTy = TypeImpl(TypeKind.Boolean)
    val trueExpr = ConstantExpr(owner = Some(guardVarsInitFuncBody),
      literal0 = BoolLiteral(tpe = Some(boolTy), value = true))
    trueExpr.resultVar = Some(ChirOps.mintResult(topLevel, boolTy, trueExpr))
    ChirOps.moveBefore(trueExpr, guardVarsInitFuncTerminator)

    val unitTy = TypeImpl(TypeKind.Unit)
    val storeToGuardVarFlag = StoreExpr(owner = Some(guardVarsInitFuncBody),
      value0 = trueExpr.resultVar.get, location0 = guardVarFlag.get)
    storeToGuardVarFlag.resultVar = Some(ChirOps.mintResult(topLevel, unitTy, storeToGuardVarFlag))
    ChirOps.moveAfter(storeToGuardVarFlag, trueExpr)
  }

  private def importFuncs(patches: LinkedHashSet[Func]): Unit = {
    for (func <- markContext.funcsToImport.values if !patches.contains(func)) {
      ChirOps.destroyFuncBody(func)
      func.attributes = func.attributes | Attribute.Imported
    }
  }

  private def importVars(): Unit = {
    // C++ AppendAttributeInfo(own) is a raw-attribute no-op; only IMPORTED matters
    for (varToImport <- markContext.varsToImport.values) {
      varToImport.attributes = varToImport.attributes | Attribute.Imported
    }
  }

  private def importTypes(): Unit = {
    val typesToImport = markContext.typesToImport
    val newClassDefs = ArrayBuffer[ClassDef]()
    val newStructDefs = ArrayBuffer[StructDef]()
    for (typeDef <- patchedPackage.curPkgCustomTypeDefs.toList) {
      val kind = typeDef.kind
      if (typesToImport.contains(typeDef.identifier.drop(1))) {
        // DefBase owns the mutable attributes var; the CustomTypeDef trait exposes it read-only
        typeDef.asInstanceOf[DefBase].attributes = typeDef.attributes | Attribute.Imported
        kind match {
          case CustomDefKind.Class => patchedPackage.importedClasses += typeDef.asInstanceOf[ClassDef]
          case CustomDefKind.Struct => patchedPackage.importedStructs += typeDef.asInstanceOf[StructDef]
          case CustomDefKind.Enum => () // TODO support
          case CustomDefKind.Extend => () // TODO support
        }
      } else {
        kind match {
          case CustomDefKind.Class => newClassDefs += typeDef.asInstanceOf[ClassDef]
          case CustomDefKind.Struct => newStructDefs += typeDef.asInstanceOf[StructDef]
          case CustomDefKind.Enum => () // TODO support
          case CustomDefKind.Extend => () // TODO support
        }
      }
    }
    patchedPackage.classes.clear()
    patchedPackage.classes ++= newClassDefs
    patchedPackage.structs.clear()
    patchedPackage.structs ++= newStructDefs
    // TODO support enums, extends
  }

  // =================================================================================
  // helpers
  // =================================================================================

  /** Port of `Block::GetTerminator`. */
  private def terminatorOf(block: Block): Option[Expression] =
    block.exprs.lastOption.filter(_.isTerminator)

  /** Port of `Block::GetNonTerminatorExpressions` (all expressions except the
    * last one, without checking whether the last one is a terminator).
    */
  private def nonTerminators(block: Block): Seq[Expression] =
    block.exprs.toSeq.dropRight(1)

  private def isPackageInit(func: Func): Boolean =
    patchedPackage.packageInitFunc.exists(_ eq func) || patchedPackage.packageLiteralInitFunc.exists(_ eq func)

  private def usersOf(v: Value): ArrayBuffer[Expression] = ChirOps.usersOf(v, patchedPackage)

  private def error(msg: String): Nothing = throw new PatchGenException(msg)

  private def error(condition: Boolean, msg: String): Unit = {
    if (!condition) {
      error(msg)
    }
  }
}

object PatchGen {
  /** C++ Utils.h: `GV_PKG_INIT_ONCE_FLAG`. */
  val GvPkgInitOnceFlag: String = "$has_applied_pkg_init_func"

  /** C++ `GetFuncParams(BlockGroup&)`. */
  def funcParamsOf(body: BlockGroup): ArrayBuffer[Parameter] = body.ownedFunc match {
    case Some(func) => func.params
    case None => body.ownedExpression.getOrElse(
      throw new IllegalStateException("block group without owner")).params
  }

  def isConstructor(func: Func): Boolean = {
    val ctorKind = func.funcKind == FuncKind.ClassCtor || func.funcKind == FuncKind.PrimalClassCtor ||
      func.funcKind == FuncKind.StructCtor || func.funcKind == FuncKind.PrimalStructCtor
    ctorKind && (func.attributes & Attribute.Static) == 0L
  }

  /** The model has no cached builtin types; "std.core" String is identified by name. */
  def isStringType(tpe: Option[Type]): Boolean = tpe match {
    case Some(t: CustomType) => t.customTypeDef match {
      case Some(d) => d.packageName == "std.core" && d.srcCodeIdentifier == "String"
      case None => false
    }
    case _ => false
  }

  def linkageOf(func: Func): Option[Linkage] =
    func.annos.collectFirst { case l: LinkTypeInfo => l.linkage }

  def findGuardVarsInitializer(packageDef: Package): Option[Func] = {
    val gvitName = PatchableName(packageDef).genGuardVarInitializedName
    packageDef.globalFuncs.find(f => f.body.isDefined && f.srcCodeIdentifier == gvitName)
  }

  def findGuardClass(packageDef: Package): Option[ClassDef] = {
    val gcName = PatchableName(packageDef).genGuardClassName(forPackageInit = false)
    packageDef.classes.find(_.srcCodeIdentifier == gcName)
  }

  def findPatchClass(packageDef: Package, guardClass: ClassDef): Option[ClassDef] = {
    val pcName = PatchableName(guardClass).genPatchClassName
    packageDef.classes.find(_.srcCodeIdentifier == pcName)
  }

  def findGuardVar(packageDef: Package): Option[GlobalVar] = {
    val gvName = PatchableName(packageDef).genGuardVarName(forPackageInit = false)
    packageDef.globalVars.find(_.srcCodeIdentifier == gvName)
  }

  /**
    * Returns true if the guard-check pattern exists in the entry block of the function:
    *
    * ```
    * %0: Bool = Load(@$funcPatchVarFlag)
    * %1: Bool = Not(%0)
    * Branch(%1, #x, #y)
    * ```
    */
  def isPatchable(func: Func): Boolean = {
    if (!func.body.isDefined) {
      return false
    }
    val entryBlock = func.body.get.entryBlock.get
    val terminator = entryBlock.exprs.lastOption
    if (!terminator.exists(_.isInstanceOf[BranchExpr])) {
      return false
    }
    val condition = terminator.get.asInstanceOf[BranchExpr].condition
    if (!condition.isInstanceOf[LocalVar]) {
      return false
    }
    var expr = condition.asInstanceOf[LocalVar].associatedExpr
    if (!expr.exists(_.isInstanceOf[UnaryExpr])) {
      return false
    }
    val unary = expr.get.asInstanceOf[UnaryExpr]
    if (unary.op != UnaryOp.Not) {
      return false
    }
    val operand = unary.operand
    if (!operand.isInstanceOf[LocalVar]) {
      return false
    }
    expr = operand.asInstanceOf[LocalVar].associatedExpr
    if (!expr.exists(_.isInstanceOf[LoadExpr])) {
      return false
    }
    val location = expr.get.asInstanceOf[LoadExpr].location
    if (!location.isInstanceOf[GlobalVar]) {
      return false
    }
    val globalVarName = location.asInstanceOf[GlobalVar].srcCodeIdentifier
    val guardVarFlagName = PatchableName(func).genGuardVarFlagName(forPackageInit = false)
    globalVarName == guardVarFlagName
  }

  def isPackageInit(func: Func, packageDef: Package): Boolean =
    packageDef.packageInitFunc.exists(_ eq func) || packageDef.packageLiteralInitFunc.exists(_ eq func)
}

/** Port of `MarkContext` (C++ PatchGen.h). */
final class MarkContext {
  val typesToImport = LinkedHashSet[String]()
  val funcsToImport = LinkedHashMap[Equality.FunctionKey, Func]()
  val funcsToMakeNonPatchable = LinkedHashMap[Equality.FunctionKey, Func]()
  val funcsToDelete = LinkedHashSet[String]()
  val varsToImport = LinkedHashMap[Equality.GlobalVarKey, GlobalVar]()
  val patchables = ArrayBuffer[Func]()

  def markTypeAsImported(typeToImport: String): Unit = {
    Debug.log("mark to import type " + typeToImport)
    typesToImport += typeToImport
  }

  def markFuncAsImported(funcToImport: Func): Unit = {
    Debug.log("mark to import function" + DebugDump.withoutPrefix(funcToImport.identifier))
    funcsToImport.update(Equality.functionKey(funcToImport), funcToImport)
  }

  def unmarkFuncAsImported(func: Func): Unit = {
    Debug.log("unmark to import function" + DebugDump.withoutPrefix(func.identifier))
    funcsToImport.remove(Equality.functionKey(func))
  }

  def markFuncToMakeNonPatchable(funcToMakeNonPatchable: Func): Unit = {
    Debug.log("mark function" + DebugDump.withoutPrefix(funcToMakeNonPatchable.identifier) + " as non-patchable")
    funcsToMakeNonPatchable.update(Equality.functionKey(funcToMakeNonPatchable), funcToMakeNonPatchable)
  }

  def markFuncAsDeleted(funcToDelete: Func): Unit = {
    Debug.log("mark to delete function" + DebugDump.withoutPrefix(funcToDelete.identifier))
    funcsToDelete += funcToDelete.identifier.drop(1)
  }

  def unmarkFuncAsDeleted(func: Func): Unit = {
    Debug.log("unmark to delete function" + DebugDump.withoutPrefix(func.identifier))
    funcsToDelete -= func.identifier.drop(1)
  }

  def markVarAsImported(varToImport: GlobalVar): Unit = {
    Debug.log("mark to import variable" + DebugDump.withoutPrefix(varToImport.identifier))
    varsToImport.update(Equality.globalVarKey(varToImport), varToImport)
  }

  def markAsPatchable(patched: Func): Unit = {
    Debug.log("mark as patchable function " + DebugDump.withoutPrefix(patched.identifier))
    patchables += patched
  }

  def markAsPatchable(patched: Option[Func]): Unit = patched.foreach(markAsPatchable)
}
