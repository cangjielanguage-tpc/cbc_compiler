/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.lowering

import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.common.Language
import com.huawei.excelsior.common.Language.CANGJIE
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.DCEComponent
import com.huawei.excelsior.jet.compiler.opt.middle.transformations.xi.XiTransform
import com.huawei.excelsior.jet.compiler.options.BoolOption.NoNewArrayCopy
import com.huawei.excelsior.jet.compiler.{Domain, Env, RTConst, RTSProc, StatsKind, symlevel}
import com.huawei.excelsior.jet.compiler.symlevel.{MethodReferenceAccessKind, SignatureType, TypeKind}
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.{ArrayType, ClassType}
import com.huawei.excelsior.jet.compiler.types.References.{Point, ReferenceApprox, UpperBounded}
import com.huawei.excelsior.jet.compiler.util.Sets

import scala.PartialFunction.{cond, condOpt}
import scala.collection.mutable.ArrayBuffer

/** - Optimize new array allocator + arraycopy patterns with copying allocator to eliminate redundant zeroing.
  * - Replace AllocArray.newArray intrinsics.
  *
  * @author ikireev
  */
trait NewArrayAllocations extends XiTransform with DCEComponent { self: Universe =>

  import MethodReferenceAccessKind._

  private lazy val cangjieArrayCopy = if (Env.languagePack.supports(CANGJIE)) env.getRTSProc(RTSProc.CJ_UncheckedArrayCopy) else null
  private lazy val javaArrayCopy = if (Env.languagePack.supports(Language.JAVA)) RT.Arraycopy.arraycopy else null

  lazy val loops = cfg.loops

  private def replaceNewArrayRT(): Boolean = {
    val newArrayRTs = all[NewArrayRT]
    val replaced = newArrayRTs.nonEmpty
    for (x <- newArrayRTs) {
      val (allocProc, arg, classArg) = x.klass match {
        case getClass @ GetClass(obj) =>
          (RTSProc.JR_NEW_REFARRAY_SAMETYPE, obj, getClass)

        case _: ClassObject =>
          shouldNotReachHere("newArrayInstance for ClassObject(constType) should be replaced with newArray in SimplifyComponent")

        case tpe =>
          (RTSProc.JR_NEW_REFARRAY_OFTYPE, tpe, null)
      }
      stats.count(StatsKind.NewArrayCopy, s"NewArrayInstance replaced by $allocProc", x)
      replaceByCode(x) { RTSCall(allocProc)(arg, x.length) }

      if (classArg != null && !classArg.hasValueUses) {
        strikeOut(classArg)
      }
    }

    replaced
  }

  private object ArrayCopy {
    private def unsafeUses(array: Node, call: Call) =
      array.valueOutEdges.flatMap { edge =>
        edge.target match {
          case `call` if call.invokeArgIdx(edge) == 2 => None
          case HasInMemory(inMem) if call dominates inMem => None
          case _: Phi if call dominates edge.usePoint => None
          case x => Some(x)
        }
      }

    private def assignCompatible(srcType: ReferenceApprox, newType: ReferenceApprox): Boolean = {
      cond((newType, srcType)) {
        case (Point(newArrType: ArrayType, _), UpperBounded(srcArrType: ArrayType, _)) =>
          newArrType >= srcArrType
        case (Point(newArrType: ArrayType, _), UpperBounded(srcArrType: ClassType, _)) =>
          // TODO: JET-17408
          newArrType.symType.isCangjieArray && srcArrType.symType == typeProvider.getAJObjectType // Array<T> hack
      }
    }

    def supportedType(call: Call, dst: NewArray): Boolean = {
      val allocType = dst.allocType.symType
      val supported = allocType.isJBCArray || // Java or Scala array
        allocType.isCangjieArray && (
          allocType.getArrayElemType.isPrimitive || // Primitive Cangjie array
          dst.uninitialized) // Uninitialized record array with no traced refs
      if (!supported) {
        stats.count(StatsKind.NewArrayCopy, s"unsupported array type ${allocType.getName}", call)
      }
      supported
    }

    def checkCopy(call: Call, src: Node, dst: SpinalNode, dstPos: Node): Boolean = {
      dstPos match {
        case IntegralConst(0) => // ok
        case _ =>
          stats.count(StatsKind.NewArrayCopy, s"dst offset ${dstPos.name}", call)
          return false
      }

      val uses = unsafeUses(dst, call)
      if (uses.nonEmpty) {
        stats.count(StatsKind.NewArrayCopy, s"unsafe uses ${uses.map(_.name).mkString("(", ",", ")")}", call)
        return false
      }

      if ((call.block != dst.block) && !loops.inSameLoop(call.block, dst.block)) {
        stats.count(StatsKind.NewArrayCopy, s"non linear path", call)
        return false
      }

      if (!safeToMergeXPointsOf(dst, call)) {
        stats.count(StatsKind.NewArrayCopy, s"diff xHandlers", call)
        return false
      }

      val srcType = nodeTypeAt(src, call)
      val newType = nodeTypeAt(dst, call)
      assert(!newType.mayBeNull)
      if (!assignCompatible(srcType, newType)) {
        stats.count(StatsKind.NewArrayCopy, s"incompatible array types $srcType & $newType", call)
        return false
      }

      true
    }

    def unapply(call: Call) : Option[(Node, Node, Node, Node, Node)] = condOpt(call) {
      case CallMethod(_, STATIC, Seq(src, srcPos, array, dstPos, count)) => (array, src, srcPos, dstPos, count)
    }
  }

  private case class CopyAnchors(call: Call, newArray: SpinalNode) {
    /** Reevaluate copy arguments due to changes after xi-transformation and var processing. */
    def args() = (call, newArray) match {
      case (ArrayCopy(_, src, srcPos, _, count), array @ NewArray(_, Seq(length))) =>
        CopyArgs(array.allocType, length, src, srcPos, count, LConst(0))
      case (ArrayCopy(_, src, srcPos, _, count), array: NewArrayFill) =>
        CopyArgs(array.allocType, array.length, src, srcPos, count, array.value)
    }
  }

  private case class CopyArgs(allocType: SignatureType, length: Node, src: Node, srcPos: Node, count: Node, value: Node)

  private def tryOptimizeOneNewArrayCopy(call: Call): Option[CopyAnchors] = {
    import ArrayCopy.*
    condOpt(call) {
      case ArrayCopy(array @ NewArray(_, Seq(length)), src, srcPos, dstPos, count) if supportedType(call, array) && checkCopy(call, src, array, dstPos) =>
        CopyAnchors(call, array)
      case ArrayCopy(array: NewArrayFill, src, srcPos, dstPos, count) if checkCopy(call, src, array, dstPos) =>
        CopyAnchors(call, array)
    }
  }

  private def sizeCheck(elemSize: Int, copyLength: Node): Node = {
    val actualSize = Mul(copyLength, IntegralConst(copyLength.tpe)(elemSize))
    val maxSize = IntegralConst(copyLength.tpe)(RTConst.SmallAJAllocator.MAX_LENGTH_OF_SPECIALIZED_PRIM_ARRAY.intValue)
    Cmp(copyLength.tpe, Condition.LE)(actualSize, maxSize)
  }

  private def insertNewArrayCopy(array: SpinalNode, call: Call)(arrayCopy: => NewArrayCopy): NewArrayCopy = {
    val newInitialized = insertCodeBefore(call) {
      arrayCopy
    }
    strikeOut(call)

    for (n <- collect[ControlledNode](array.valueUses) if n.inCtrl == array) {
      n.inCtrl = newInitialized
    }
    strikeOutWithValueUses(array, newInitialized)

    newInitialized
  }

  // TODO: JET-17408
  def optimizeNewArrayAllocations(): Boolean = {
    var changed = replaceNewArrayRT()

    if (changed) dbgPrinter.debugNodes("all graph after new array rt replacing")

    if (!isO1Compiled && !env.enabled(NoNewArrayCopy)) {
      val arrayCopies = all[Call].collect {
        case call if call.targetRef.hasMethod && (call.targetRef.method == javaArrayCopy || call.targetRef.method == cangjieArrayCopy) =>
          tryOptimizeOneNewArrayCopy(call)
      }.flatten.toList

      // Actual array copy pairs without duplicates.
      val copiesToVersion = ArrayBuffer.empty[CopyAnchors]

      // We are not able to version one block twice per transformation session.
      // Conflicting array copy pairs will not be optimized.
      // Note that duplicates in pairs will be filtered out as conflicting.
      val versionedBlocks = Sets[Block].newMSet

      for (copy <- arrayCopies) {
        val subGraph = versioningSubGraph(copy.newArray, copy.call)
        if (!(subGraph exists versionedBlocks.contains)) {
          copiesToVersion += copy
          versionedBlocks ++= subGraph
        } else {
          stats.count(StatsKind.NewArrayCopy, s"unable to version array copy", copy.call)
        }
      }

      if (copiesToVersion.nonEmpty) {
        xiTransform { scheduler =>
          for (copy <- copiesToVersion; args = copy.args()) {
            scheduler.version(PredicateConstructor.atom(sizeCheck(args.allocType.getArrayElemType.symType.size, args.length)),
              copy.newArray, copy.call)
          }
        }

        dbgPrinter.debugNodes("all graph after new array alloc versioning")

        completeSSA()

        dbgPrinter.debugNodes("all graph after new array alloc ssa completion")

        eliminateUnreachableCode()
        eliminateDeadCode()

        dbgPrinter.debugNodes("all graph after new array alloc uce-dce")

        for (copy <- copiesToVersion; args = copy.args()) {
          val uninitialized = copy.newArray match {
            case array: NewArray =>
              stats.count(StatsKind.NewArrayCopy, s"optimized NewArray type ${array.allocType.symType.getName}", copy.call)
              array.uninitialized
            case array: NewArrayFill =>
              stats.count(StatsKind.NewArrayCopy, s"optimized NewArrayFill type ${array.allocType.symType.getName}", copy.call)
              false
          }

          val newArrayCopy = insertNewArrayCopy(copy.newArray, copy.call) {
            NewArrayCopy(args.allocType)(args.length, args.src, args.srcPos, args.count, args.value)
          }
          newArrayCopy.uninitialized = uninitialized
        }

        changed = true
        dbgPrinter.debugNodes("all graph after new array alloc optimizing")

        checkIRConsistency(CheckLevels.Desirable)
      }
    }

    changed
  }

}
