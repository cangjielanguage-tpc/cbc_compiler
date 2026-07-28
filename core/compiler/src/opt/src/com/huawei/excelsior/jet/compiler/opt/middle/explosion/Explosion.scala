/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.explosion

import com.huawei.excelsior.common.Arch.CBC
import com.huawei.excelsior.jet.compiler.StatsKind
import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.Env.targetArch
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.LivenessAnalysis
import com.huawei.excelsior.jet.compiler.opt.middle.escape.EscapeAnalysis
import com.huawei.excelsior.jet.compiler.symlevel.{Field, SignatureType, Type as SymType}
import com.huawei.excelsior.jet.util.ScalaCollections.*
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.util.Log.Kind
import com.huawei.excelsior.jet.compiler.util.Log
import com.huawei.excelsior.jet.util.{Closure, DisjointSet, Worklist}

import scala.PartialFunction.*
import scala.annotation.nowarn
import scala.collection.mutable

/** Explosion optimization.
  *
  * @author conwor
  * @author liontiger
  * @author haitaka
  */
// TODO: remove when scala 3 is supported (see https://github.com/scala/bug/issues/4440)
@nowarn("msg=The outer reference in this type test cannot be checked at run time")
trait Explosion extends EscapeAnalysis with LivenessAnalysis { self: Universe =>

  object Explosion {
    def log = Log(Kind.Explosion)
  }

  object ConstantArrayGet {
    def unapply(n: ArrayGet): Option[(Node, Long)] = condOpt(n) {
      case ArrayGet(_, _, arr, IntegralConst(i)) => (arr, i)
    }
  }

  object ConstantArrayPut {
    def unapply(n: SpinalMemoryNode): Option[(Node, Long, Node)] = condOpt(n) {
      case ArrayPut(arr, IntegralConst(i), value) => (arr, i, value)
    }
  }

  object AnyConstantArrayPut {
    def unapply(n: SpinalMemoryNode): Option[(Node, Seq[Long])] = condOpt(n) {
      case ConstantArrayPut(arr, i, _) => (arr, Seq(i))
      case f: ArrayFill => (f.array, 0L until f.size.toLong ensuring (_.nonEmpty))
      case f: AJArrayFill => (f.array, Seq())
    }
  }

  object ConstantArrayOp {
    def unapply(n: TypedArrayOperation): Option[(Node, Seq[Long])] = condOpt(n) {
      case ConstantArrayGet(arr, i) => (arr, Seq(i))
      case AnyConstantArrayPut(arr, indexes) => (arr, indexes)
    }
  }

  object EnrichedArrayOp {
    def unapply(n: Node): Option[SignatureType] = condOpt(n) {
      case f: ArrayGet => f.enrichedElemType
      case f: ArrayPut => f.enrichedElemType
      case f: AJArrayFill => f.enrichedElemType
    }
  }

  def allocHasSideEffects(n: Node) = n match {
    case node: AnyNewArray =>
      // Can strike out, if all dimensions are positive
      node.lengths forall {
        case IntegralConst(v) if v >= 0 => false
        case _ => true
      }
    case _: AnyNewClass => false
    case b: BoxedValue => b.hasSideEffects
    case _: StackAlloc => false
    case _: Phi => false
    case _ => shouldNotReachHere(n)
  }

  /** Checks whether the object's reference is unique immediately after the node execution.
    * E.g. valueOf(_) is not unique because it may return pre-cached objects.
    */
  def isUniqueValue(explosive: Node): Boolean = explosive match {
    case _: AnyNew => true
    case _: BoxedValue => false
    case _: StackAlloc => true
    case _ => shouldNotReachHere(explosive)
  }

  def isExplosiveArray(newArray: AnyNewArray) = cond(newArray.lengths) {
    case Seq(IConst(10240000 | Int.MaxValue)) =>
      // Huge arrays created in JCK tests expr01501 & expr01502 (issue JET-7337).
      false

    case Seq(_) =>
      // currently only single-dimensional arrays are supported
      true
  }

  def isExplosionOfCangjieStringAllowed: Boolean = {
    // Stack alloc for string record should not be exploded in CBC
    // in order not to expose fields of String implementation.
    if (targetArch == CBC) {
      return false
    }

    // We should not explode stack alloc for string record earlier than serialization,
    // since serialized IR could be used for both AOT and CBC targetArch.
    CompilerPhase.PostInline <= currentPhase && currentPhase <= CompilerPhase.Lowering
  }

  private def isExplosive(n: Node) = cond(n) {
    case n: AnyNew if implicitlyEscapedType(n.allocType.symType) =>
      // It is better not to mess with these types.
      false

    case _: NewArrayCopy =>
      // TODO: maybe explode them
      false

    case _: NewString =>
      // We ignore them in current implementation.
      // TODO: explode key strings
      false

    case _: NewArrayMimic => false

    case n: AnyNewArray => isExplosiveArray(n)

    case _: AnyNewClass => true

    case n: BoxedValue =>
      n.valueUses.nonEmpty || !n.hasSideEffects

    case StackAlloc.Local(t) =>
      // Will check for memory uses later because there can be
      // both safe uses by other new thins of the same group
      // and unsafe uses.
      // And we don't want to explode stack alloc for string record in CBC too early,
      // more info in `isExplosionOfCangjieStringAllowed`.
      t.isThinClass || (t.isRecord
        && (Cangjie.Support.String.symType != t.symType || isExplosionOfCangjieStringAllowed)
        && !t.isUniversalGeneric) // FIXME-UG temporarily exclude UG types explosion

    case _: StackAlloc => false

    case n: AnyNew => shouldNotReachHere(s"unexpected AnyNew: $n")
  }

  private[explosion] object Get {
    def unapply(n: GetMemoryOperation) = condOpt(n) {
      case n: GetInstanceFieldOperation => n.obj
      case ConstantArrayGet(arr, _) => arr
    }
  }

  private[explosion] object Put {
    def unapply(n: PutMemoryOperation) = condOpt(n) {
      case n: PutField => (n.obj, n.inValue0)
      case n: ArrayPut => (n.array, n.inValue0)
    }
  }

  private[explosion] object ConstantPut {
    def unapply(n: PutMemoryOperation) = condOpt(n) {
      case n: PutField => (n.obj, n.inValue0)
      case ConstantArrayPut(array, _, value) => (array, value)
    }
  }

  private[explosion] object Fill {
    def unapply(n: SpinalMemoryNode) = condOpt(n) {
      case n: ArrayFill => n.array
      case n: AJArrayFill => n.array
    }
  }

  def explodeAllObjects(collectFailStats: Boolean = false) = explodeObjects(all[SpinalNode] ++ all[StackAlloc], collectFailStats)

  def explodeObjects(objects: Iterator[Node], collectFailStats: Boolean = false): Boolean = {
    if (env.enabled(NoExplosion)) return false

    /** Collects all values interconnected with `n` through a web of phies. */
    def aliasesClosure(n: Node) = Closure(Seq[Node](n) ++ collect[Phi](n.valueUses)) {
      case p: Phi =>
        val phiUses = collect[Phi](p.valueUses)
        val (phiArgs, defs) = partitionPhies(p.argsSeq)
        val siblings = defs flatMap (d => collect[Phi](d.valueUses))
        phiUses ++ phiArgs ++ defs ++ siblings
      case _ => Iterator.empty
    }

    def partitionPhies(xs: Seq[Node]): (Seq[Phi], Seq[Node]) = xs partitionMap {
      case p: Phi => Left(p)
      case x => Right(x)
    }

    def logFailure(defs: Seq[Node], reasons: (String, Position.Owner)*): Unit = {
      if (collectFailStats) {
        Explosion.log("- failed explosion")
        for (n <- defs) {
          Explosion.log(s"  ${n.name}", n)
        }
        for ((msg, pos) <- reasons) {
          if (pos != null) {
            Explosion.log(s"  $msg", pos)
          } else {
            Explosion.log(s"  $msg")
          }
        }
      }
    }

    def analyzeUses(rawCandidates: Seq[Node]): (Worklist[Node], DisjointSet[Node]) = {
      val explosives = Worklist.empty[Node]
      val aliases = DisjointSet.empty[Node]

      {
        val candidates = Worklist.from(rawCandidates)
        for (candidate <- candidates.drain) {

          val candidateAliases = aliasesClosure(candidate)
          val (phies, defs) = partitionPhies(candidateAliases.toSeq)
          assert(defs contains candidate)

          def compatibleDef(n: Node) = n == candidate || candidates.contains(n)

          if (phies.nonEmpty && !env.enabled(PhiExplosion)) {
            logFailure(defs, ("Phi explosion disabled", candidate))

          } else if (defs exists (!compatibleDef(_))) {
            val (compatibleDefs, incompatibleDefs) = defs.partition(compatibleDef)
            val reasons = incompatibleDefs map (a => (s"incompatible alias: ${a.name}", a))
            logFailure(compatibleDefs, reasons*)

          } else {
            explosives appendAll candidateAliases
            aliases unionAll candidateAliases
          }

          candidates removeAll defs
        }
      }

      lazy val alive = calcCFGLiveness()

      // For the sake of correctness we have to ensure that no phi can live simultaneous with any of its arguments.
      // Otherwise on each and every assignment to a phi's field we would have to change a corresponding field in a corresponding phi argument
      // and vice versa.
      def livenessNotOverlap(n: Node, p: Phi) = {
        val phiLiveness = alive.in collect { case (k, v) if v contains p => k }
        phiLiveness forall { b =>
          if (b == p.block) {
            // ensure that there are no other phies with the same argument
            n.valueUses.filter(_.block == b).toSet subsetOf Set[Node](p)
          } else {
            !alive.in(b).contains(n)
          }
        }
      }

      // TODO: consider a better check
      lazy val uniqueValue = explosives.iterator.filterNot(_.isInstanceOf[Phi]) forall isUniqueValue

      def explosiveUse(e: Edge) = cond(e.target) {
        case Get(obj)        => assert(obj == e.source); true
        case ConstantPut(obj, value) => obj == e.source && !aliases.equiv(obj, value)
        case ArrayPut(arr, _, value) => arr == e.source && !aliases.equiv(arr, value) && (
            // Cannot explode array put at non-constant index,
            // if there are any reads of array elements.
            !aliases.equivElements(arr).flatMap(_.valueUses).exists(_.isInstanceOf[ArrayGetOperation])
        )

        case Fill(arr) => arr == e.source

        case u: ThinNew => u.addr == e.source

        case u: CopyStructure
          if !u.structureType.isDeferred && // TODO: JET-16112
             !u.structureType.symType.isVArray =>   // TODO: JET-16620
          assert(u.dst == e.source || u.src == e.source)
          true

        case u: ArrayIndexCheck => assert(u.array == e.source); true
        case _: Cmp             => uniqueValue

        case u: EscapeWriteBarrier.Instance => env.enabled(OptimizeWriteBarriers) && u.receiver == e.source

        case u: Phi => assert(explosives contains u); u.args forall (a => livenessNotOverlap(a, u))
      }

      for (e <- explosives.track) {
        if (!e.valueOutEdges.forall(explosiveUse)) {
          val badAliases = explosives.iterator.filter(aliases.equiv(_, e)).toSeq
          if (collectFailStats) {
            val badUses = badAliases.flatMap(_.valueOutEdges) collect { case e if !explosiveUse(e) => e.target }
            val reasons = badUses.distinct.map(u => (s"bad value use: ${u.name}", u))
            logFailure(badAliases.filterNot(_.isInstanceOf[Phi]), reasons*)
          }
          explosives removeAll badAliases
        }
      }

      (explosives, aliases)
    }

    sealed abstract class FieldLike
    case object EveryField extends FieldLike
    sealed abstract class ExactField extends FieldLike {
      def tpe: Type
    }
    case class ObjField(f: Field) extends ExactField {
      def tpe = ValueType.fromSig(f.getType, instantiateRich = true)
    }
    case class ArrayElement(i: Long, tpe: Type) extends ExactField
    case class ArraySliceField(name: String, tpe: Type) extends ExactField
    object ArraySliceField {
      def apply(f: Field): ArraySliceField = ArraySliceField(f.getName, ValueType(f.getType))
    }

    withIncrementalGCM {
      Explosion.log.inSession("explosion", codeUnit) {
        var changed = false

        val groupedCandidates = groupBy(objects filter isExplosive) {
          case AnyNewArray(t, size) => (t, size)
          case StackAlloc.Local(t) => t
          case n => n.proto
        }

        for {
          (_, candidates) <- groupedCandidates
          (explosives, aliases) = analyzeUses(candidates) if explosives.nonEmpty
        } {

          val (fieldOps, nonFieldOps) = explosives.iterator.flatMap(_.valueUses).toSeq.distinct partition {
            case Get(_) => true
            case ConstantPut(_, _) => true
            case _: ArrayPut => false
            case Fill(_) => true
            case _: CopyStructure => true
            case _ => false
          }

          // Set of aliases' representatives that must not be stricken out
          val cannotStrikeOut = mutable.Set.from(explosives.iterator filter allocHasSideEffects)

          nonFieldOps foreach {
            case op: SpinalNode => op match {
              // Eliminate nop-nodes
              case _: EscapeWriteBarrier.Instance | _: ArrayPut | _: ThinNew =>
                strikeOut(op)
                changed = true

              case op: ArrayIndexCheck =>
                cannotStrikeOut ++= (op.array match {
                  case arr: Phi => Phi.transitiveValueArgs(arr)
                  case arr => Seq(arr)
                })

              case op => shouldNotReachHere(op)
            }

            // Eliminate cmps
            // Explosive check ensures that there can be no references to allocNode except for allocNode itself
            case cmp: Cmp =>
              assert(explosives.iterator.filterNot(_.isInstanceOf[Phi]) forall isUniqueValue)
              // Note: const folding is a weak invariant, so we can't rely that cmp.l != cmp.r here
              val trueCond = if (cmp.l == cmp.r) Condition.EQ else Condition.NE
              cmp.replaceBy(ConstCondition(cmp.op == trueCond))
              changed = true

            // Phies will be exploded later
            case _: Phi =>

            case op => shouldNotReachHere(op)
          }

          def fields(op: Node): Seq[FieldLike] = op match {
            case op: FieldOperation if op.field.getDeclaringClass.isArraySlice => Seq(ArraySliceField(op.field))
            case op: FieldOperation => Seq(ObjField(op.field))
            case ConstantArrayOp(_, Seq()) => Seq(EveryField)
            case op @ ConstantArrayOp(_, indexes) =>
              val elemType = op match {
                case EnrichedArrayOp(tpe) if tpe.isCangjieType || !tpe.isInterface => tpe
                case _ => op.arrayType.getArrayElemType
              }
              indexes map (i => ArrayElement(i, ValueType(elemType, eopTypeForInterfaces = true, instantiateRich = true)))
            case op: CopyStructure =>
              asClassType(op.structureType).getFields.toSeq collect { case f if !f.isStatic && !f.getType.isZST =>
                if (op.structureType.isArraySliceLike) ArraySliceField(f) else ObjField(f)
              }
            case _ => shouldNotReachHere(op)
          }

          // Eliminate uses of allocation node

          if (fieldOps.nonEmpty) {

            // Because no explosive phi can live together with any of its arguments,
            // we can treat every phi as a creation of a copy of its arguments rather than an alias.
            // So we're going to replace phies with independent vars.

            splitCriticalEdges()
            changed = true

            val opsByField = toMultiMap(fieldOps flatMap { op => mapWith(fields(op))(_ => op) })
            val everyFieldOps = explosives.iterator ++: opsByField.getOrElse(EveryField, Seq())
            for (case (f: ExactField, ops) <- opsByField) {
              val tpe = f.tpe
              withNewVars { (newVar, assignAt, readAt) =>
                // define vars
                val vars = mapWith[Node, Var](explosives.iterator)(_ => newVar(tpe))
                for (op <- ops ++ everyFieldOps if op.isCommitted) op match {

                  // Replace phies with vars
                  case phi: Phi =>
                    for (case e @ ControlEdge(ctrl, _) <- phi.block.inEdges) {
                      val pred = Projection.skip(ctrl).asInstanceOf[LowerPoint]
                      val inVal = readAt(pred.inCtrl, vars(phi.phiArg(e)))
                      assignAt(inVal, vars(phi), inVal)
                    }

                  case copy: CopyStructure =>
                    val field = (f: @unchecked) match {
                      case ObjField(field) => field
                      case ArraySliceField(name, _) => asClassType(copy.structureType).findField(xstr(name))
                    }

                    def readScalarOrAddr(source: CopyStructure => Node) = {
                      val src = source(copy)
                      if (explosives contains src) {
                        readAt(copy.inCtrl, vars(src))
                      } else {
                        insertCodeBefore(copy) { GetField(field)(src) }
                      }
                    }

                    def assignScalar(value: Node): Unit = {
                      val dst = copy.dst
                      if (explosives contains dst) {
                        assignAt(copy.inCtrl, vars(dst), value)
                      } else {
                        insertCodeBefore(copy) { PutField(field)(dst, value) }
                      }
                    }

                    def assignFlat(valueAddr: Node) = {
                      val dstAddr = readScalarOrAddr(_.dst)
                      insertCodeBefore(copy) {
                        CopyStructure(field.getType)(dstAddr, valueAddr)
                      }
                    }

                    val valueOrAddr = readScalarOrAddr(_.src)
                    if (field.isAJFlat) {
                      assignFlat(valueOrAddr)
                    } else {
                      assignScalar(valueOrAddr)
                    }

                  case get @ Get(obj) =>
                    assert(explosives contains obj)
                    get.replaceBy(readAt(get.upperPoint, vars(obj)))

                  case put @ Put(obj, _) =>
                    assert(explosives contains obj)
                    assignAt(put.inCtrl, vars(obj), put.storedValue())

                  case fill @ Fill(obj) =>
                    assert(explosives contains obj)
                    val storedValue = fill match {
                      case fill: ArrayFill =>
                        val ArrayElement(idx, _) = f
                        IntegralConst(ValueType(fill.elemType))(fill.storedValues(Math.toIntExact(idx)))
                      case fill: AJArrayFill => fill.value
                      case _ => shouldNotReachHere(fill)
                    }
                    assignAt(fill.inCtrl, vars(obj), storedValue)

                  case allocNode =>
                    val initialValue = f match {
                      case ObjField(f) if f.isAJFlat =>
                        val fieldType = f.getType
                        if (fieldType.isThinClass || fieldType.isRecord) {
                          StackAlloc.Local(fieldType)
                        } else {
                          assert(fieldType.isPrimitive) // Address (struct)
                          StackAlloc.raw(f.size, f.alignment)
                        }

                      case _ => allocNode match {
                        case allocNode: AnyNewArray if allocNode.allocType.isRecordArray =>
                          assert(!allocNode.allocType.getArrayElemType.isZST)
                          StackAlloc.Local(allocNode.allocType.getArrayElemType)

                        case _: AnyNew | _: StackAlloc => ZeroValueNode(tpe)
                        case b: BoxedValue => b.primitiveValue() ensuring (_.tpe == tpe)
                        case _ => shouldNotReachHere(s"unexpected operation $allocNode")
                      }
                    }
                    val inCtrl = allocNode match {
                      case allocNode: SpinalNode => allocNode.inCtrl
                      case _: StackAlloc => entryBlock
                      case _ => shouldNotReachHere(s"unexpected operation $allocNode")
                    }
                    assignAt(inCtrl, vars(allocNode), initialValue)
                }
              }
            }
            // can't strike out ArrayFills earlier
            withoutRepinAfterStructuralChange {
              bulkReplace {
                collect[SpinalNode](fieldOps) foreach strikeOut
              }
            }
          }

          // Eliminate allocation nodes if possible
          // (let phi uses die on their own)
          for ((_, allocNodes) <- groupBy(explosives.iterator.filterNot(_.isInstanceOf[Phi]))(aliases.find)) {

            Explosion.log("- explosion")

            for (n <- allocNodes) {
              if (cannotStrikeOut(n)) {
                n match {
                  case n: AnyNewArray =>
                    val replacement = replaceByCode(n) {
                      NewArrayMimic(n.allocType, n.canThrow)(n.lengths*)
                    }
                    Explosion.log(s"  ${n.name}", n)
                    Explosion.log(s"    replaced by ${replacement.simpleName}")
                    stats.count(StatsKind.NewOptimization, s"successful explosion of ${n.simpleName}", n)
                    changed = true

                  case n: BoxedValue =>
                    assert(n.hasSideEffects)
                    // leave it be
                    Explosion.log(s"  ${n.name}", n)
                    Explosion.log(s"    preserved (has side-effects)")

                  case n => shouldNotReachHere(n)
                }

              } else {
                n match {
                  case n: SpinalNode => replaceValueUsesByNoValueAndStrikeOut(n)
                  case n: StackAlloc =>
                    n.replaceValueUsesByNoValue()
                    decommit(n)
                  case n => shouldNotReachHere(n)
                }
                changed = true

                Explosion.log(s"  ${n.name}", n)
                Explosion.log(s"    struck out successfully")
                stats.count(StatsKind.NewOptimization, s"successful explosion of ${n.simpleName}", n)
              }
            }
          }
        }

        changed
      }
    }
  }

  /** Simplified pass that performs only trivial (fast uses) explosion.
    *
    * This is strictly a compilation time optimization pass,
    * that explodes most trivial cases more efficiently than the general explosion pass.
    */
  def expressExplodeAllObjects(): Boolean = {
    if (env.enabled(NoExplosion) || !env.enabled(ExpressExplosion)) {
      return false
    }

    def isExpressExplosive(n: Node) = isExplosive(n) && !allocHasSideEffects(n) && n.valueUses.forall {
      case Put(obj, _)      => obj == n
      case Fill(arr)        => arr == n
      case u: ThinNew       => u.addr == n
      case u: CopyStructure => u.dst == n
      case _ => false
    }

    val explosives = (all[SpinalNode] ++ all[StackAlloc]) filter isExpressExplosive
    if (explosives.isEmpty) {
      return false
    }

    Explosion.log.inSession("express explosion", codeUnit) {
      for (allocNode <- explosives) {
        Explosion.log(s"- explosion of ${allocNode.name}", allocNode)
        stats.count(StatsKind.NewOptimization, s"successful explosion of ${allocNode.simpleName}", allocNode)

        allocNode.valueUses.distinct.asInstanceOf[Iterator[SpinalNode]].toSeq foreach strikeOut
        allocNode match {
          case n: SpinalNode => replaceValueUsesByNoValueAndStrikeOut(n)
          case n: StackAlloc =>
            n.replaceValueUsesByNoValue()
            decommit(n)
          case n => shouldNotReachHere(n)
        }

        Explosion.log(s"  struck out successfully")
      }

      true
    }
  }

}
