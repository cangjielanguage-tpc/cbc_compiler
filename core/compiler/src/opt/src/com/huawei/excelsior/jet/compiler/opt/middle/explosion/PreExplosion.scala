/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.explosion

import com.huawei.excelsior.jet.compiler.bytecode.Position
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.codeemitter.BarrierKind.STORE_STORE
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.opt.middle.{DCEComponent, UCEComponent}
import com.huawei.excelsior.jet.compiler.options.BoolOption.OptimizeWriteBarriers
import com.huawei.excelsior.jet.compiler.options.BoolOption.GenerateWriteBarriers
import com.huawei.excelsior.jet.compiler.options.{BoolOption, NumOption}
import com.huawei.excelsior.jet.compiler.symlevel.Type.asClassType
import com.huawei.excelsior.jet.compiler.util.Sets
import com.huawei.excelsior.jet.compiler.symlevel
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType

import scala.PartialFunction.cond
import scala.collection.mutable.ArrayBuffer

/** The aim of this optimization is to defer object allocation until the first use of its reference/identity.
  * As long as the object's reference has not escaped, all operations with its fields can be replaced with operations on SSA-variables.
  * It's said that object is pre-exploded on creation and then reconstructed before the reference escapes.
  *
  * This transformation duplicates object allocations in certain points of CFG such a way that after [[Explosion]]
  * of the original allocations it would look like that object was initially created in pre-exploded form and reconstructed later on.
  *
  * @author haitaka
  */
trait PreExplosion extends DCEComponent with ReconPlacement with Explosion {
  self: Universe with UCEComponent =>

  private def allInstanceFields(classType: symlevel.ClassType) = {
    classType.getFields.filterNot(x => x.isStatic || x.getSignature.isZST).toSeq
  }


  def preExplodeObjects(collectFailStats: Boolean = false): Boolean = {
    if (!env.enabled(BoolOption.PreExplosion) || env.enabled(BoolOption.NoExplosion) || currentPhase <= CompilerPhase.Serialization) return false

    Explosion.log.inSession("pre-explosion", codeUnit) {

      def logFailure(allocNode: Node, reasons: (String, Position.Owner)*): Unit = {
        if (collectFailStats) {
          Explosion.log(s"- failed pre-explosion of ${allocNode.name}", allocNode)
          for ((msg, pos) <- reasons) {
            if (pos != null) {
              Explosion.log(s"  $msg", pos)
            } else {
              Explosion.log(s"  $msg")
            }
          }
        }
      }

      object PreExplosive {
        def unapply(allocNode: SpinalNode): Option[(SpinalNode, SignatureType, Sets[Node]#QSet)] = {

          val explosive = cond(allocNode) {
            case n: AnyNew if implicitlyEscapedType(n.allocType.symType) =>
              // It is better not to mess with these types.
              false

            case n: New => allInstanceFields(asClassType(n.allocType)).forall(!_.isAJFlat) // TODO: support flat fields and record arrays reconstruction

            case n: NewArray => isExplosiveArray(n) && !allocHasSideEffects(n) && !n.allocType.isRecordArray

            case b: BoxedValue =>
              assert(!implicitlyEscapedType(b.boxType.symType))
              !b.hasSideEffects

            // TODO string pre-explosion
          }

          if (!explosive) return None

          val allocType = allocNode match {
            case n: AnyNew => n.allocType
            case b: BoxedValue => SignatureType.fromSymType(b.boxType.symType)
            case _ => shouldNotReachHere(allocNode)
          }

          val fieldsCount: Long = allocNode match {
            case _: New | _: BoxedValue => allInstanceFields(asClassType(allocType)).length
            case NewArray(_, Seq(IntegralConst(singleLength))) => singleLength
            case NewArray(_, lengths) => shouldNotReachHere(lengths)
          }

          if (fieldsCount > env.valueOf(NumOption.MaxPreExplosiveFields)) {
            logFailure(allocNode, (s"too many fields for pre-explosion: $fieldsCount", allocNode))
            return None
          }

          object ExplosiveUse {
            def unapply(node: Node): Boolean = cond(node) {
              case Get(obj) => obj == allocNode
              case ConstantPut(obj, value) => obj == allocNode && value != allocNode
              case Fill(arr) => arr == allocNode
              case _: Cmp => isUniqueValue(allocNode)
              case u: EscapeWriteBarrier.Instance => env.enabled(OptimizeWriteBarriers) && u.receiver == allocNode
            }
          }

          object ReferenceUse {
            def unapply(node: Node) = cond(node) {
              case _: AbstractCall | _: Return | _: Phi => true
              case u: PutMemoryOperation => u.inValue0 == allocNode
              case _: Cmp => !isUniqueValue(allocNode)
            }
          }

          val referenceUses = Sets[Node].newQSet
          val badUses = ArrayBuffer.empty[Node]

          allocNode.valueUses foreach {
            case ExplosiveUse() => // ok
            case u @ ReferenceUse() => referenceUses.add(u)
            case u =>
              if (collectFailStats) {
                badUses += u
              } else {
                return None
              }
          }

          if (badUses.nonEmpty) {
            val reasons = badUses.toSeq.map(u => (s"bad value use: ${u.name}", u))
            logFailure(allocNode, reasons*)
            return None
          }

          Some((allocNode, allocType, referenceUses))
        }
      }

      var changed = false
      val toExplode = ArrayBuffer.empty[SpinalNode]

      withIncrementalGCM {
        Explosion.log.inSession("pre-explosion", codeUnit) {
          // foreach is used instead of for(){} because PreExplosive extractor may have side effects such as logging
          // and we do not want them to be executed twice: for filtering and for extraction
          all[SpinalNode].toSeq.foreach { case PreExplosive(explosive, allocType, referenceUses) =>
            if (referenceUses.isEmpty) {
              // not a failure, just let the generic explosion deal with it
            } else if (liveBlocks(explosive) exists (_.hasXHandlers)) {
              val reasons = liveBlocks(explosive) filter (_.hasXHandlers) map (_ => (s"node lives alongside the x-handler", null))
              logFailure(explosive, reasons.toSeq*)
            } else {
              def oneOfExitsFromExplBlock(e: Edge) = explosive.block.succBlockEdges.size > 1 && explosive.block.succBlockEdges.contains(e)

              def inDifferentBlock(e: Edge) = e.source.block != explosive.block

              lazy val cold = findWarmAndColdBlocks()

              def isCold(e: Edge) = cold(e.source.block) || cold(e.target.block)

              // FIXME: Reconstruction inside sync-region requires special xHandler to properly exit the region.
              //        There might be no such handler in the reconstruction block.
              //        So for now we consider any MonitorEnter as reference use.
              val refUsePoints = (referenceUses.map(upperPoint) ++ all[MonitorEnter]).toSet[ControlNode]
              val reconEdges = findReconPoints(explosive, refUsePoints).toSeq

              if (reconEdges.nonEmpty && (reconEdges forall (e =>
                (inDifferentBlock(e) || oneOfExitsFromExplBlock(e))
                  && (env.enabled(BoolOption.ReconstructInHotBlocks) || isCold(e))))
              ) {
                Explosion.log(s"- pre-explosion of ${explosive.name}", explosive)
                changed = true

                toExplode += explosive

                def reconstructAfter(after: UpperPoint): (SpinalNode, SpinalNode) = {
                  insertCodeAfter(after, useDefaultHandler = true) {
                    require(!after.block.hasXHandlers)
                    withPos(explosive) {
                      explosive match {
                        case n: New =>
                          val r = New(n.allocType)()
                          val fields = allInstanceFields(asClassType(allocType))
                          val puts = fields map { f =>
                            val v = GetField(f)(explosive)
                            if (env.enabled(GenerateWriteBarriers) && f.getType.isTraceableReference) {
                              assert(n.inlineContext.method.isManaged)
                              WriteBarrier.instance(r, depriveIfNeeded(v))
                            }
                            PutField(f)(r, v)
                          }

                          val finalFieldsRequirePublicationBarrier = !env.enabled(GenerateWriteBarriers) // see JET-12699
                          if ((fields exists (_.isFinal)) && finalFieldsRequirePublicationBarrier) {
                            // also reconstruct SS barrier from constructor
                            val memBarrier = MemBarrier(Set(STORE_STORE))()
                            (r, memBarrier)
                          } else {
                            (r, puts.lastOption getOrElse r)
                          }

                        case n @ NewArray(_, lengths) =>
                          val arrayType = allocType
                          val elemType = n.allocType.getArrayElemType
                          val r = NewArray(arrayType)(lengths*)
                          val IntegralConst(length) = lengths.head
                          assert(length >= 0)
                          val puts = (0L until length) map { i =>
                            val idx = IntegralConst(TypedArrayOperation.idxType(arrayType))(i)
                            val value = ArrayGet(arrayType)(explosive, idx)
                            if (env.enabled(GenerateWriteBarriers) && elemType.isTraceableReference) {
                              assert(n.inlineContext.method.isManaged)
                              WriteBarrier.instance(r, value)
                            }
                            ArrayPut(arrayType)(r, idx, value)
                          }
                          (r, puts.lastOption getOrElse r)

                        case b: BoxedValue =>
                          val r = BoxedValue(b.boxType)(b.inValue0)
                          // reconstruction itself initializes the value field
                          (r, r)

                        case _ => shouldNotReachHere(explosive)
                      }
                    }
                  }
                }

                eliminateCrossBlockMemoryEdges()

                if (Explosion.log.isEnabled) {
                  for (case Edge(from, to) <- reconEdges) {
                    Explosion.log(s"  reconstruction between ${from.name} and ${to.name}", to)
                  }
                }

                val recons = reconEdges map inSeparateBlock map reconstructAfter

                splitCriticalEdges()

                replaceAllValueUsesByVar(explosive) match {
                  case Some(initialAssign) =>
                    for ((recon, lastReconNode) <- recons) {
                      assert(explosive dominates lastReconNode)
                      insertCodeAfter(lastReconNode, useDefaultHandler = true) {
                        withPos(explosive) {
                          AssignVar(initialAssign.variable)(recon)
                        }
                      }
                    }
                  case None => shouldNotReachHere("Empty uses case should be filtered earlier")
                }
              }
            }
          case _ =>
          }
          changed
        }
      }

      if (changed) {
        completeSSA()
        eliminateUnreachableCode()
        eliminateDeadCode()
        val oldDefExploded = explodeObjects(toExplode.iterator)
        if (!oldDefExploded) {
          val msgWithVar = "explosion failed to cleanup after pre-explosion"
          dbgPrinter.debugNodes(msgWithVar, info = { n => if (toExplode contains n) "[TO EXPLODE]" else null })
          dbgPrinter.debugGraphs(msgWithVar)
          explodeObjects(toExplode.iterator, collectFailStats = true)
          shouldNotReachHere(s"all toExplode nodes {${toExplode.mkString("; ")}} have had to explode at this point")
        }
        true
      } else {
        assert(toExplode.isEmpty)
        false
      }
    }
  }

  def inSeparateBlock(edge: Edge): Block = edge match {
    case e @ Edge(_, bb: BBlock) => BBlock.extractInputEdges(bb, Seq(e))
    case e @ Edge(_, xb: XBlock) => XBlock.extractInputEdges(xb, Seq(e))
    case Edge(a: UpperPoint, b: LowerPoint) =>
      Block.splitAfter(a, keepControlled = true)
      Block.splitBefore(b).block
    case _ => shouldNotReachHere(edge)
  }
}
