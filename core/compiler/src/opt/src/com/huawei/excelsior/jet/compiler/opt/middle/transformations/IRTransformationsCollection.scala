/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.transformations

import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.symlevel.Field
import com.huawei.excelsior.jet.util.ScalaCollections

import scala.PartialFunction.{cond, condOpt}
import scala.annotation.tailrec

/**
 * Collection of IR transformations.
 * @author conwor
 * @author paul
 */

trait IRTransformationsCollection extends IRTransformationsFramework { self: Universe =>

  /**
   * This transformation removes block, if it has not control flow nodes
   * and has single successor.
   *
   * Attention: this transformation can produce critical edges.
   */
  abstract class EmptyBlocksEliminationCore extends IRTransformation {

    protected def allowOnlySinglePredecessor: Boolean

    private object GoodEmptyBlock {
      def unapply(b: BBlock): Option[(BBlock, Goto)] = {
        // skip entry block
        if (b == entryBlock) return None

        // skip blocks with non-goto exit
        val goto = b.blockEnd match {
          case x: Goto => x
          case _ => return None
        }

        // skip blocks with many inputs if requested
        if (allowOnlySinglePredecessor && b.predBlocks.size != 1) return None

        // skip totally unreachable blocks
        if (b.predBlocks.isEmpty) return None

        // skip looped blocks and blocks with XBlock predecessor
        // (the last one is a partial workaround for JET-9706)
        if (b.predBlocks exists { pb => pb == b || pb.isInstanceOf[XBlock] }) return None

        // skip blocks with non-trivial control or controlled nodes
        if (b.uses exists { u => u != goto && !u.isInstanceOf[Phi] }) return None

        // All phies should have no uses except uses in target.phies in proper places.
        val targetEdge = goto.targetEdge
        val phiesAreUseless = b.phies forall (_.outEdges forall { e => e.target match {
          case `goto` => true
          case targetPhi: Phi if targetPhi.controlInput(e) == targetEdge => true
          case _ => false
        }})
        if (!phiesAreUseless) return None

        Some(b, goto)
      }
    }

    register {
      case GoodEmptyBlock(b, goto) =>
        val target = goto.target
        val targetEdge = goto.targetEdge

        if (b.phies.isEmpty) {
          // fast-path
          val inputs = b.inputs
          if (inputs.size == 1) {
            // even faster path
            targetEdge.source = inputs.head

          } else {
            Block.addEdgesWithTemplate(inputs, targetEdge)
            Block.removeEdge(targetEdge)
          }

        } else {
          // these phies should be inlined, i.e. replaced by their arguments
          val ourPhies = b.phies.toSet

          for (inEdge <- b.inEdges) {
            target.addInEdge(inEdge.source, { phi =>
              phi.phiArg(targetEdge) match {
                case ourPhi: Phi if ourPhies contains ourPhi =>
                  ourPhi.phiArg(inEdge)
                case arg =>
                  arg
              }
            })
          }
          Block.removeEdge(targetEdge)

          ourPhies foreach decommit
        }

        decommit(b)
        decommit(goto)
        true
    }
  }

  case object EmptyBlocksElimination extends EmptyBlocksEliminationCore {
    override def allowOnlySinglePredecessor = true
  }

  // Elimination of all empty blocks is a good transformation in general sense.
  // However right now it has at least following problems:
  // * it may increase code size because of duplication of transfers for phi functions,
  //   this may be fixed by introducing opposite transformation - extraction of block with same phi arguments
  // * counted loops recognizer cannot process loop header with many forward inputs,
  //   this may be fixed by preprocessing loop headers and extraction of block with invariant phi arguments
  // TODO: fix these and others problems and enable this variant of elimination by default
  case object EmptyBlocksEliminationWithManyPredecessors extends EmptyBlocksEliminationCore {
    override def allowOnlySinglePredecessor = false
  }


  /**
   * This transformation removes control flow edge, if it's start has only one successor
   * and it's end has only one predecessor.
   * If there were control flow nodes in one of this blocks, they are connected.
   */
  case object BlocksConnectionTransformation extends IRTransformation {
    private def hasNoThrowingOperations(b: BBlock): Boolean = b.xpoints.isEmpty

    register {
      case goto @ Goto(from: BBlock, to: BBlock) if (to.predBlocks.size == 1) && (from != to) =>
        condOpt((from.singleXHandlerOrNull, to.singleXHandlerOrNull)) {
          // We can connect block with xHandlers if:
          // it's safe to merge blocks with equal handlers;
          // it's safe to extend try-block up & down.
          case (xb1, xb2) if xb1 == xb2 => xb1
          case (xb, null) if hasNoThrowingOperations(to) => xb
          case (null, xb) if hasNoThrowingOperations(from) => xb
        } match {
          case Some(xHandler) =>
            to.refreshBlockRef()

            to.replaceUses { e => e.sourceLabel match {
              case Tag.CONTROL => goto.inCtrl
              case Tag.MEMORY  => goto.inMemory
            }}
            decommit(goto)
            from.blockEnd = to.blockEnd
            decommit(to)
            true

          case None => false
        }
    }
  }

  /** This transformation removes uses of default exception handler which just rethrows exception. */
  case object DefaultHandlersElimination extends IRTransformation {
    register {
      case xb: XBlock if xb.reachable && xb.inputs.nonEmpty && catchedObjectIsRethrowedInSuccessor(xb) =>
        makeUnreachable(xb.inEdges)
        // the rest of the job will be done by UCE
        true
    }

    private def catchedObjectIsRethrowedInSuccessor(catchBlock: XBlock): Boolean = {
      cond(catchBlock.blockEnd) {
        case goto @ Goto(_, throwBlock @ EmptyThrowBlock(throwed)) =>
          val catched = catchBlock.catchNode
          cond(throwed) {
            case `catched` =>
              true

            case phi @ Phi(`throwBlock`, _*) =>
              // check whether throwed object passed on edge from xblock is equal to catched one
              phi.phiArg(goto.targetEdge) == catched
          }
      }
    }

    private object EmptyThrowBlock {
      /** Returns throwed node. */
      def unapply(b: BBlock): Option[Node] = {
        b.blockEnd match {
          case halt: Halt =>
            halt.inCtrl match {
              case throwNode @ Throw(thrCtrl, _, throwed) if !throwNode.hasXHandler =>
                thrCtrl match {
                  // NullCheck may be unremovable if throwed node is a phi
                  case `b` | NullCheck(`b`, _, `throwed`) => Some(throwed)
                  case _ => None
                }
              case _ => None
            }
          case _ => None
        }
      }
    }
  }

  /**
   * This transformation removes multi-edge. If some block has all outgoing edges to the same block,
   * and this block has no phi-functions, then all this edges are transformed to single goto.
   */
  case object MultiEdgeElimination extends IRTransformation { //TODO: does it needed there? DCE should do all this stuff
    register { case from: Block if !isUnreachableBar(from) && (from.succBlocks.size > 1) =>
      cond(ScalaCollections.uniqueValue(from.succBlocks)) {
        case Some(to) if to.phies.isEmpty =>
          val branch = from.blockEnd
          val remainingEdges = to.inputs filter (_.block != from)
          val goto = Goto(branch.inCtrl, branch.inMemory) //TODO: branch.replaceByGoto()
          to.replaceArgsBySeq(Seq(goto) ++ remainingEdges)
          decommit(branch)
          true
      }
    }
  }

  /**
    * This transformation replaces phi-function to CondVal operation, if:
    *   1) phi-function has integral constants 0 and 1 as arguments,
    *   2) phi-function is in block, that has single predecessor (by two edges).
    *
    * Note: javac often produces such pattern (e.g. storing boolean to local, passing boolean to function etc.)
    */
  case object PhiToCondValReplacing extends IRTransformation {
    register {
      case phi @ Phi(block: BBlock, IConst(x), IConst(y)) if (x == 1 && y == 0) || (x == 0 && y == 1) =>
        cond(ScalaCollections.uniqueValue(block.predBlocks).map(_.blockEnd)) {
          case Some(branch @ If(condition)) =>
            val invertedCondition = ((x == 1) != (block.inputs.head == branch.trueExit))
            phi replaceBy CondVal(if (invertedCondition) Not(condition) else condition)
            true
        }
    }
    override def toString = "replacing simple phies to CondVal"
  }

  /** Eliminates first comparison in the following pattern
    * `b == o || (o != null && o instanceof Box && o.value == i.value)` if b is a Box.valueOf boxing operation on some integral type.
    * Such pattern appears as an optimized version of `b == o || (o != null && b.equals(o))`
    * which in turn is a common way to compare boxed values (e.g. HashMap.getNode/ConcurrentHashMap.get).
    * In some cases (HashMap) `b != null` may appear instead of `o != null`, this way it would inevitably be eliminated by type analysis.
    *
    * First comparison (`b == o`) elimination does not violate the correctness of the program.
    * However, it prevents `b` from proper explosion in certain cases.
    * Possible performance degradations caused by this comparison elimination are considered insignificant
    * in presence of possible gains from boxing operation explosion.
    */
  case object BoxingEqualitySimplification extends IRTransformation {
    private def hasNoSideEffects(b: Block): Boolean = b.spine forall SpinalNode.sideEffectFree
    // Prevent empty and nearly-empty blocks from ruining our efforts
    private object SkippingEmpty {
      @tailrec
      private def skipEmpty(b: Block): Block = b.blockEnd match {
        case g: Goto if hasNoSideEffects(b) => skipEmpty(g.target)
        case _ => b
      }
      def unapply(b: Block) = Some(skipEmpty(b))
    }

    private def matchCmpNull(o: Node, b: BoxedValue, eqBlock: Block, toFirstCheck: If.Exit): Boolean = {
      object GetValueField {
        def unapply(gf: GetField) = condOpt(gf) {
          case GetField(field, _, _, o) if b.boxType.value == field => o
        }
      }
      def eliminateOuterIf() = {
        replaceByGoto(toFirstCheck)
        true
      }
      def matchInstOf(instOfBlock: Block, notEqBlock: Option[Block]): Boolean = {
        hasNoSideEffects(instOfBlock) && cond(instOfBlock.blockEnd) {
          case IfInstanceOf(tpe, `o`, BlockExit(_, valCmpBlock), BlockExit(_, SkippingEmpty(neqB2))) =>
            val sameNeqBlock = notEqBlock forall (_ == neqB2)
            if (tpe == b.boxType.symType && sameNeqBlock && hasNoSideEffects(valCmpBlock)) {
              IfEq.Commutative.cond(valCmpBlock.blockEnd) {
                case (b.PrimitiveValue(), GetValueField(`o`),
                      BlockExit(_, SkippingEmpty(`eqBlock`)), BlockExit(_, SkippingEmpty(`neqB2`))) => eliminateOuterIf()
              }
            } else {
              false
            }
        }
      }

      val ncB = toFirstCheck.target
      b.primitiveType.isIntegral && hasNoSideEffects(ncB) && cond(ncB.blockEnd) {
        case IfNull(`o`, BlockExit(_, SkippingEmpty(nullBlock)), notNullExit) => matchInstOf(notNullExit.target, Some(nullBlock))
        case _ => matchInstOf(toFirstCheck.target, None)
      }
    }

    register { n =>
      IfEq.Commutative.cond(n) {
        case (o, b: BoxedValue, BlockExit(_, SkippingEmpty(eqBlock)), toFirstCheck) => matchCmpNull(o, b, eqBlock, toFirstCheck)
      }
    }

    override def toString = "simplifying boxing equality"
  }

  /** Swaps the order of CheckCast immediately followed by NullCheck,
    * to allow NullCheck to be generated as implicit check after CheckCast is lowered.
    */
  case object CheckCastNullCheckSwapping extends IRTransformation {
    register {
      case nc @ NullCheck(cc @ CheckCast(_, ccObj), _, ncObj) if ccObj == ncObj && !nc.trusted && !cc.trusted &&
        nc.block.reachable && noXPhiArgsBelowCC(nc, cc) =>
        assert(nc.inMemory == cc.inMemory)

        insertCode(xContext = nc, posContext = nc, ctrlBefore = cc.inCtrl, useDefaultHandler = false) { NullCheck(ncObj) }
        strikeOut(nc)
        true
    }

    def noXPhiArgsBelowCC(nc: NullCheck, cc: CheckCast) = nc.xHandlerOption forall { xb =>
      withIncrementalGCM {
        val e = nc.xpoint.xEdge
        xb.phies forall (phi => upperPoint(phi.phiArg(e)) strictDominates cc)
      }
    }

    override def toString = "swapping CheckCast -> NullCheck pattern"
  }

}
