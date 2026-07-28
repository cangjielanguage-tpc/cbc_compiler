/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.inline

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.compiler.StatsKind.Devirt
import com.huawei.excelsior.jet.compiler.opt.CompilerException
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.{CheckLevels, Universe}
import com.huawei.excelsior.jet.compiler.opt.middle.*
import com.huawei.excelsior.jet.compiler.options.BoolOption.*
import com.huawei.excelsior.jet.compiler.options.NumOption.Worker
import com.huawei.excelsior.jet.compiler.{Stage, symlevel}
import com.huawei.excelsior.jet.util.Worklist

import scala.PartialFunction.cond
import scala.annotation.nowarn
import scala.collection.mutable

/**
 * Inliner can inline method invocations, that means, that it replaces invoke
 * instruction with method content. It can work on different method content
 * providers: bytecode or serialized IR.
 *
 * Inline process is separated into two stages. First stage is processing inlined method content
 * and appending it into IR with its specialization. For example, we could
 * replace some instruction "a+b" from inlined method content if such instruction already exists
 * in our IR. In this process we could specialize inlined method content with initial part of IR.
 *
 * Second stage is specializing all IR using inlined instructions. For example, we
 * replace result of inlined invoke with result of inlined method return instruction, which may
 * cause a series of substitutions and specializations.
 *
 * The decision to inline can be made after first stage, because at this moment we can
 * still make a cheap roll back. Indeed, we still do not replace any usages in our initial IR,
 * so roll back of inline is only removing of inlined instructions from IR. After second stage
 * roll back of inline is more expensive, so we do not support it yet.
 *
 * @author cypok
 * @author conwor
 */
trait InlineOptimization extends Devirtualization with InlineEngine { self: Universe with ContextTypesRecalculation =>

  def inlineAll(): Unit = {
    if (env.enabled(NeverInline)) return

    currentPhase match {
      case CompilerPhase.PreInline  if !env.enabled(PreInline)  => return
      case CompilerPhase.PostInline if !env.enabled(PostInline) => return
      case _ =>
    }

    val inliner = new Inliner()
    if (isO1Compiled) {
      inliner.fastInlineOnlyAJ()
    } else {
      inliner.inlineAll()
    }
  }

  private class Inliner {

    @nowarn("msg=match may not be exhaustive")
    private val preinline = currentPhase match {
      case CompilerPhase.PreInline => true
      case CompilerPhase.PostInline => false
    }

    private def inlineModeStr = if (preinline) "preinline" else "postinline"

    def fastInlineOnlyAJ(): Unit = stage(Stage.InlineAll) {
      all[Call] foreach {
        case call @ DirectCall(method) if method.isInlineAllAndRemove =>
          val success = doInline(new CallSite(method, direct = true, call), allowFromBytecode = true)
          assert(success, s"$call must be inlined")
          inlineDebugLog(s"all graph after fast inline ${method.getFullName}")
        case _ =>
      }
    }

    def inlineAll(): Unit = stage(Stage.InlineAll) {
      inlineLogSession(inlineModeStr) { TauTest.log.inSession("inline", codeUnit) {
        requireNoGlobalCodeMotion()

        dbgPrinter.debugNodes("All graph before inline")

        val tailRecCallSites = mutable.Buffer.empty[CallSite]

        val invocations = Worklist.from(all[Call])
        val newInvocations = Worklist.empty[Call]

        def addInvoke(n: Node): Unit = n match {
          case invoke: Call => newInvocations += invoke
          case _ =>
        }

        while (invocations.nonEmpty) {
          recalculateContextTypes()
          dbgPrinter.debugNodes(s"All graph after context types recalculation during inline")
          checkIRConsistency(CheckLevels.Optional)

          // Workaround for JET-14525.
          // TODO: introduce bridge methods for such narrowing return type overriding,
          //       which will contain necessary Eop transformation.
          def convertDevirtInvoke(tpe: Type, newArg: Node): Node = {

            def enrich(t: symlevel.Type, obj: Node) = {
              if (t.isDeferred) {
                // Do not enrich deferred types.
                obj
              } else {
                assert(t.isInterface)
                Enrich(t)(obj, WeakCast(t)(obj, WeakCast.NoCheck()))
              }
            }

            if (!tpe.isValueType) return newArg

            import EnrichmentDecision.*
            newArg match {
              case invoke: Call => (isRich(tpe), producesRich(invoke).toOption) match {
                case (No, None) =>
                  // Both plain, so nothing to be done.
                  invoke

                case (Yes(expected), Some(actual)) if expected == actual =>
                  // Same enrichment, so nothing to be done.
                  invoke

                case (No, Some(actual)) =>
                  // Interface cannot override class.
                  shouldNotReachHere(s"non-rich call devirtualized into rich $actual (devirtualized: $invoke)")

                case (Yes(expected), None) =>
                  // Interface overridden by class.
                  enrich(expected, invoke)

                case (Yes(expected), Some(actual)) =>
                  // Interface overridden by another interface.
                  enrich(expected, Deprive(actual)(invoke))

                case _ =>
                  // Argument enrichment is unknown, so nothing to be done.
                  invoke
              }
              case _ => newArg
            }
          }

          for {
            invoke <- invocations.drain if invoke.isCommitted && invoke.targetRef.hasMethod && invoke.block.reachable
            invokeAfterDevirt <- Node.withImplicitArgConversion(convertDevirtInvoke)(devirtualizeWithLog(invoke))
          } {
            tryInline(invokeAfterDevirt, tailRecCallSites, addInvoke)
          }
          newInvocations swap invocations
        }

        if (tailRecCallSites.nonEmpty) {
          transformTailRec(tailRecCallSites)
          inlineLog("tailrec", s"$codeUnit (${tailRecCallSites.size} call site(s))")
        }

        dbgPrinter.debugNodes(s"All graph after $inlineModeStr")
        checkIRConsistency(CheckLevels.Desirable)
      }}
    }

    private def devirtualizeWithLog(invoke: Call): Seq[Call] = {
      import Devirtualization.*

      val targetName = invoke.targetRef.method.getFullName
      val invokePos = invoke.pos
      // save these values because invoke node might be replaced by exception throw in devirtualize()

      def log(statsMsg: String, logTitle: String, logDetails: String): Unit = {
        stats.count(Devirt, statsMsg, invokePos)
        inlineLog(logTitle, logDetails)
        inlineDebugLog(s"Inline - $logTitle $targetName")
      }

      devirtualize(invoke, allowGuarded = !preinline) match {
        case Changed(devirtInvokes, msg) =>
          devirtInvokes match {
            case Seq(devirtInvoke) =>
              val devirtTargetName = devirtInvoke.targetRef.method.getFullName
              log(msg, "devirt", s"$targetName ${if (targetName != devirtTargetName) s"-> $devirtTargetName " else ""}($msg)")
            case _ =>
              log(msg, "multidevirt",
                s"$targetName -> <${devirtInvokes.map(_.targetRef.method.getFullName).mkString(", ")}> ($msg)")
          }
          devirtInvokes

        case ChangedLight(lightInvoke, msg) =>
          val lightTargetName = lightInvoke.targetRef.method.getFullName
          log(msg, "deinterf", s"$targetName ${if (targetName != lightTargetName) s"-> $lightTargetName " else ""}($msg)")
          Seq(lightInvoke)

        case Unchanged =>
          Seq(invoke)

        case Cold =>
          log("cold", "frozen", targetName)
          Seq(invoke)

        case Unreachable =>
          log("unreachable", "delete", s"$targetName (unreachable call)")
          Nil

        case Erroneous(msg) =>
          log("exceptional", "thrown", s"$targetName ($msg)")
          Nil
      }
    }

    private def tryInline(invoke: Call, tailRecCallSites: mutable.Buffer[CallSite], commitCallback: Node => Unit): Unit = {
      val cs = new JavaCallSite(invoke)
      cs.shouldInline(preinline) match {
        case Yes(reason) =>
          if (cs.isTailRec) {
            tailRecCallSites += cs

          } else {
            onCommit.withCallback(commitCallback) {
              if (doInline(cs, allowFromBytecode = preinline)) {
                inlineLog("inline", s"${cs.target.getFullName} ($reason)")
                if (currentPhase != CompilerPhase.PreInline && env.valueOf(Worker) == 0) {
                  // Do not mutate inline plan in workers to stabilize compilation result!
                  profile.markInlined(cs.node.inlineContext, cs.target, invoke.bytecodePos.get)
                }

              } else {
                handleNoInline(cs, "failed during inline", env.enabled(CollectFailStats))
              }
            }
          }

        case No(reason) => handleNoInline(cs, reason, env.enabled(CollectFailStats))
        case DoNotKnow => handleNoInline(cs, "not enough motivation", report = false)
      }
    }

    private def handleNoInline(cs: CallSite, reason: String, report: Boolean): Unit = {
      if (cs.target.isInlineAllAndRemove) {
        throw new CompilerException(
          s"cannot inline call of AJ @Inline(forced = ${cs.target.isAJInlineForced}) method ${cs.target.getFullName} at ${cs.node.pos} (because: $reason)")
      } else if (!preinline) {
        if (cs.target.isJCAInline) {
          inlineLog("cannot inline", s"${cs.target.getFullName} (JCA ALWAYS_INLINE) (because: $reason)")
        } else if (!env.enabled(InlineOnlyForced) && cs.target.isAJInline) {
          inlineLog("cannot inline", s"@Inline(forced = false) ${cs.target.getFullName} (because: $reason)")
        } else if (report) {
          inlineLog("cannot inline", s"${cs.target.getFullName} (because: $reason)")
        }
      }
    }

  }
}
