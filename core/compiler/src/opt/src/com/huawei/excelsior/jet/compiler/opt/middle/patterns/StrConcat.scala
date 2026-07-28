/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.patterns

import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind
import com.huawei.excelsior.jet.compiler.{PreparationRequired, Stats, StatsKind}
import com.huawei.excelsior.jet.compiler.symlevel.MethodReferenceAccessKind.*
import com.huawei.excelsior.jet.compiler.opt.ir.{Tag, Universe}
import com.huawei.excelsior.jet.compiler.symlevel.{Method, MethodSignature, SignatureType, TypeKind, Type as SymType}

import scala.PartialFunction.condOpt
import scala.annotation.tailrec

/**
 * Str concat optimizer
 *
 * @author ikireev
 * @author paul
 */
trait StrConcat { self: Universe =>

  private def isJavaSBClass(name: String) = name == "java/lang/StringBuilder" || name == "java/lang/StringBuffer"
  private def isAJSBClass(name: String) = name == "com/huawei/excelsior/aj/lang/AJStringBuilder"
  private def isSBClass(name: String) = isJavaSBClass(name) || isAJSBClass(name)
  private def isSBMethod(m: Method, name: String) = isSBClass(m.getDeclaringClass.getName) && m.getName == name
  private def isSBToStringMethod(m: Method) = isSBMethod(m, "toString") || isSBMethod(m, "toAJString")
  private def isStringMethod(m: Method, name: String) = m.getDeclaringClass.getName == "java/lang/String" && m.getName == name

  private def tryOptimizeOneStrConcat(root: Call): Boolean = {

    def sbAppendArgIndex = 1
    /** Argument node & sym type of StringBuilder.append method */
    def sbAppendArg(append: Call) = append.invokeArgs(sbAppendArgIndex)
    def sbAppendArgType(append: Call) = append.methodType.parameterType(sbAppendArgIndex).symType

    val isAJ = isAJSBClass(root.targetRef.method.getDeclaringClass.getName)
    val stringType = if (isAJ) typeProvider.getAJStringType else typeProvider.getStringType

    def hasSideEffects(append: Call) = {
      val argt = sbAppendArgType(append)
      import TypeKind._
      argt.getKind match {
        case CLASS => argt != stringType
        case INTERFACE => true
        case _ =>
          assert(argt.isPrimitive)
          false
      }
    }

    def isSBAppendMethod(m: Method) = isSBMethod(m, "append") && !m.getParamType(1).symType.isJavaArray

    def valueOfSig = MethodSignature(SignatureType.javaLangObject)(SignatureType.javaLangString)
    lazy val stringValueOfObject = typeProvider.getStringType.getMethodRefToLocal(xstr("valueOf"), valueOfSig, STATIC)

    /** Nullcheck(recv) just before non-static Invoke(recv) */
    object ReceiverCheck {
      def unapply(nc: NullCheck): Option[Call] = condOpt(nc.outCtrl) {
        case call: Call if call.targetRef.hasReceiverParameter && call.receiver == nc.obj => call
      }
    }

    def checkSBAppendUses(obj: Node, call: Call) = obj.valueUses forall {
      case `call` | ReceiverCheck(`call`) => true
      case u => u == call.target
    }

    def checkSBNewUses(obj: Node, init: Call, call: Call) = obj.valueUses forall {
      case `init` | ReceiverCheck(`init`) | `call` => true
      case u => u == init.target || u == call.target
    }

    object SBNew {
      def unapply(obj: New): Boolean = isSBClass(obj.allocType.symType.getName)
    }

    object SBInit {
      def unapply(call: Call): Option[New] = condOpt(call) {
        case CallMethod(m, SPECIAL, (obj @ SBNew()) +: _) if isSBMethod(m, "<init>") => obj
      }
    }

    object SBAppend {
      def unapply(call: Call): Option[Node] = condOpt(call) {
        case CallMethod(m, VIRTUAL, Seq(recv, _)) if isSBAppendMethod(m) => recv
      }
    }

    object SBToString {
      def unapply(call: Call): Option[Node] = condOpt(call) {
        case CallMethod(m, VIRTUAL, Seq(recv)) if (isSBToStringMethod(m)) => recv
      }
    }

    object StringValueOf {
      def unapply(call: Call): Option[(Node, SymType)] = condOpt(call) {
        case CallMethod(m, STATIC, Seq(arg)) if isStringMethod(m, "valueOf") => (arg, m.getParamType(0).symType)
      }
    }

    object SBInitNoArg {
      def unapply(call: Call): Boolean = call.invokeArgs.size == 1
    }

    object SBInitWithArg {
      def unapply(call: Call): Option[BytecodeTypeKind] =
        Option.when(call.invokeArgs.size == 2)(call.methodType.parameterType(1).jbcKind)
    }

    case class SBInitArgInfo(arg: Node, argType: SymType)

    object SBInitString {
      def unapply(call: Call): Option[(SBInitArgInfo, Option[Call])] = call.invokeArgs match {
        case Seq(_, ctorArg) if call.methodType.parameterType(1).symType == stringType =>
          ctorArg match {
            case _: ConstString => Some((SBInitArgInfo(ctorArg, stringType), None))
            case valueOf @ StringValueOf(vofArg, argType) =>
              val hasOtherUses = valueOf.valueUses exists (_ != call)
              if (argType.isPrimitive && !hasOtherUses) {
                Some((SBInitArgInfo(vofArg, argType), Some(valueOf)))
              } else {
                Some((SBInitArgInfo(ctorArg, typeProvider.getStringType), None))
              }
            case _ => None
          }
        case _ => None
      }
    }

    /**
     *  SBChain is a code pattern of the shape `new StringBuilder(xs).append(E1). ... .append(En).toString()`
     * (for Java) or `new AJStringBuilder(xs).append(E1). ... .append(En).toAJString()` (for AJ),
     *  where `xs` may be empty arg sequence or single arg of the form:
     *   - String.valueOf(primitive type) (only for Java)
     *   - String.valueOf(non-primitive type) (only for Java)
     *   - ConstString
     *   - Value of type `int` (only for Java)
     *   - Value of type `AddrUInt` (only for AJ)
     *   In the first case `valueOf` call will be removed.
     *   In the last case `int`/`AddrUInt` value will be ignored.
     */
    case class SBChain(sbNew: New, sbInit: Call, appends: List[Call], sbToString: Call,
                       sbInitArg: Option[SBInitArgInfo], valueOfCall: Option[Call])

    /** Find a StringBuilder chain ending at `root` (i.e. chain which `.to[AJ]String` call node is `root`). */
    def findSBChain(root: Call): Option[SBChain] = {
      root match {
        case toString @ SBToString(recv: Call) if checkSBAppendUses(recv, toString) =>
          @tailrec def step(x: Call, appends: List[Call]): Option[SBChain] = x match {
            case x @ SBAppend(recv: Call) if checkSBAppendUses(recv, x) =>
              step(recv, x :: appends)
            case x @ SBAppend(recv @ SBNew()) =>
              val inits = recv.valueUses.collect { case x @ SBInit(`recv`) => x }.toList
              inits match {
                case List(init) if checkSBNewUses(recv, init, x) =>
                  assert(init dominates x)
                  init match {
                    case SBInitNoArg() | SBInitWithArg(BytecodeTypeKind.INT | BytecodeTypeKind.LONG) =>
                      Some(SBChain(recv, init, x :: appends, toString, None, None))
                    case SBInitString(argInfo, valueOfCall) =>
                      Some(SBChain(recv, init, x :: appends, toString, Some(argInfo), valueOfCall))
                    case _ => None
                  }
                case _ => None
              }
            case _ => None
          }
          step(recv, Nil)

        case _ => None
      }
    }

    /** Replace one StringBuilder chain with StrConcat node */
    def replaceSBChain(chain: SBChain): Unit = {
      val (args, argTypes) = {
        val argsWithTypes = chain.appends map { append =>
          var arg = sbAppendArg(append)
          var argt = sbAppendArgType(append)
          if (hasSideEffects(append)) {
            // Preserve `SB.append(Object|CharSequence)` side effects by calling `String.valueOf(Object)` explicitly.
            assert(!isAJ)
            arg = insertCodeBefore(append) {
              ensurePrepared(PreparationRequired.forInvoke(stringValueOfObject))
              val x = depriveMethodArg(append.methodType, sbAppendArgIndex, arg)
              Invoke(stringValueOfObject)(x)
            }
            argt = typeProvider.getStringType
          }
          (arg, argt)
        }

        (chain.sbInitArg match {
          case Some(SBInitArgInfo(a, t)) => (a, t) +: argsWithTypes
          case None => argsWithTypes
        }).unzip
      }

      val result = chain.sbToString
      decommit(result.target)
      val concat = replaceByCode(result) { StrConcat(argTypes, isAJ)(args: _*) }
      stats.count(StatsKind.StringOpt, s"strconcat${if(isAJ) " (AJ)" else ""} found: ${concat.formatString}", concat)

      val nullchecks = collect[NullCheck]((chain.sbNew :: chain.appends) flatMap (_.valueUses)).toList
      val sbcalls = chain.sbInit :: chain.appends
      val sbops = chain.sbNew :: sbcalls
      nullchecks foreach strikeOut
      sbcalls.reverse foreach { c => decommit(c.target) }
      sbops.reverse foreach strikeOut
      chain.valueOfCall foreach strikeOut
    }

    var changed = false
    for (chain <- findSBChain(root)) {
      replaceSBChain(chain)
      changed = true
    }
    changed
  }

  /** Find all complete StringBuilder chains and replace each one with StrConcat node */
  def optimizeStrConcat(): Boolean = {
    var changed = false
    for (x <- all[Call] if x.targetRef.hasMethod && isSBToStringMethod(x.targetRef.method)) { // fast path check is outlined
      changed |= tryOptimizeOneStrConcat(x)
    }
    changed
  }
}
