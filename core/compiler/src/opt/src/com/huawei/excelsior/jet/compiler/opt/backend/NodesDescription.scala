/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.backend

import com.huawei.excelsior.common.CodeHelpers.*
import com.huawei.excelsior.jet.assembler.Location.AnyReg
import com.huawei.excelsior.jet.compiler.Env.tailRegister
import com.huawei.excelsior.jet.compiler.abi.ABI.{AltLocation, TailSlot}
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.*
import com.huawei.excelsior.jet.compiler.opt.ir.Universe
import com.huawei.excelsior.jet.compiler.util.Maps

import scala.annotation.nowarn
import scala.collection.mutable

/**
 * Machine dependent nodes description: preferred registers for arguments
 * and results, bound results, commutative operations, e.t.c
 *
 * @author conwor
 */
@nowarn("msg=match may not be exhaustive")
trait NodesDescription { self: Universe with BackEnd =>

  /**
    * First return result of this method is cached with key node.proto, meaning that we can't create constructions like that:
    * {{{
    * case n: Node if isFoo(n) => fooRes
    * case n: Node if isBar(n) => barRes
    * }}}
    *
    * if isFoo/isBar test information not related to node.proto, instead do it like this:
    * {{{
    * case n: Node => fooBarRes
    *
    * val fooBarRes: Edge => ResourceSet = { e =>
    *   if (isFoo(e.target)) return fooRes(e)
    *   else if (isBar(e.target)) return barRes(e)
    *   shouldNotReachHere(s"$e")
    * }
    * }}}
    *
    * because if there is information about being from `foo` or `bar` isn't encoded in node.proto, we possibly can reuse
    * result of isBar case e.t.c.
    */
  protected def nodeClassFormImpl(node: Node): NodeForm = node match {
    case _: Call              => callForm
    case _: Constraints       => constraintsForm
    case _: BulldozerHint     => bulldozerHintForm
    case _: Copy              => new CustomForm(Seq(copyArgs))
    case _: LoadTailParam     => new CustomForm(Seq(allParamIRegsSet, loadTailParam))
    case _: Return            => new CustomForm(Seq(returnArg _))
    case _: EndLocalUnmovable => new CustomForm(Seq(universalSet))

    case _ => simpleForm
  }

  private val formsByNodeCache = Maps[Node].newMMap[NodeForm]
  private val formsByNodeProtoCache = new mutable.LinkedHashMap[Prototype[_], NodeForm]

  /** Returns backend form for given `node`. */
  def nodeForm(node: Node): NodeForm = formsByNodeCache.getOrElseUpdate(node, {
    formsByNodeProtoCache.getOrElseUpdate(node.proto, nodeClassFormImpl(node))
  })

  protected def temporalSlotsCount(node: Node): Int = 0

  /** @return set of resources, allowed for source of given edge `e`, based on edge target node form. */
  def allowedLocations(e: Edge): ResourceSet = nodeForm(e.target).argumentResources(e)


  //////////////////////////////////////////////////////////////////////////////////////

  protected val resRegs: Node => ResourceSet = { n =>
    if (n.isFP) allFRegsSet else allIRegsSet
  }

  protected val argRegs: Edge => ResourceSet = { e =>
    if (e.source.isFP) allFRegsSet else allParamIRegsSet
  }

  private val loadTailParam: Edge => ResourceSet = {
    case Edge(p /*: Param | Proxy */, _) => setOf(p.resource ensuring (_.isInstanceOf[TailSlot]))
  }

  val shouldNotCall: Node => ResourceSet = { _ => shouldNotCallThis() }

  private val copyArgs: Edge => ResourceSet = { case e @ Edge(_, copy: Copy) =>
    if (copy.isStore) {
      argRegs(e)
    } else {
      universalSet
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////

  private def nodeToSet(x: Any): Node => ResourceSet = x match {
    case set: ResourceSet => { (_: Node) => set }
    case _ => x.asInstanceOf[Node => ResourceSet]
  }

  private def edgeToSet(x: Any): Edge => ResourceSet = x match {
    case set: ResourceSet => { (_: Edge) => set }
    case _ => x.asInstanceOf[Edge => ResourceSet]
  }

  /** NodeForm is a description of the operations type `T` properties, important for this operations code generation.
    * It includes allowed resources for operation result & sequence of allowed resources for arguments.
    *
    * NodeForms generated for all types of nodes that may occur during code generation.
    */
  abstract class NodeForm {
    protected def argumentRegisters(e: Edge): ResourceSet

    final def argumentResources(e: Edge): ResourceSet = {
      if (e.source.isInstanceOf[ExecEnv]) {
        eeIRegSet.ensuring(_ != emptySet)
      } else if (shouldBeUsedAsImmediate(e)) {
        assert(e.source.isInstanceOf[Constant])
        immSet
      } else {
        if (mayBeUsedAsImmediate(e)) {
          argumentRegisters(e) | immSet
        } else {
          argumentRegisters(e)
        }
      }
    }

    final def argumentCandidates(e: Edge)(state: Value => MutableResourceSet): ResourceSet =
      argumentResources(e) & state(valueOf(e.source))
  }

  class SimpleForm extends NodeForm {
    def argumentRegisters(e: Edge): ResourceSet = argRegs(e)
  }

  class CustomForm(arguments: Seq[Any]) extends NodeForm {
    private val argumentSets: Seq[Edge => ResourceSet] = arguments map { argument => edgeToSet(argument) }
    protected def argumentRegisters(e: Edge): ResourceSet = argumentSets(indexInValueArgs(e))(e)
  }

  protected lazy val simpleForm = new SimpleForm


  /////////////////////////////////////////////////////////////////////////////
  // Call & Return

  private def returnArg(e: Edge): ResourceSet =
    if (frame.abi.returnType.isZST) emptySet else setOf(frame.abi.resultLocation)

  protected def indirectCallTargetSet(call: Call): ResourceSet

  protected def callParamSet(call: Call, paramIdx: Int, e: Edge): ResourceSet = {
    // Most common implementation
    call.abi.paramLocations(paramIdx) match {
      case r: AnyReg => setOf(r)
      case _: TailSlot => e.source.asInstanceOf[CallArgStore].allowedResults
      case altLoc: AltLocation => setOf(altLoc)
    }
  }

  private lazy val callForm: NodeForm = new NodeForm {
    def argumentRegisters(e: Edge): ResourceSet = (e.targetArgIndex, e.target) match {
      case (0 | 1, _) => invalidSet
      case (2, DAICall(_) | AnyInvoke()) => invalidSet
      case (2, DirectCall(_)) => immSet
      case (2, call: Call) =>
        val baseSet = indirectCallTargetSet(call)
        if (call.abi.hasRealTail) {
          (baseSet - tailRegister) ensuring (_.nonEmpty)
        } else {
          baseSet
        }
      case (_, call: Call) =>
        if (e.source.isInstanceOf[Void] || e.source.isInstanceOf[MutFunc.Host]) {
          immSet
        } else {
          callParamSet(call, call.invokeArgIdx(e), e)
        }
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Constraints

  private lazy val constraintsForm: NodeForm = new NodeForm {
    def argumentRegisters(e: Edge): ResourceSet = Constraints.shouldBeLiveOn(e) match {
      case InvalidResource => universalSet
      case x => setOf(x)
    }
  }


  /////////////////////////////////////////////////////////////////////////////
  // Bulldozer hint form

  private lazy val bulldozerHintForm: NodeForm = new NodeForm {
    override protected def argumentRegisters(e: Edge) = e.target match {
      case hint: BulldozerHint if hint.load => argRegs(e)
      case _ => universalSet
    }
  }
}
