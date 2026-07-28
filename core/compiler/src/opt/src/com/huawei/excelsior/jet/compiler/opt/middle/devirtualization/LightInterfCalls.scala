/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.middle.devirtualization

import com.huawei.excelsior.common.Arch
import com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere
import com.huawei.excelsior.jet.compiler.opt.CompilerPhases.CompilerPhase
import com.huawei.excelsior.jet.compiler.opt.ir.{Nodes, Universe}
import com.huawei.excelsior.jet.compiler.options.BoolOption.NoTauTests
import com.huawei.excelsior.jet.compiler.symlevel.SignatureType
import com.huawei.excelsior.jet.compiler.types.Guards.OpenConeGuard
import com.huawei.excelsior.jet.compiler.types.ReferenceTypes.ClassType
import com.huawei.excelsior.jet.compiler.types.References.*
import com.huawei.excelsior.jet.compiler.{Env, Environment, RTConst, symlevel}

import scala.PartialFunction.condOpt

trait LightInterfCalls extends CallTargetInfos with Nodes { self: Universe =>

  object LightInterfCalls {
    sealed abstract class Result
    case object Unknown extends Result
    case class Inferred(klass: symlevel.ClassType) extends Result
    case class Guarded(guard: OpenConeGuard, rcvType: Cone) extends Result
  }

  import LightInterfCalls._

  def lightInterfCast(obj: Node, itype: symlevel.ClassType, point: ControlNode = null): Option[Node] = {
    assert(itype.isInterface)

    if (currentPhase < CompilerPhase.Deserialization) return None
    if (Env.targetArch == Arch.CBC) return None

    if (currentScope.inDeserialization) return None // ugly patch for null in node args; TODO: rewrite deserialization
    if (obj.tpe == UnreachableValueType) return None // Do not optimize such cases.

    assert(obj.tpe.isTraceableRefType)

    // Note that we do not optimize TypeEmpty and TypeNull, because it is essentially unreachable code.
    // See also JET-14348
    val t = if (point != null) nodeTypeAt(obj, point) else nodeType(obj)
    condOpt(t) {
      case UpperBounded(objType: ClassType, _) if objType implements SignatureType.fromSymType(itype) =>
        lightInterfCast(objType.symType, itype)
    }
  }

  def lightInterfCast(rcvType: symlevel.ClassType, itype: symlevel.ClassType): Node = {
    assert(rcvType.isClass)
    assert(rcvType doesImplement itype)
    val vmtOffs = if (rcvType.isJavaReference) {
      RTConst.JavaInstanceDescriptor.VMT_OFFSET.intValue
    } else if (rcvType.isXScalaType) {
      RTConst.ScalaInstanceDescriptor.VMT_OFFSET.intValue
    } else if (rcvType.isCangjieType) {
      RTConst.CangjieInstanceDescriptor.VMT_OFFSET.intValue
    } else {
      assert(rcvType.isAJManagedType)
      RTConst.ManagedInstanceDescriptor.VMT_OFFSET.intValue
    }
    val offset: Long = vmtOffs + rcvType.getIMTSlot(itype) * AddrType.size
    val shiftedOffs = offset << ciaoOffsetShift
    val idx = rcvType.getSuperInterfaceIndex(itype)
    assert(0 <= idx)
    IntegralConst(AddrIntType)(if (idx < enrichmentIndexLimit) shiftedOffs | (idx + 1) else shiftedOffs)
  }

  def findLightInterfCallResult(call: Call, guardMode: GuardMode): Result = {
    findLightInterfCallResult(call, nodeTypeAt(call.receiver, call), guardMode)
  }

  def findLightInterfCallResult(call: Call, rcvTypeAppr: ReferenceApprox, guardMode: GuardMode): Result = {
    val ref = call.targetRef
    if (!ref.isInterfCall) {
      return Unknown
    }

    val interf = SignatureType.fromSymType(ref.refClass)
    val rcvType = refineReceiverType(rcvTypeAppr, interf)
    rcvType match {
      case rcvType: Cone => rcvType.root match {

        case klass: ClassType if klass implements interf =>
          Inferred(klass.symType)

        case _ if guardMode == GuardMode.NoGuards || env.enabled(NoTauTests) => Unknown

        case _ => (refineTypeSpeculatively(rcvType) weakIntersect refineTypeSpeculatively(cone(interf.symType)))._1 match {
          case UpperBounded(klass: ClassType, _) if klass implements interf =>
            Guarded(OpenConeGuard(klass.symType), rcvType)

          case _ => Unknown
        }
      }

      case _ => shouldNotReachHere("unexpected receiver type after devirtualization: " + rcvType)
    }
  }
}
