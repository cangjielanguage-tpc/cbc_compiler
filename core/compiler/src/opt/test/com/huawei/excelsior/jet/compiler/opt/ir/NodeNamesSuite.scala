/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.opt.ir

import com.huawei.excelsior.jet.assembler.cbc.{CbcTypeKind, FieldReference}
import com.huawei.excelsior.jet.common.XString
import com.huawei.excelsior.jet.compiler.CompilerSuite
import com.huawei.excelsior.jet.compiler.RTSProc.JR_FatalError
import com.huawei.excelsior.jet.compiler.bytecode.{BytecodePosition, MethodAccessKind}
import com.huawei.excelsior.jet.compiler.ir.InlineContext
import com.huawei.excelsior.jet.compiler.opt.ir.nodes.HLIRNodes
import com.huawei.excelsior.jet.compiler.opt.ir.Resources.FrameSlot
import com.huawei.excelsior.jet.compiler.opt.testutils.DSLs.GlobalNodesBuilder
import com.huawei.excelsior.jet.compiler.symlevel.TypeKind.{CLASS, RECORD}
import com.huawei.excelsior.jet.compiler.symlevel.{CallKind, SignatureType}
import com.huawei.excelsior.jet.compiler.symlevel.impl.fake.{FakeField, FakeMethod, FakeMethodReference, FakeMethodType, FakeType}
import com.huawei.excelsior.jet.compiler.types.TypesToolbox

class NodeNamesSuite extends CompilerSuite with GlobalNodesBuilder with TypesToolbox with HLIRNodes {

  // pair
  private def p(nLazy: => Node, name: String) =
    (() => nLazy, name)

  private def refClass = FakeType.create(classOf[System])
  private def method = refClass.method("gc")
  private def methodRef = new FakeMethodReference(method, MethodAccessKind.STATIC, refClass)
  private def inlineContext = InlineContext.newRoot(method)

  private def someAddr = IntegralConst(AddrType)(42)

  for (((nLazy, expectedName), pos) <- Seq(

    tp(p(FConst(3.14f),
      "FConst[3.14 # 4048f5c3]")),

    tp(p(DConst(3.14),
      "DConst[3.14 # 40091eb851eb851f]")),

    tp(p(FConst(Float.NaN),
      "FConst[NaN # 7fc00000]")),

    tp(p(DConst(-Double.PositiveInfinity),
      "DConst[-Infinity # fff0000000000000]")),

    // Trivial call:
    tp(p(Invoke(methodRef)(),
      "Call[STATIC, java/lang/System.gc()V]")),

    // Non-trivial refClass:
    tp(p(InvokeInterface(new FakeMethodReference(FakeType.create(classOf[String]).method("charAt"), MethodAccessKind.INTERFACE, FakeType.create(classOf[CharSequence])), someAddr)(Null(), IConst(37)),
      "Call[INTERFACE, refClass: java/lang/CharSequence, java/lang/String.charAt(i32)u16]")),

    // Call without method:
    tp(p(Call(new FakeMethodReference(FakeMethodType.create().changeCallKind(CallKind.CJ_FOREIGN)))(someAddr),
      "Call[STATIC, ()V, MANAGED, CJ_FOREIGN]")),

    tp(p(StackAlloc.raw(37, 42),
      "StackAlloc[Raw(37,42)]")),

    tp(p(StackAlloc(FrameSlot.NewOnStack(SignatureType.fromSymType(tObj))),
      "StackAlloc[NewOnStack(JBCSymReference(class java/lang/Object))]")),

    tp(p(StackAlloc(FrameSlot.NewArrayOnStack(SignatureType.fromSymType(tObj1D), 10)),
      "StackAlloc[NewArrayOnStack(JavaArray(JBCSymReference(class java/lang/Object),1),10)]")),

    tp(p(StackAlloc.Local(sig(symTX)),
      "StackAlloc[Local(JBCSymReference(thin TX))]")),

    tp(p(DelayedPut(xstr("foo"), xstr("bar"), TRefType)(Null(), Null()),
      "DelayedPut[foo.bar]")),

    tp(p(withPos(BytecodePosition(37, 42, 99, inlineContext)) { DebugTextPosBreakpoint() },
      "DebugTextPosBreakpoint[line: 42 col: 99 bc: 37 [java/lang/System.gc()V:*]]")),

    tp(p(Halt.explained("DebugInfo")(),
      "Halt[Reason: DebugInfo]")),

    tp(p(Halt.afterRTSCall(JR_FatalError, "DebugInfo")(),
      "Halt[Inserted after ErrorRTSCall JR_FatalError. Reason: DebugInfo]")),

    tp(p({
      val classA = FakeType("A", CLASS)
      val recR = FakeType("R", RECORD)
      val rInA = new FakeField("r", SignatureType.Record("R"))
      val cInR = new FakeField("c", SignatureType.Int32)
      classA.addField(rInA)
      recR.addField(cInR)
      val objA = Fake(ValueType(classA))
      val fields = List(FieldReference.forNonGenericFieldType(classA.getTypeInfo, rInA.getPermanent, CbcTypeKind(rInA.getType.toAsm)),
        FieldReference.forNonGenericFieldType(recR.getTypeInfo, cInR.getPermanent, CbcTypeKind(cInR.getType.toAsm)))
      FieldChainRead(fields, InstanceFieldOperation.declaringClassType(rInA), ValueType.apply(SignatureType.Int32))(objA)
    }, "FieldChainRead[FieldReference(NonGenericType,field A#r,A,null,U64) -> FieldReference(NonGenericType,field R#c,R,null,I32)]"))


  )) {
    test(expectedName) {
      makeCFG(0)
      val n = makeNodes { at =>
        at(0)
        nLazy()
      }
      n.name shouldBe expectedName
    }
  }

}
