/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier.checking;

import static com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere;

import com.huawei.excelsior.jet.common.XString;
import com.huawei.excelsior.jet.compiler.Environment;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeIterator;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind;
import com.huawei.excelsior.jet.compiler.bytecode.CompareOp;
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute.ExceptionTableTraverser;
import com.huawei.excelsior.jet.compiler.util.ListHelpers;
import com.huawei.excelsior.jet.compiler.verifier.AbstractVerifier.InfoBuilder;
import com.huawei.excelsior.jet.compiler.verifier.ClassVerifier;
import com.huawei.excelsior.jet.compiler.verifier.StackMapFrame;
import com.huawei.excelsior.jet.compiler.verifier.StaticConstraintsVerifier;
import com.huawei.excelsior.jet.compiler.verifier.VerifiableMethod;
import com.huawei.excelsior.jet.compiler.verifier.VerifiableType;

import scala.collection.mutable.BitSet;

/**
 * Java bytecode verifier via Type Checking.
 * (Chapter 4.10.1. Verification by Type Checking of JVM specification.)
 * <br/>
 * This implementation performs two bytecode passes: first static constraints are checked
 * and then structural constraints are checked using parsed stack map table.
 * These passes may be merged to improve efficiency but currently there is no motivation for this.
 *
 * @author kit
 * @author cypok
 */
public final class TypeCheckingVerifier extends ClassVerifier {

    public TypeCheckingVerifier(Environment env, InfoBuilder builder, int cfVersion, VerifiableType thisClass) {
        super(env, builder, cfVersion, thisClass);
    }

    @Override
    protected MethodVerifier createMethodVerifier(VerifiableMethod method) {
        return new TypeCheckingMethodVerifier(method);
    }

    private final class TypeCheckingMethodVerifier extends MethodVerifier {

        TypeCheckingMethodVerifier(VerifiableMethod method) {
            super(method);
        }

        @Override
        public void verify() {
            final BitSet instructions = new StaticConstraintsVerifier(method, types, cfVersion).verify();

            final StackMapFrame entryFrame = new StackMapFrame(entryLocalTypes, ListHelpers.empty(), 0, entryUninitializedThis);
            final StackMapFrame[] frames = new StackMapTable(method, types, instructions)
                    .parse(entryLocalTypes, paramsNum, paramSlotsNum, entryUninitializedThis);
            final TypeCheckingBytecodeVerifier bcVerifier = new TypeCheckingBytecodeVerifier(frames, entryFrame);

            final BytecodeIterator bc = new BytecodeIterator(codeAttr, false, method);
            bc.iterate(bcVerifier, 0, codeAttr.bytecodeLength());
        }

        private class TypeCheckingBytecodeVerifier extends BytecodeVerifier {

            private final StackMapFrame[] frames;

            private final int minHandlerStartPC;
            private final int maxHandlerEndPC;

            private boolean fallThrough = true;

            private int nextBC;

            TypeCheckingBytecodeVerifier(StackMapFrame[] frames, StackMapFrame state) {
                super(state);
                this.frames = frames;

                int minHandlerStartPC = Integer.MAX_VALUE;
                int maxHandlerEndPC = -1;
                for (final ExceptionTableTraverser it = codeAttr.getExceptionTableTraverser(); it.hasNext(); ) {
                    it.queryNext();
                    minHandlerStartPC = Math.min(it.startPC(), minHandlerStartPC);
                    maxHandlerEndPC = Math.max(it.endPC(), maxHandlerEndPC);
                    verifyAndGetStackMap(it.handlerPC());
                }
                this.minHandlerStartPC = minHandlerStartPC;
                this.maxHandlerEndPC = maxHandlerEndPC;
            }

            private StackMapFrame getStackMap(int bc) {
                return (frames != null && 0 <= bc && bc < frames.length) ? frames[bc] : null;
            }

            private StackMapFrame verifyAndGetStackMap(int bc) {
                final StackMapFrame frame = getStackMap(bc);
                verifyThat(frame != null, "No stack map frame at offset %d", bc);
                return frame;
            }

            private void verifyStackMapTarget(int bc) {
                verifyAndGetStackMap(bc).verifyAssignableFrom(TypeCheckingMethodVerifier.this, state);
            }

            @Override
            public void startInstruction(int offset, int nextOffset) {
                super.startInstruction(offset, nextOffset);
                nextBC = nextOffset;

                if (fallThrough) {
                    final StackMapFrame frame = getStackMap(curBC);
                    if (frame != null) {
                        frame.verifyAssignableFrom(TypeCheckingMethodVerifier.this, state);
                        state.copyFrom(frame);
                    }
                } else {
                    final StackMapFrame frame = verifyAndGetStackMap(curBC);
                    state.copyFrom(frame);
                }

                fallThrough = true;
            }

            private void setNotFallThrough() {
                fallThrough = false;
            }

            @Override
            public void finishInstruction() {
                verifyThat(!fallThrough || (nextBC < codeAttr.bytecodeLength()), "Fall of the bottom of the code");

                if ((minHandlerStartPC <= curBC) && (curBC < maxHandlerEndPC)) {
                    StackMapFrame xState = null;
                    for (final ExceptionTableTraverser it = codeAttr.getExceptionTableTraverser(); it.hasNext(); ) {
                        it.queryNext();
                        if ((it.startPC() <= curBC) && (curBC < it.endPC())) {
                            if (xState == null) {
                                // FIXME: locals and flags must be taken from the state _before_ instruction interpretation, JET-11686
                                xState = new StackMapFrame(state.getLocalsUnsafe(), null, 1, state.uninitializedThis);
                            }

                            final XString catchTypeName = it.catchTypeName();
                            xState.stack = ListHelpers.single(catchTypeName == null ? types.THROWABLE() : types.classOf(catchTypeName));

                            final StackMapFrame handlerFrame = getStackMap(it.handlerPC());
                            assert handlerFrame != null : "it's checked earlier";
                            handlerFrame.verifyAssignableFrom(TypeCheckingMethodVerifier.this, xState);
                        }
                    }
                } // otherwise it's guaranteed that there are no handlers
            }

            @Override
            protected void anySwitch(int bcDefault, int[] bcTargets) {
                super.anySwitch(bcDefault, bcTargets);
                verifyStackMapTarget(bcDefault);
                for (int bcTarget : bcTargets) {
                    verifyStackMapTarget(bcTarget);
                }
                setNotFallThrough();
            }

            @Override
            public void doReturn(BytecodeTypeKind tkind, boolean isLastBytecode) {
                super.doReturn(tkind, isLastBytecode);
                setNotFallThrough();
            }

            @Override
            public void doThrow() {
                super.doThrow();
                setNotFallThrough();
            }

            @Override
            public void jsr(int bc) {
                super.jsr(bc);
                verifyStackMapTarget(bc);
                setNotFallThrough();
            }

            @Override
            public void jump(int bc) {
                verifyStackMapTarget(bc);
                setNotFallThrough();
            }

            @Override
            public void unaryIf(BytecodeTypeKind tkind, CompareOp op, int bc) {
                super.unaryIf(tkind, op, bc);
                verifyStackMapTarget(bc);
            }

            @Override
            public void binaryIf(BytecodeTypeKind tkind, CompareOp op, int bc) {
                super.binaryIf(tkind, op, bc);
                verifyStackMapTarget(bc);
            }

            @Override
            public void ret(int var) {
                shouldNotReachHere("RET instruction is not allowed during type checking verification");
            }
        }
    }
}
