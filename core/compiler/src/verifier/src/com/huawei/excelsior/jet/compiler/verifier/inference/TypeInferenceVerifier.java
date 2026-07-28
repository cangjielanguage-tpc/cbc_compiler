/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier.inference;

import com.huawei.excelsior.jet.compiler.Environment;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeIterator;
import com.huawei.excelsior.jet.compiler.bytecode.parsing.CompleteControlFlowParser;
import com.huawei.excelsior.jet.compiler.bytecode.parsing.DataFlowAnalyzer;
import com.huawei.excelsior.jet.compiler.bytecode.parsing.DataFlowMergeResult;
import com.huawei.excelsior.jet.compiler.bytecode.parsing.DataFlowMergeResult$;
import com.huawei.excelsior.jet.compiler.bytecode.parsing.XHInfo;
import com.huawei.excelsior.jet.compiler.util.ListHelpers;
import com.huawei.excelsior.jet.util.SuffixTree;
import com.huawei.excelsior.jet.compiler.verifier.AbstractVerifier.InfoBuilder;
import com.huawei.excelsior.jet.compiler.verifier.ClassVerifier;
import com.huawei.excelsior.jet.compiler.verifier.StackMapFrame;
import com.huawei.excelsior.jet.compiler.verifier.StaticConstraintsVerifier;
import com.huawei.excelsior.jet.compiler.verifier.VerifiableMethod;
import com.huawei.excelsior.jet.compiler.verifier.VerifiableType;
import com.huawei.excelsior.jet.compiler.verifier.VerificationTypes.VerificationType;

import scala.Tuple2;
import scala.collection.immutable.List;
import scala.collection.mutable.BitSet;
import scala.reflect.ClassTag;

/**
 * Java bytecode verifier via type inference data-flow analysis.
 * <br/>
 * Note that subroutines (JSR/RET) are handled separately in {@link CompleteControlFlowParser}:
 * they are verified in {@link com.huawei.excelsior.jet.compiler.bytecode.parsing.subroutines.SubroutineAnalyzer}
 * and inlined by {@link com.huawei.excelsior.jet.compiler.bytecode.parsing.subroutines.SubroutineInliner}.
 * Further verification mostly ignores semantics of JSR/RET.
 * Such approach dramatically simplifies type inference verifier.
 * However this leads to verification of some methods which are unverifiable by reference implementation
 * (see core/compiler/fun-tests/tests/verifier/TooSmartLocalTypesInSubroutines.jasm).
 *
 * @author kit
 * @author cypok
 */
public final class TypeInferenceVerifier extends ClassVerifier {

    public TypeInferenceVerifier(Environment env, InfoBuilder builder, int cfVersion, VerifiableType thisClass) {
        super(env, builder, cfVersion, thisClass);
    }

    @Override
    protected MethodVerifier createMethodVerifier(VerifiableMethod method) {
        return new TypeInferenceMethodVerifier(method);
    }

    // TODO: Implement CFG class and use it here instead of ControlFlowParser.
    final class TypeInferenceMethodVerifier extends MethodVerifier {
        private final ControlFlowParser cfg;

        TypeInferenceMethodVerifier(VerifiableMethod method) {
            super(method);

            cfg = new ControlFlowParser(TypeInferenceVerifier.this.env, method, true);
        }

        @Override
        public void verify() {
            new StaticConstraintsVerifier(method, types, cfVersion).verify();
            cfg.parse();

            final Block entryBlock = cfg.entryBlock();
            entryBlock.setVerificationState(new State(entryBlock, entryLocalTypes, ListHelpers.empty(), 0, entryUninitializedThis));

            if (entryBlock.hasNoOutputs() && cfg.blockHandlersTree().get(entryBlock).isEmpty()) {
                // Super popular fast path: single block without back edges and handler.
                verifyOneBlock(entryBlock, entryBlock.getVerificationState(), null);
            } else {
                final Analyzer dataFlowAnalyzer = new Analyzer();
                dataFlowAnalyzer.verifyBlocks();
            }
        }

        private VerificationType getCatchType(Block handler, Block handled) {
            VerificationType catchType = types.NULL();
            SuffixTree<XHInfo<Block>> handlerInfos = cfg.handlers(handled);
            while (!handlerInfos.isRoot()) {
                final XHInfo<Block> info = handlerInfos.elem();
                if (handler.equals(info.handler())) {
                    final VerificationType oneCatchType = info.isCatchAll() ? types.THROWABLE() : types.classOf(info.catchTypeName());
                    catchType = catchType.merge(oneCatchType);
                }
                handlerInfos = handlerInfos.parent();
            }
            assert !catchType.isNull();
            return catchType;
        }

        final class State extends StackMapFrame implements DataFlowAnalyzer.State<State> {

            final Block block;

            // Is used to prevent multiple iterations over CFG in case of diamonds.
            boolean wasProcessed;

            public State(Block block, VerificationType[] locals, List<VerificationType> stack, int stackHeight, boolean uninitializedThis) {
                super(locals, stack, stackHeight, uninitializedThis);
                this.block = block;
                this.wasProcessed = false;
            }

            boolean isBottom() {
                return this.locals == null;
            }

            @Override
            public DataFlowMergeResult mergeFrom(State that) {
                // Compute catch type: this.block - exception handler, that.block - protected code.
                final List<VerificationType> thatStack;
                if (that.stackHeight == 1 && that.stack.head().equals(types.CATCH_TYPE_PLACEHOLDER())) {
                    thatStack = ListHelpers.single(getCatchType(this.block, that.block));
                } else {
                    thatStack = that.stack;
                }

                if (this.isBottom()) {
                    assert !wasProcessed;
                    this.locals = that.borrowLocals();
                    this.stack = thatStack;
                    this.stackHeight = that.stackHeight;
                    this.uninitializedThis = that.uninitializedThis;
                    return DataFlowMergeResult$.INITIALIZED;
                }

                boolean changed = false;

                for (int i = 0; i < codeAttr.maxLocals(); i++) {
                    final VerificationType thisType = this.readLocal(i);
                    final VerificationType thatType = that.readLocal(i);

                    if (!thisType.equals(thatType)) {
                        changed |= this.writeLocal(i, thisType.merge(thatType));
                    }
                }

                verifyThat(this.stackHeight == that.stackHeight,
                        "Stack height mismatch: %d != %d", this.stackHeight, that.stackHeight);

                final List<VerificationType> mergedStack = mergeStacks(this.stack, thatStack);
                if (this.stack != mergedStack) {
                    this.stack = mergedStack;
                    changed = true;
                }

                return changed ? (wasProcessed ? DataFlowMergeResult$.CHANGED : DataFlowMergeResult$.INITIALIZED) : DataFlowMergeResult$.UNCHANGED;
            }

            private List<VerificationType> mergeStacks(List<VerificationType> thisStack, List<VerificationType> thatStack) {
                assert thisStack.isEmpty() == thatStack.isEmpty();

                if (thisStack.isEmpty()) {
                    return thisStack;
                }

                final List<VerificationType> thisTail = ListHelpers.tail(thisStack);
                final List<VerificationType> thatTail = ListHelpers.tail(thatStack);
                final List<VerificationType> mergedTail = mergeStacks(thisTail, thatTail);
                final boolean changedTail = thisTail != mergedTail;

                final VerificationType thisType = thisStack.head();
                final VerificationType thatType = thatStack.head();

                final VerificationType mergedType;
                if (!thisType.equals(thatType)) {
                    mergedType = thisType.merge(thatType);
                    verifyThat(!mergedType.equals(types.TOP()), "Stack types cannot be merged");
                } else {
                    mergedType = thisType;
                }
                final boolean changedHead = !thisType.equals(mergedType);

                if (changedHead || changedTail) {
                    return ListHelpers.prepended(mergedType, mergedTail);
                } else {
                    return thisStack;
                }
            }

            public State copy() {
                return new State(
                        block,
                        borrowLocals(),
                        stack,
                        stackHeight,
                        uninitializedThis);
            }

            public State xCopy() {
                assert codeAttr.maxStack() > 0;
                return new State(
                        block,
                        borrowLocals(),
                        ListHelpers.single(types.CATCH_TYPE_PLACEHOLDER()),
                        1,
                        uninitializedThis);
            }

        }

        private State newBottom(Block block) {
            return new State(block, null, null, -1, false);
        }

        private final class Analyzer extends DataFlowAnalyzer.RoundRobinVersion<Block, State> {

            Analyzer() {
                super(true, TypeInferenceMethodVerifier.this.verificationContext(), ClassTag.apply(Block.class));
            }

            void verifyBlocks() {
                analyze(new TopSort().topSortedBlocks());
            }

            @Override
            public TypeInferenceMethodVerifier.State inputState(Block block) {
                TypeInferenceMethodVerifier.State state = block.getVerificationState();
                if (state == null) {
                    state = newBottom(block);
                    block.setVerificationState(state);
                }
                return state;
            }


            @Override
            public BlockProcessResult<TypeInferenceMethodVerifier.State> processBlock(Block block, TypeInferenceMethodVerifier.State inputState) {
                inputState.wasProcessed = true;
                final TypeInferenceMethodVerifier.State state = inputState.copy();
                final TypeInferenceMethodVerifier.State xState = hasHandlers(block) ? inputState.xCopy() : null;

                verifyOneBlock(block, state, xState);

                return new BlockProcessResult<>(state, xState);
            }

            ////////////////////////
            // DEBUGGING

            @Override
            public boolean debugEnabled() {
                return false;
            }

            @Override
            public scala.collection.Map<Block, TypeInferenceMethodVerifier.State> allInputStatesForDebug() {
                return scala.collection.immutable.Map.from(
                        cfg.allBlocks().iterator()
                                .filter((Block x) -> x.getVerificationState() != null)
                                .map((Block x) -> Tuple2.apply(x, x.getVerificationState())));
            }

            // DEBUGGING
            ////////////////////////

            @Override public Block entryBlock() { return cfg.entryBlock(); }
            @Override public scala.collection.Iterator<Block> succBlocks(Block block) { return cfg.succBlocks(block); }
            @Override public scala.collection.Iterator<Block> handlerBlocks(Block block) { return cfg.handlerBlocks(block); }
        }

        private void verifyOneBlock(Block block, State state, State xState) {
            verifyThat(!block.isFallOfTheBottom(), "Fall of the bottom of the code");

            final TypeInferenceBytecodeVerifier bcVerifier = new TypeInferenceBytecodeVerifier(state, xState);

            final BytecodeIterator bc = new BytecodeIterator(codeAttr, false, null);
            bc.iterate(bcVerifier, block.startBC(), block.endBC());
        }

        private class TypeInferenceBytecodeVerifier extends BytecodeVerifier {

            private final StackMapFrame xState; // null iff block has no handlers

            TypeInferenceBytecodeVerifier(StackMapFrame state, StackMapFrame xState) {
                super(state);
                this.xState = xState;
            }

            @Override
            protected void writeRaw(int localIdx, VerificationType value) {
                super.writeRaw(localIdx, value);
                if (xState != null) {
                    final VerificationType old = xState.readLocal(localIdx);
                    xState.writeLocal(localIdx, old.merge(value));
                }
            }

            @Override
            protected void initUninitializedInLocals(VerificationType uninitialized, VerificationType initialized) {
                super.initUninitializedInLocals(uninitialized, initialized);
                if (xState != null) {
                    for (int i = 0; i < codeAttr.maxLocals(); i++) {
                        if (xState.readLocal(i).equals(uninitialized)) {
                            xState.writeLocal(i, types.TOP());
                        }
                    }
                }
            }
        }

        private final class TopSort extends com.huawei.excelsior.jet.compiler.bytecode.parsing.TopSort<Block> {
            final BitSet visited = new BitSet();

            TopSort() {
                perform();
            }

            @Override
            public boolean markVisited(Block block) {
                final int id = block.id();
                if (visited.contains(id)) {
                    return false;
                } else {
                    visited.addOne(id);
                    return true;
                }
            }

            @Override public Block entryBlock() { return cfg.entryBlock(); }
            @Override public scala.collection.Iterator<Block> succBlocks(Block block) { return cfg.succBlocks(block); }
            @Override public scala.collection.Iterator<Block> handlerBlocks(Block block) { return cfg.handlerBlocks(block); }
        }
    }
}
