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
import com.huawei.excelsior.jet.compiler.bytecode.Bytecode;
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool;
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute;
import com.huawei.excelsior.jet.compiler.bytecode.Tag;
import com.huawei.excelsior.jet.compiler.util.ListHelpers;
import com.huawei.excelsior.jet.compiler.verifier.StackMapFrame;
import com.huawei.excelsior.jet.compiler.verifier.VerifiableMethod;
import com.huawei.excelsior.jet.compiler.verifier.VerificationTypes;
import com.huawei.excelsior.jet.compiler.verifier.VerificationTypes.VerificationType;
import com.huawei.excelsior.jet.compiler.verifier.VerificationUnit;

import scala.Array;
import scala.collection.immutable.List;
import scala.collection.mutable.BitSet;

import java.nio.ByteBuffer;

/**
 * Parsing of StackMapTable attribute
 * (Chapter 4.7.4. The StackMapTable Attribute in JVM specification).
 * <br/>
 * Note that parser throws two kinds of ClassFormatErrors:
 * <ul>
 *     <li>
 *         {@link #verifyStackMap(boolean, String)} is thrown when
 *         stack map is incorrect by itself (e.g. invalid type tag).
 *         In this case fail over to type inference verifier is prohibited.
 *     </li>
 *     <li>
 *         {@link #verifyClassFormat(boolean, String)} is throw when
 *         stack map is incorrect because it is inconsistent with bytecode
 *         which might be modified without taking care of stack map table
 *         (e.g. full_frame's locals count is more than max_locals).
 *         In this case fail over to type inference verifier is allowed.
 *     </li>
 * </ul>
 */
class StackMapTable extends VerificationUnit {

    private final ConstantPool cp;
    private final VerificationTypes types;
    private final MethodCodeAttribute codeAttr;
    private final BitSet instructions;
    private final ByteBuffer attributeBuffer; // may be null

    public StackMapTable(VerifiableMethod method, VerificationTypes types, BitSet instructions) {
        super(true, method);
        this.types = types;
        this.instructions = instructions;

        cp = method.getDeclaringClass().getClassConstantPool();
        codeAttr = method.codeAttribute();
        attributeBuffer = wrap(codeAttr.stackMapTable());
    }

    private static ByteBuffer wrap(byte[] arr) {
      return arr == null ? null : ByteBuffer.wrap(arr);
    }

    /** Reads {@code U1} value and increments position in attribute bytes buffer. */
    private int nextU1() {
        verifyStackMap(attributeBuffer.hasRemaining(), "Unexpected end of StackMapTable attribute");
        return attributeBuffer.get() & 0xff;
    }

    /** Reads {@code U2} value and increments position in attribute bytes buffer. */
    private int nextU2() {
        return (nextU1() << 8) | nextU1();
    }

    private VerificationType nextType() {
        final int tag = nextU1();
        switch (tag) {
            case 0: return types.TOP();
            case 1: return types.INT();
            case 2: return types.FLOAT();
            case 3: return types.DOUBLE();
            case 4: return types.LONG();
            case 5: return types.NULL();
            case 6: return types.thisType(true);
            case 7: return types.classOf(verifyAndGetClassNameValue(nextU2()));
            case 8: return types.uninitialized(verifyNewOffset(nextU2()));

            default:
                verifyStackMap(false, "Invalid verification type tag %d", tag);
                return shouldNotReachHere();
        }
    }

    private XString verifyAndGetClassNameValue(int classCPIndex) {
        verifyStackMap(0 < classCPIndex && classCPIndex < cp.getCount() && cp.getTag(classCPIndex) == Tag.CLASS,
                "Bad constant pool index for verification type in StackMapTable");
        return cp.getClassNameValue(classCPIndex);
    }

    private int verifyNewOffset(int bc) {
        verifyClassFormat(instructions.contains(bc) && codeAttr.bytecodeArray()[bc] == ((byte) Bytecode.NEW.code()),
                "invalid offset for uninitialized in StackMapTable");
        return bc;
    }

    public StackMapFrame[] parse(VerificationType[] entryLocalTypes, int paramsNum, int paramSlotsNum, boolean entryUninitializedThis) {
        if (attributeBuffer == null) {
            return null;
        }

        final ParsingState state = new ParsingState(entryLocalTypes, paramsNum, paramSlotsNum, entryUninitializedThis);

        final int framesCount = nextU2();
        for (int i = 0; i < framesCount; i++) {
            final int type = nextU1();
            if (type <= 63) {
                // same_frame
                state.offsetDelta(type);
                state.sameLocals();
                state.emptyStack();

            } else if (type <= 127) {
                // same_locals_1_stack_item_frame
                state.offsetDelta(type - 64);
                state.sameLocals();
                state.singleStack();

            } else if (type <= 246) {
                // reserved for future use
                verifyStackMap(false, "Reserved frame type tag %d", type);

            } else if (type == 247) {
                // same_locals_1_stack_item_frame_extended
                state.offsetDelta(nextU2());
                state.sameLocals();
                state.singleStack();

            } else if (type <= 250) {
                // chop_frame
                state.offsetDelta(nextU2());
                state.chopLocals(251 - type);
                state.emptyStack();

            } else if (type == 251) {
                // same_frame_extended
                state.offsetDelta(nextU2());
                state.sameLocals();
                state.emptyStack();

            } else if (type <= 254) {
                // append_frame
                state.offsetDelta(nextU2());
                state.appendLocals(type - 251);
                state.emptyStack();

            } else if (type == 255) {
                // full_frame
                state.offsetDelta(nextU2());
                state.fullLocals(nextU2());
                state.stack(nextU2());

            } else {
                return shouldNotReachHere(String.valueOf(type));
            }

            state.buildAndPrepareNext();
        }
        verifyStackMap(!attributeBuffer.hasRemaining(), "Unexpected content at the end of StackMapTable attribute");

        return state.frames;
    }

    private final class ParsingState {
        final StackMapFrame[] frames = new StackMapFrame[codeAttr.bytecodeLength()];

        // locals.length is always equal to maxLocals.
        // localsNum is a number of locals whose types were specified by stack map.
        // localSlotsNum is a number of slots occupied by locals whose types were specified by stack map.
        // Types in range [localSlotsNum, maxLocals) are always equal to TOP.

        int prevOffset;

        VerificationType[] prevLocals;
        int prevLocalsNum;
        int prevLocalSlotsNum;
        boolean prevUninitializedThis;

        int offsetDelta;

        VerificationType[] locals;
        int localsNum;
        int localSlotsNum;
        boolean uninitializedThis;

        List<VerificationType> stack;
        int stackHeight;

        ParsingState(VerificationType[] entryLocalTypes, int entryLocalsNum, int entryLocalSlotsNum, boolean entryUninitializedThis) {
            assert entryLocalTypes.length == codeAttr.maxLocals();

            prevOffset = -1;
            prevLocals = entryLocalTypes;
            prevLocalsNum = entryLocalsNum;
            prevLocalSlotsNum = entryLocalSlotsNum;
            prevUninitializedThis = entryUninitializedThis;

            reset();
        }

        void offsetDelta(int delta) {
            offsetDelta = delta;
        }

        void sameLocals() {
            locals = prevLocals;
            localsNum = prevLocalsNum;
            localSlotsNum = prevLocalSlotsNum;
            uninitializedThis = prevUninitializedThis;
        }

        void chopLocals(int choppedCount) {
            localsNum = prevLocalsNum - choppedCount;
            verifyClassFormat(localsNum >= 0, "Stack map locals count underflow");
            locals = new VerificationType[codeAttr.maxLocals()];
            uninitializedThis = false;
            copyPreviousLocals(localsNum);
        }

        void appendLocals(int appendedCount) {
            localsNum = prevLocalsNum + appendedCount;
            locals = (VerificationType[]) Array.copyOf(prevLocals, prevLocals.length); // prevLocals.length == maxLocals
            uninitializedThis = prevUninitializedThis;
            parseLocals(prevLocalSlotsNum, appendedCount);
        }

        void fullLocals(int localsNum) {
            this.localsNum = localsNum;
            locals = new VerificationType[codeAttr.maxLocals()];
            uninitializedThis = false;
            parseLocals(0, localsNum);
        }

        private void copyPreviousLocals(int count) {
            int index = 0;
            for (int l = 0; l < count; l++) {
                final VerificationType localType = prevLocals[index];
                if (localType.isUninitializedThis()) {
                    uninitializedThis = true;
                }
                locals[index++] = localType;
                if (localType.is2Slots()) {
                    locals[index++] = localType.get2ndHalf();
                }
            }
            localSlotsNum = index;
            fillLocalsTail();
        }

        private void parseLocals(int startIndex, int count) {
            int index = startIndex;
            for (int l = 0; l < count; l++) {
                final VerificationType localType = nextType();
                if (localType.isUninitializedThis()) {
                    uninitializedThis = true;
                }
                putLocalIfInRange(index++, localType);
                if (localType.is2Slots()) {
                    putLocalIfInRange(index++, localType.get2ndHalf());
                }
            }
            // This fall-back-able check must be performed after all stack map types are verified (non-fall-back-able checks).
            verifyClassFormat(index <= codeAttr.maxLocals(), "Stack map frame locals index is out of range");
            localSlotsNum = index;
            fillLocalsTail();
        }

        private void putLocalIfInRange(int index, VerificationType value) {
            if (index < locals.length) {
                locals[index] = value;
            }
        }

        private void fillLocalsTail() {
            // TODO-DECAF: provide own `Arrays.fill` implementation and use it here
            final VerificationType val = types.TOP();
            for (int i = localSlotsNum, length = locals.length; i < length; i++)
                locals[i] = val;
        }

        void stack(int stackItemsCount) {
            stack = ListHelpers.empty();
            stackHeight = 0;
            for (int i = 0; i < stackItemsCount; i++) {
                final VerificationType stackItemType = nextType();
                if (stackItemType.is2Slots()) {
                    stack = ListHelpers.prepended(stackItemType.get2ndHalf(), stack);
                    stackHeight++;
                }
                stack = ListHelpers.prepended(stackItemType, stack);
                stackHeight++;
            }
            // This fall-back-able check must be performed after all stack map types are verified (non-fall-back-able checks).
            verifyClassFormat(stackHeight <= codeAttr.maxStack(), "Stack map frame stack size is out of range");
        }

        void emptyStack() {
            stack(0);
        }

        void singleStack() {
            stack(1);
        }

        private void assertInitialized() {
            assert offsetDelta != -1;
            assert locals != null;
            assert localsNum != -1;
            assert localSlotsNum != -1;
            assert stack != null;
            assert stackHeight != -1;
        }

        void buildAndPrepareNext() {
            assertInitialized();

            final int offset = prevOffset + offsetDelta + 1;
            verifyThat(offset < codeAttr.bytecodeLength(), "Stack map frame offset is out of range");
            verifyThat(instructions.contains(offset), "No instruction at offset %d in StackMapTable", offset);

            frames[offset] = new StackMapFrame(locals, stack, stackHeight, uninitializedThis);

            prevOffset = offset;
            prevLocals = locals;
            prevLocalsNum = localsNum;
            prevLocalSlotsNum = localSlotsNum;
            prevUninitializedThis = uninitializedThis;

            reset();
        }

        private void reset() {
            offsetDelta = -1;
            locals = null;
            localSlotsNum = -1;
            localsNum = -1;
            stack = null;
            stackHeight = -1;
            uninitializedThis = false;
        }
    }
}
