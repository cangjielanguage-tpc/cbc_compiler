/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier;

import static com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere;

import com.huawei.excelsior.common.CodeHelpers;
import com.huawei.excelsior.jet.classfile.SignatureTraverser;
import com.huawei.excelsior.jet.common.XString;
import com.huawei.excelsior.jet.compiler.bytecode.Bytecode;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeIterator;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind$;
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool;
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute;
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute.ExceptionTableTraverser;
import com.huawei.excelsior.jet.compiler.bytecode.OpKind;
import com.huawei.excelsior.jet.compiler.bytecode.Tag;
import com.huawei.excelsior.jet.compiler.verifier.VerificationTypes.VerificationType;

import scala.collection.mutable.BitSet;

/**
 * Verification of bytecode static constraints as described in JVM specification
 * (Chapter 4.9.1 Static Constraints).
 * In general it checks constraints which do not require data-flow analysis.
 *
 * @author kit
 * @author cypok
 */
public class StaticConstraintsVerifier extends VerificationUnit {

    private static final int MAX_JVM_ARITY = 255;

    private final VerifiableMethod method;
    private final MethodCodeAttribute codeAttr;
    private final ConstantPool cp;
    private final VerificationTypes types;
    private final int cfVersion;

    private final BitSet instructions;
    private final BitSet targets;

    public StaticConstraintsVerifier(VerifiableMethod method, VerificationTypes types, int cfVersion) {
        super(true, method);
        this.method = method;
        this.types = types;
        this.cfVersion = cfVersion;

        codeAttr = method.codeAttribute();
        cp = method.getDeclaringClass().getClassConstantPool();

        instructions = new BitSet(codeAttr.bytecodeLength());
        targets = new BitSet(codeAttr.bytecodeLength());
    }

    /**
     * Returns set of bytecode positions corresponding to starts of instructions.
     */
    public BitSet verify() {
        assert instructions.isEmpty() && targets.isEmpty();
        verifySignature();
        parseInstructions();
        verifyExceptionTable();
        return instructions;
    }

    private void verifySignature() {
        final int maxLocals = codeAttr.maxLocals();
        final int paramSlots = countParamSlots(method.isStatic(), SignatureTraverser.fromString(method.getXSignature()));
        verifyThat(paramSlots <= maxLocals, "max_locals is too small to hold parameters");
    }

    private void parseInstructions() {
        final BytecodeIterator bc = new BytecodeIterator(codeAttr, true, method);

        while (bc.hasNext()) {
            instructions.addOne(bc.offset());

            final Bytecode op = bc.next();
            switch (op.kind()) {
                case CONTROL:
                    switch (op) {
                        case GOTO:
                        case GOTO_W:
                            markTarget(bc.param());
                            break;

                        case JSR:
                        case JSR_W:
                            verifyThat(cfVersion < CFVersion.NO_JSR, "jsr instruction is prohibited in class file of version %d", cfVersion);
                            markTarget(bc.param());
                            break;

                        case RET:
                            verifyLocal(BytecodeTypeKind$.CLASS, bc.param());
                            break;

                        case ATHROW:
                            break;

                        case TABLESWITCH:
                            verifyTableSwitch(bc);
                            break;

                        case LOOKUPSWITCH:
                            verifyLookupSwitch(bc);
                            break;

                        default:
                            shouldNotReachHere(String.valueOf(op));
                            break;
                    }
                    break;

                case CONST:
                case ARITH:
                case CONVERT:
                case STACK:
                case ARRAYGET:
                case ARRAYPUT:
                case XRETURN:
                    break;

                case UNARY_IF:
                case BINARY_IF:
                    markTarget(bc.param());
                    break;

                case LOAD:
                    verifyLocal(op.resultType(), bc.param());
                    break;
                case STORE:
                    verifyLocal(op.operandType(), bc.param());
                    break;

                case OTHER:
                    switch (op) {
                        case NOP:
                        case ARRAYLENGTH:
                            break;

                        case MONITORENTER:
                        case MONITOREXIT:
                            method.markAsContainingMonitorOperations();
                            break;

                        case LDC:
                        case LDC_W:
                            verifyLdcCPIndex(bc.param());
                            break;
                        case LDC2_W:
                            verifyLdc2CPIndex(bc.param());
                            break;

                        case IINC:
                            verifyLocal(BytecodeTypeKind$.INT, bc.param(0));
                            break;

                        case GETSTATIC:
                        case PUTSTATIC:
                        case GETFIELD:
                        case PUTFIELD:
                            verifyFieldCPIndex(bc.param());
                            break;

                        case INVOKEVIRTUAL:
                        case INVOKESPECIAL:
                        case INVOKESTATIC:
                            verifyInvokeCPIndex(bc.param(0), op);
                            break;
                        case INVOKEINTERFACE: {
                            final int index = bc.param(0);
                            verifyInvokeCPIndex(index, op);
                            verifyInvokeInterfaceParamCount(index, bc.param(1));
                            verifyThat(bc.param(2) == 0, "Fourth operand of invoke interface must be zero");
                            break;
                        }
                        case INVOKEDYNAMIC:
                            verifyInvokeCPIndex(bc.param(0), op);
                            verifyThat(bc.param(1) == 0 && bc.param(2) == 0, "Third and fourth operand of invoke dynamic must be zero");
                            break;

                        case NEW: {
                            final int index = bc.param();
                            verifyClassCPIndex(index, op);
                            verifyThat(getCPArrayDimensions(index) == 0, "new cannot be used for array creation");
                            break;
                        }
                        case ANEWARRAY: {
                            final int index = bc.param();
                            verifyClassCPIndex(index, op);
                            verifyThat(1 + getCPArrayDimensions(index) <= MAX_JVM_ARITY, "Too many dimensions for new array");
                            break;
                        }
                        case MULTIANEWARRAY: {
                            final int index = bc.param(0);
                            final int dimNum = bc.param(1);
                            verifyClassCPIndex(index, op);
                            final int dimAll = getCPArrayDimensions(index);
                            verifyThat(dimAll <= MAX_JVM_ARITY, "Too many dimensions for multi new array");
                            verifyThat(0 < dimNum && dimNum <= dimAll, "Invalid creation of multi new array");
                            break;
                        }

                        case CHECKCAST:
                        case INSTANCEOF:
                            verifyClassCPIndex(bc.param(), op);
                            break;

                        case NEWARRAY:
                            final int basicType = bc.param();
                            verifyThat(Bytecode.NEWARRAY_BASIC_TYPE_KIND_START() <= basicType && basicType <= Bytecode.NEWARRAY_BASIC_TYPE_KIND_END(),
                                    "Bad newarray basic type %d", basicType);
                            break;

                        default:
                            shouldNotReachHere(String.valueOf(op));
                            return;
                    }
                    break;

                case RESERVED:
                    verifyThat(false, "Unknown instruction op code: %d", op.code() & 0xff);

                default:
                    shouldNotReachHere(String.valueOf(op));
                    return;
            }
        }

        verifyTargets();
    }

    private void verifyTargets() {
        targets.foreach(v -> {
            int target = (Integer) v;
            verifyThat(instructions.contains(target), "Bad instruction target pc %d", target);
            return null;
        });
    }

    private void verifyExceptionTablePC(int pc) {
        verifyClassFormat(instructions.contains(pc),
                "Bad exception table pc %d", pc);
    }

    private void verifyExceptionTable() {
        final ExceptionTableTraverser xTable = codeAttr.getExceptionTableTraverser();
        while (xTable.hasNext()) {
            xTable.queryNext();
            
            final int startPC = xTable.startPC();
            verifyExceptionTablePC(startPC);
            markTarget(startPC);

            final int endPC = xTable.endPC();
            if (endPC != codeAttr.bytecodeLength()) {
                verifyExceptionTablePC(endPC);
                markTarget(endPC);
            }

            verifyClassFormat(startPC < endPC,
                    "Bad exception table range [%d, %d)", startPC, endPC);

            verifyExceptionTablePC(xTable.handlerPC());

            verifyCatchTypeIndex(xTable.catchTypeIndex());
        }
    }

    private void markTarget(int targetBC) {
        verifyThat(0 <= targetBC && targetBC < codeAttr.bytecodeLength(), "PC %d is out of bytecode range", targetBC);
        targets.addOne(targetBC);
    }

    /////////////////////////////////////
    // ConstantPool stuff:

    private int countParamSlots(boolean statik, SignatureTraverser sig) {
        int count = statik ? 0 : 1;
        while (sig.hasNext()) {
            count += sig.getSlotsNum();
            sig.queryNext();
        }
        // queried return type is ignored
        return count;
    }

    private Tag verifyIndexAndGetTag(int index) {
        verifyThat(0 < index && index < cp.getCount(), "Invalid constant pool index %d", index);
        return cp.getTag(index);
    }

    private void verifyLdcCPIndex(int index) {
        final Tag tag = verifyIndexAndGetTag(index);
        final boolean isValid;
        switch (tag) {
            case INTEGER:
            case FLOAT:
            case STRING:
                isValid = true;
                break;

            case CLASS:
                isValid = cfVersion >= CFVersion.LDC_CLASS;
                break;

            case METHOD_HANDLE:
            case METHOD_TYPE:
                isValid = cfVersion >= CFVersion.JSR292;
                break;

            default:
                isValid = false;
                break;
        }
        verifyThat(isValid, "Bad constant pool tag %s for ldc", tag);
    }

    private void verifyLdc2CPIndex(int index) {
        final Tag tag = verifyIndexAndGetTag(index);
        verifyThat(tag == Tag.LONG || tag == Tag.DOUBLE,
                "Bad constant pool tag %s for ldc2", tag);
    }

    private void verifyFieldCPIndex(int index) {
        final Tag tag = verifyIndexAndGetTag(index);
        verifyThat(tag == Tag.FIELDREF, "Bad constant pool tag %s for field operation", tag);
    }

    private void verifyInvokeCPIndex(int index, Bytecode op) {
        final Tag tag = verifyIndexAndGetTag(index);
        final boolean ok;
        switch (op) {
            case INVOKEVIRTUAL:
                ok = tag == Tag.METHODREF;
                break;

            case INVOKESTATIC:
            case INVOKESPECIAL:
                ok = tag == Tag.METHODREF ||
                        (cfVersion >= CFVersion.INTERFACE_METHODS && tag == Tag.INTERFACE_METHODREF);
                break;

            case INVOKEINTERFACE:
                ok = tag == Tag.INTERFACE_METHODREF;
                break;

            case INVOKEDYNAMIC:
                ok = cfVersion >= CFVersion.JSR292 && tag == Tag.INVOKE_DYNAMIC;
                break;

            default:
                CodeHelpers.shouldNotReachHere(String.valueOf(op));
                return;
        }
        verifyThat(ok, "Bad constant pool tag %s for %s", tag, op);

        final XString name = cp.getRefName(index);
        if (name.charAt(0) == (byte)'<') {
            verifyThat(name.equals(XString.apply("<init>")), "Method %s cannot be called", name);
            verifyThat(op == Bytecode.INVOKESPECIAL, "Only invokespecial is allowed to invoke an instance initialization method");
        }
    }

    private void verifyInvokeInterfaceParamCount(int index, int count) {
        final int paramSlots = countParamSlots(false, cp.getRefSignatureTraverser(index));
        verifyThat(paramSlots == count,
                "count operand of invoke interface is not consistent with referenced method arguments count");
    }

    private void verifyClassCPIndex(int index, Object purpose) {
        final Tag tag = verifyIndexAndGetTag(index);
        verifyThat(tag == Tag.CLASS, "Bad constant pool tag %s for %s", tag, purpose);
    }

    private int getCPArrayDimensions(int classIndex) {
        final XString klass = cp.getClassNameValue(classIndex);
        int i = 0;
        while (i < klass.length() && klass.charAt(i) == (byte)'[') {
            i++;
        }
        return i;
    }

    private void verifyCatchTypeIndex(int catchTypeIndex) {
        if (catchTypeIndex != 0) {
            verifyClassCPIndex(catchTypeIndex, "catch");
            final VerificationType catchType = types.classOf(cp.getClassNameValue(catchTypeIndex));
            verifyThat(types.baseThrowableType().isAssignableFrom(catchType, verificationContext()),
                    "Catch type %s is not throwable", catchType);
        }
    }

    /////////////////////////////////////
    // Switches stuff:

    private void verifyTableSwitch(BytecodeIterator bc) {
        verifySwitchPadding(bc);

        final int lowMatch = bc.param(1);
        final int highMatch = bc.param(2);
        verifyThat(lowMatch <= highMatch, "Unsorted match values in tableswitch");
        verifyThat(0 <= highMatch - lowMatch, "Too many match values in tableswitch");

        markSwitchTargets(bc);
    }

    private void verifyLookupSwitch(BytecodeIterator bc) {
        verifySwitchPadding(bc);

        final int nPairs = bc.param(1);
        verifyThat(nPairs >= 0, "Invalid number of match values in lookupswitch");

        final int[] matches = bc.getSwitchMatches();
        boolean sorted = true;
        for (int i = 1; i < matches.length; i++) {
            if (matches[i-1] >= matches[i]) {
                sorted = false;
                break;
            }
        }
        verifyThat(sorted, "Unsorted match values in lookupswitch");

        markSwitchTargets(bc);
    }

    private void verifySwitchPadding(BytecodeIterator bc) {
        verifyThat(bc.isSwitchPaddingZero(), "Non-zero switch padding");
    }

    private void markSwitchTargets(BytecodeIterator bc) {
        final int defaultBC = bc.param(0);
        markTarget(defaultBC);

        for (int bcTarget : bc.getSwitchTargets()) {
            markTarget(bcTarget);
        }
    }

    /////////////////////////////////////
    // Other verification stuff:

    private void verifyLocal(BytecodeTypeKind typeKind, int localIdx) {
        verifyThat(0 <= localIdx && localIdx + typeKind.nslots() - 1 < codeAttr.maxLocals(), "Invalid local %d", localIdx);
    }

}
