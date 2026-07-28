/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier;

import com.huawei.excelsior.jet.compiler.util.ListHelpers;
import com.huawei.excelsior.jet.compiler.verifier.VerificationTypes.VerificationType;

import scala.collection.immutable.List;

/**
 * Verification state of a verifier.
 *
 * @author kit
 * @author cypok
 */
public class StackMapFrame {

    protected VerificationType[] locals;
    private boolean privateLocals;

    public List<VerificationType> stack;
    public int stackHeight;

    public boolean uninitializedThis;

    public StackMapFrame(VerificationType[] locals, List<VerificationType> stack, int stackHeight, boolean uninitializedThis) {
        this.locals = locals;
        this.privateLocals = false;
        this.stack = stack;
        this.stackHeight = stackHeight;
        this.uninitializedThis = uninitializedThis;
    }

    public VerificationType[] borrowLocals() {
        privateLocals = false;
        return locals;
    }

    /**
     * Returned arrays should never be modified by caller and may be modified by state in future.
     */
    public VerificationType[] getLocalsUnsafe() {
        return locals;
    }

    public VerificationType readLocal(int localIdx) {
        return locals[localIdx];
    }

    public boolean writeLocal(int localIdx, VerificationType type) {
        if (locals[localIdx].equals(type)) {
            return false;
        }

        if (!privateLocals) {
            // Equivalent to Arrays.copyOf(locals, locals.length), but faster.
            final VerificationType[] copiedLocals = new VerificationType[locals.length];
            System.arraycopy(locals, 0, copiedLocals, 0, locals.length);
            locals = copiedLocals;

            privateLocals = true;
        }
        locals[localIdx] = type;
        return true;
    }

    private void verifyAssignCompatible(VerificationUnit verification, VerificationType to, VerificationType from) {
        verification.verifyThat(to.isAssignableFrom(from, verification.verificationContext()), "%s is not assignable from %s", to, from);
    }

    public final void verifyAssignableFrom(VerificationUnit verification, StackMapFrame that) {
        for (int i = 0; i < locals.length; i++ ) {
            verifyAssignCompatible(verification, this.locals[i], that.locals[i]);
        }

        verification.verifyThat(this.stackHeight == that.stackHeight,
                "Stack height mismatch: %d != %d", this.stackHeight, that.stackHeight);
        verifyStacks(verification, this.stack, that.stack);

        verification.verifyThat(this.uninitializedThis == that.uninitializedThis, "Stack map frame flags mismatch");
    }

    private void verifyStacks(VerificationUnit verification, List<VerificationType> toStack, List<VerificationType> fromStack) {
        if (toStack.isEmpty()) {
            return;
        }
        verifyAssignCompatible(verification, toStack.head(), fromStack.head());
        verifyStacks(verification, ListHelpers.tail(toStack), ListHelpers.tail(fromStack));
    }

    public void copyFrom(StackMapFrame that) {
        for (int i = 0; i < locals.length; i++ ) {
            this.writeLocal(i, that.locals[i]);
        }
        this.stack = that.stack;
        this.stackHeight = that.stackHeight;
        this.uninitializedThis = that.uninitializedThis;
    }
}
