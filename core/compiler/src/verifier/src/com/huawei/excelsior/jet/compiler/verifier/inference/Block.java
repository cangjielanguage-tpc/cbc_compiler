/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier.inference;

import com.huawei.excelsior.jet.compiler.bytecode.parsing.simple.SimpleBlock;
import com.huawei.excelsior.jet.compiler.verifier.inference.TypeInferenceVerifier.TypeInferenceMethodVerifier.State;

final class Block extends SimpleBlock<Block> {

    // For debug purposes only.
    private int id = -1;

    private boolean fallOfTheBottom;

    private State verificationState;

    public Block(int startBC) {
        super(startBC);
    }

    public Block(int startBC, int endBC) {
        super(startBC, endBC);
    }

    @Override
    public Block newTailOfSplit(int splitBC) {
        return new Block(splitBC);
    }

    @Override
    public void connectTo(Block that) {
        assert !this.fallOfTheBottom;
        super.connectTo(that);
    }

    @Override
    public Block split(int bc) {
        final boolean wasFallOfTheBottom = this.fallOfTheBottom;
        if (wasFallOfTheBottom) {
            this.fallOfTheBottom = false;
        }
        final Block tail = super.split(bc);
        if (wasFallOfTheBottom) {
            tail.fallOfTheBottom = true;
        }
        return tail;
    }

    @Override
    public void connectJsrRetToRealTarget(Block targetBlock) {
        fallOfTheBottom = false;
        super.connectJsrRetToRealTarget(targetBlock);
    }

    public int id() {
        return id;
    }

    public Block withId(int id) {
        assert this.id == -1 && id != -1;
        this.id = id;
        return this;
    }

    public boolean isFallOfTheBottom() {
        return fallOfTheBottom;
    }

    public void markAsHalted() {
        this.fallOfTheBottom = true;
    }

    public State getVerificationState() {
        return verificationState;
    }

    public void setVerificationState(State verificationState) {
        this.verificationState = verificationState;
    }

    @Override
    public String toString() {
        return "#" + id + " [" + startBC() + "," + _endBC() + ")";
    }
}
