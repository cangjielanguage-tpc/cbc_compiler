/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier.inference;

import com.huawei.excelsior.jet.compiler.Environment;
import com.huawei.excelsior.jet.compiler.bytecode.parsing.CompleteControlFlowParser;
import com.huawei.excelsior.jet.compiler.verifier.VerifiableMethod;

import scala.reflect.ClassTag;

class ControlFlowParser extends CompleteControlFlowParser<Block> {

    // This class has common parts with BytecodeGenerator.StructuredLockingAnalyzer and it's hard to fix this.
    // I've done my best.
    // -- cypok

    private int nextBlockId = 0;

    public ControlFlowParser(Environment env, VerifiableMethod method, boolean verify) {
        super(env, method, verify, ClassTag.apply(Block.class));
    }

    @Override
    public void setBlockBCRange(Block block, int start, int end) {
        block.setBlockBCRange(start, end);
    }

    @Override
    public int blockStartPC(Block block) {
        return block.startBC();
    }

    @Override
    public int blockEndPC(Block block) {
        return block.endBC();
    }

    @Override
    public scala.collection.Iterator<Block> succBlocks(Block block) {
        return block.outputs();
    }

    @Override
    public Block createBlock(int bc) {
        final int nonNegativeBC = (bc != NO_BYTECODE_POSITION()) ? bc : codeAttr().bytecodeLength();
        return new Block(nonNegativeBC).withId(nextBlockId++);
    }

    @Override
    public Block cloneBlock(Block block){
        return new Block(block.startBC(), block.endBC()).withId(nextBlockId++);
    }

    @Override
    public final void connectClonedBlockToClonedTargets(Block block, scala.collection.Iterator<Block> targetBlocks) {
        block.connectClonedToClonedTargets(targetBlocks);
    }

    @Override
    public void connectJsrRetBlockToRealTarget(Block block, Block targetBlock) {
        block.connectJsrRetToRealTarget(targetBlock);
    }

    @Override
    public Block splitBlock(int bc, Block block) {
        return block.split(bc).withId(nextBlockId++);
    }

    @Override
    public void addReturn(int bc, Block block) {
    }

    @Override
    public void addThrow(int bc, Block block) {
    }

    @Override
    public void addHalt(int bc, Block block) {
        block.markAsHalted();
    }

    @Override
    public void addJump(int bc, Block block, Block targetBlock) {
        block.connectTo(targetBlock);
    }

    @Override
    public void addIf(int bc, Block block, Block falseTarget, Block trueTarget) {
        block.connectTo(falseTarget, trueTarget);
    }

    // Array[B] type in Scala gets erased to Object in Java code.
    // So to satisfy IDEA, we have dummy methods with `Block[] targetBlocks` parameters.
    // To satisfy javac, we have proper methods with `Object targetBlocks` parameters.

    public void addTableSwitch(int bc, Block block, int lowMatch, int highMatch, Block[] targetBlocks, Block defaultBlock) { }
    public void addLookupSwitch(int bc, Block block, int[] matches, Block[] targetBlocks, Block defaultBlock) { }

    @SuppressWarnings("unused")
    public void addTableSwitch(int bc, Block block, int lowMatch, int highMatch, Object /* Block[] */ targetBlocks, Block defaultBlock) {
        addSwitch(block, (Block[]) targetBlocks, defaultBlock);
    }

    @SuppressWarnings("unused")
    public void addLookupSwitch(int bc, Block block, int[] matches, Object /* Block[] */ targetBlocks, Block defaultBlock) {
        addSwitch(block, (Block[]) targetBlocks, defaultBlock);
    }

    @Override
    public void addMonitorOp(int bc, Block block, boolean isEnter) {
    }

    private void addSwitch(Block block, Block[] targetBlocks, Block defaultBlock) {
        block.connectTo(defaultBlock, targetBlocks);
    }
}
