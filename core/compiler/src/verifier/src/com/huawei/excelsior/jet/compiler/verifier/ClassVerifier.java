/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier;

import static com.huawei.excelsior.common.CodeHelpers.shouldNotReachHere;

import com.huawei.excelsior.jet.classfile.SignatureParser;
import com.huawei.excelsior.jet.classfile.SignatureTraverser;
import com.huawei.excelsior.jet.common.XString;
import com.huawei.excelsior.jet.compiler.Environment;
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp;
import com.huawei.excelsior.jet.compiler.bytecode.ArithOp$;
import com.huawei.excelsior.jet.compiler.bytecode.Bytecode;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeIterator;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeProcessor;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind;
import com.huawei.excelsior.jet.compiler.bytecode.BytecodeTypeKind$;
import com.huawei.excelsior.jet.compiler.bytecode.CompareOp;
import com.huawei.excelsior.jet.compiler.bytecode.ConstantPool;
import com.huawei.excelsior.jet.compiler.bytecode.ConvertOp;
import com.huawei.excelsior.jet.compiler.bytecode.FieldAccessKind;
import com.huawei.excelsior.jet.compiler.bytecode.MethodAccessKind;
import com.huawei.excelsior.jet.compiler.bytecode.MethodCodeAttribute;
import com.huawei.excelsior.jet.compiler.bytecode.Tag;
import com.huawei.excelsior.jet.compiler.util.ListHelpers;
import com.huawei.excelsior.jet.compiler.verifier.AbstractVerifier.InfoBuilder;
import com.huawei.excelsior.jet.compiler.verifier.VerificationTypes.VerificationType;

import scala.collection.IndexedSeq;
import scala.collection.immutable.List;
import scala.collection.mutable.HashMap;

/**
 * Performs verification of a class:
 * initializes class-specific data structures and verifies every method.
 *
 * @author kit
 * @author cypok
 */
public abstract class ClassVerifier {

    protected final Environment env;
    protected final int cfVersion;

    protected final ConstantPool cp;
    protected final VerificationTypes types;

    protected final VerifiableType thisClass;
    protected final VerificationType thisType;

    private final HashMap<Integer, Boolean> protectedAccessCache = new HashMap<>();

    public ClassVerifier(Environment env, InfoBuilder builder, int cfVersion, VerifiableType thisClass) {
        this.env = env;
        this.thisClass = thisClass;
        this.cfVersion = cfVersion;

        cp = thisClass.getClassConstantPool();
        types = new VerificationTypes(env.getTypeProvider(), builder, thisClass);
        thisType = types.thisType(false);
    }

    /**
     * Performs verification of the class and returns {@link VerificationError} if verification fails.
     * Otherwise, returns {@code null}.
     */
    public final VerificationError verify() {
        try {
            thisClass.getDeclaredMethods().foreach((VerifiableMethod method) -> {
                if (method.canBeVerified()) {
                    createMethodVerifier(method).verify();
                }
                return null;
            });
        } catch (VerificationError err) {
            return err;
        }
        return null;
    }

    protected abstract MethodVerifier createMethodVerifier(VerifiableMethod method);

    /**
     * Performs verification of a method:
     * verifies bytecode and some method's attributes (e.g. StackMapTable).
     */
    public abstract class MethodVerifier extends VerificationUnit {

        protected final VerifiableMethod method;
        protected final MethodCodeAttribute codeAttr;

        protected final int paramsNum;
        protected final int paramSlotsNum;
        protected final VerificationType[] entryLocalTypes;
        protected final VerificationType returnType;
        protected final boolean entryUninitializedThis;

        protected MethodVerifier(VerifiableMethod method) {
            super(true, method);
            this.method = method;

            codeAttr = method.codeAttribute();

            entryLocalTypes = new VerificationType[codeAttr.maxLocals()];

            entryUninitializedThis = method.isConstructor() && !thisClass.isHierarchyRoot();

            int slot = 0;
            int param = 0;
            if (!method.isStatic()) {
                param++;
                entryLocalTypes[slot++] = types.thisType(entryUninitializedThis);
            }
            final SignatureParser<VerificationType> iter = types.parseSignature(SignatureTraverser.fromString(method.getXSignature()));
            VerificationType type = iter.next();
            while (iter.hasNext()) {
                param++;
                entryLocalTypes[slot++] = type;
                if (type.is2Slots()) {
                    entryLocalTypes[slot++] = type.get2ndHalf();
                }
                type = iter.next();
            }
            returnType = type;
            paramsNum = param;
            paramSlotsNum = slot;

            if (paramSlotsNum < entryLocalTypes.length) {
                // TODO-DECAF: provide own `Arrays.fill` implementation and use it here
                final VerificationType val = types.TOP();
                for (int i = paramSlotsNum, length = entryLocalTypes.length; i < length; i++)
                    entryLocalTypes[i] = val;
            }
        }

        public abstract void verify();

        /**
         * Performs verification of a bytecode instruction.
         * Takes given input state and mutates it.
         */
        public class BytecodeVerifier implements BytecodeProcessor {

            protected final StackMapFrame state;

            protected int curBC;

            public BytecodeVerifier(StackMapFrame state) {
                this.state = state;
            }

            private void verifyAssignCompatible(VerificationType to, VerificationType from) {
                verifyThat(to.isAssignableFrom(from, verificationContext()), "%s is not assignable from %s", to, from);
            }

            ////////////////////////
            // types forwarders

            private VerificationType classAt(int classIndex) {
                return types.classOf(cp.getClassNameValue(classIndex));
            }

            private VerificationType primitive(BytecodeTypeKind tkind) {
                return types.primitive(tkind, 0);
            }

            private VerificationType primitiveArray(BytecodeTypeKind tkind) {
                return types.primitive(tkind, 1);
            }

            private VerificationType primitiveOrNull(BytecodeTypeKind tkind) {
                return tkind.isPrimitive() ? primitive(tkind) : types.NULL();
            }

            private VerificationType primitiveOrObject(BytecodeTypeKind tkind) {
                return tkind.isPrimitive() ? primitive(tkind) : types.OBJECT();
            }

            private VerificationType primitiveOrReference(BytecodeTypeKind tkind) {
                return tkind.isPrimitive() ? primitive(tkind) : types.REFERENCE();
            }

            private VerificationType primitiveOrReferenceOrReturnAddress(BytecodeTypeKind tkind) {
                return tkind.isPrimitive() ? primitive(tkind) : types.REFERENCE_OR_RETURN_ADDRESS();
            }

            private VerificationType primitiveOrObjectArray(BytecodeTypeKind tkind) {
                if (tkind.isPrimitive()) {
                    return primitiveArray(tkind);
                } else {
                    return types.array(types.OBJECT(), 1);
                }
            }

            // types forwarders
            ////////////////////////

            ///////////////////////////
            // locals & stack basic operations

            // Note how 2-slot types are stored in state.
            //
            //   Long on local #3:
            //     [..., long, half, ...]
            //            #3    #4
            //
            //   Long on top of the stack:
            //     [..., half, long]
            //
            // Also note that 2-slot type in locals may be "broken" and its consistency must be checked,
            // however such type on stack is undivided and its consistency is asserted.
            //
            // Stack layout differs to JVM specification (Chapter "Verification by Type Checking")
            // to simplify checking in "pop": it is enough to type check only top of the stack.

            private void push(VerificationType value) {
                if (value.is2Slots()) {
                    pushRaw(value.get2ndHalf());
                }
                pushRaw(value);
            }

            private VerificationType pop(VerificationType expectedType) {
                final VerificationType type = popRaw();
                verifyAssignCompatible(expectedType, type);
                if (type.is2Slots()) {
                    final VerificationType half = popRaw();
                    assert half.isHalf();
                }
                return type;
            }

            private void pushRaw(VerificationType value) {
                verifyThat(state.stackHeight < codeAttr.maxStack(), "Stack overflow");
                state.stack = ListHelpers.prepended(value, state.stack);
                state.stackHeight++;
            }

            private VerificationType popRaw() {
                verifyThat(state.stackHeight > 0, "Pop from empty stack");
                final VerificationType head = state.stack.head();
                state.stack = ListHelpers.tail(state.stack);
                state.stackHeight--;
                return head;
            }

            private void write(int localIdx, VerificationType type) {
                writeRaw(localIdx, type);
                if (type.is2Slots()) {
                    writeRaw(localIdx + 1, type.get2ndHalf());
                }
            }

            private VerificationType read(VerificationType expectedType, int localIdx) {
                final VerificationType type = readRaw(localIdx);
                verifyAssignCompatible(expectedType, type);
                if (expectedType.is2Slots()) {
                    final VerificationType half = readRaw(localIdx + 1);
                    verifyAssignCompatible(expectedType.get2ndHalf(), half);
                }
                return type;
            }

            protected void writeRaw(int localIdx, VerificationType value) {
                state.writeLocal(localIdx, value);
            }

            private VerificationType readRaw(int localIdx) {
                return state.readLocal(localIdx);
            }

            // locals & stack basic operations
            ///////////////////////////

            ///////////////////////////
            // uninitialized support

            private boolean thisOrDirectSuperOfRefClass(XString refClassName) {
                if (thisClass.getXName().equals(refClassName)) {
                    return true;
                }
                final VerifiableType superClass = thisClass.getSuperClass();
                return (superClass != null) && superClass.getXName().equals(refClassName);
            }

            protected void initUninitializedInLocals(VerificationType uninitialized, VerificationType initialized) {
                for (int i = 0; i < codeAttr.maxLocals(); i++) {
                    if (state.readLocal(i).equals(uninitialized)) {
                        state.writeLocal(i, initialized);
                    }
                }
            }

            private List<VerificationType> initUninitializedOnStack(VerificationType uninitialized, VerificationType initialized, List<VerificationType> stack) {
                if (stack.isEmpty()) {
                    return stack;
                }

                final List<VerificationType> tail = ListHelpers.tail(stack);
                final List<VerificationType> mergedTail = initUninitializedOnStack(uninitialized, initialized, tail);
                final boolean tailChanged = tail != mergedTail;

                final VerificationType head = stack.head();
                final boolean headChanged = head.equals(uninitialized);
                final VerificationType mergedHead = headChanged ? initialized : head;

                if (headChanged || tailChanged) {
                    return ListHelpers.prepended(mergedHead, mergedTail);
                } else {
                    return stack;
                }
            }

            private XString decodeNewClassName(int instrPC) {
                // We only decode class name, verification that it's correct NEW instruction should be done earlier.
                final BytecodeIterator bc = new BytecodeIterator(codeAttr);
                bc.reset(instrPC, codeAttr.bytecodeLength());
                final Bytecode instr = bc.next();
                assert instr == Bytecode.NEW;
                return cp.getClassNameValue(bc.param());
            }

            private VerificationType initUninitialized(VerificationType uninitialized, XString refClassName) {
                final VerificationType initialized;
                if (uninitialized.isUninitializedThis()) {
                    verifyThat(thisOrDirectSuperOfRefClass(refClassName), "Call of not this or super constructor");
                    initialized = thisType;
                } else {
                    verifyThat(uninitialized.isUninitializedNew(), "Constructor call from not uninitialized value");
                    final XString newClassName = decodeNewClassName(uninitialized.getUninitializedNewOffset());
                    verifyThat(newClassName.equals(refClassName), "New value is initialized with wrong constructor");
                    initialized = types.classOf(newClassName);
                }

                initUninitializedInLocals(uninitialized, initialized);
                state.stack = initUninitializedOnStack(uninitialized, initialized, state.stack);

                if (uninitialized.isUninitializedThis()) {
                    assert state.uninitializedThis;
                    state.uninitializedThis = false;
                }

                return initialized;
            }

            // uninitialized support
            ///////////////////////////

            @Override
            public void startInstruction(int offset, int nextOffset) {
                curBC = offset;
            }

            @Override
            public final void pushLocal(BytecodeTypeKind tkind, int index) {
                final VerificationType type = read(primitiveOrReference(tkind), index);
                push(type);
            }

            @Override
            public final void storeLocal(BytecodeTypeKind tkind, int index) {
                final VerificationType type = pop(primitiveOrReferenceOrReturnAddress(tkind));
                write(index, type);
            }

            private boolean isValidSingle(VerificationType x) {
                return !x.is2SlotsOrHalf();
            }

            private boolean isValidSingles(VerificationType x, VerificationType y) {
                return isValidSingle(x) && isValidSingle(y);
            }

            private boolean isValidLong(VerificationType x, VerificationType y) {
                return x.is2Slots() && y.equals(x.get2ndHalf());
            }

            private boolean isValidPair(VerificationType x, VerificationType y) {
                return isValidSingles(x, y) || isValidLong(x, y);
            }

            @Override
            public final void stackOp(Bytecode op) {
                switch (op) {
                    case POP:
                        // ..., x => ...
                        verifyThat(isValidSingle(popRaw()), "Illegal pop");
                        break;
                    case POP2: {
                        // ..., y, x => ...
                        final VerificationType x = popRaw();
                        final VerificationType y = popRaw();
                        verifyThat(isValidPair(x, y), "Illegal pop2");
                        break;
                    }
                    case DUP: {
                        // ..., x => ..., x, x
                        final VerificationType x = popRaw();
                        verifyThat(isValidSingle(x), "Illegal dup");
                        pushRaw(x);
                        pushRaw(x);
                        break;
                    }
                    case DUP_X1: {
                        // ..., y, x => ..., x, y, x
                        final VerificationType x = popRaw();
                        final VerificationType y = popRaw();
                        verifyThat(isValidSingles(x, y), "Illegal dup_x1");
                        pushRaw(x);
                        pushRaw(y);
                        pushRaw(x);
                        break;
                    }
                    case DUP_X2: {
                        // ..., z, y, x => ..., x, z, y, x
                        final VerificationType x = popRaw();
                        final VerificationType y = popRaw();
                        final VerificationType z = popRaw();
                        verifyThat(isValidSingle(x) && isValidPair(y, z), "Illegal dup_x2");
                        pushRaw(x);
                        pushRaw(z);
                        pushRaw(y);
                        pushRaw(x);
                        break;
                    }
                    case DUP2: {
                        // ..., y, x => ..., y, x, y, x
                        final VerificationType x = popRaw();
                        final VerificationType y = popRaw();
                        verifyThat(isValidPair(x, y), "Illegal dup2");
                        pushRaw(y);
                        pushRaw(x);
                        pushRaw(y);
                        pushRaw(x);
                        break;
                    }
                    case DUP2_X1: {
                        // ..., z, y, x => ..., y, x, z, y, x
                        final VerificationType x = popRaw();
                        final VerificationType y = popRaw();
                        final VerificationType z = popRaw();
                        verifyThat(isValidSingle(z), "Illegal dup2_x1");
                        assert isValidPair(x, y);
                        pushRaw(y);
                        pushRaw(x);
                        pushRaw(z);
                        pushRaw(y);
                        pushRaw(x);
                        break;
                    }
                    case DUP2_X2: {
                        // ..., w, z, y, x => ..., y, x, w, z, y, x
                        final VerificationType x = popRaw();
                        final VerificationType y = popRaw();
                        final VerificationType z = popRaw();
                        final VerificationType w = popRaw();
                        verifyThat(isValidPair(x, y) && isValidPair(z, w), "Illegal dup2_x2");
                        pushRaw(y);
                        pushRaw(x);
                        pushRaw(w);
                        pushRaw(z);
                        pushRaw(y);
                        pushRaw(x);
                        break;
                    }

                    case SWAP: {
                        // ..., y, x => ..., x, y
                        final VerificationType x = popRaw();
                        final VerificationType y = popRaw();
                        verifyThat(isValidSingles(x, y), "Illegal swap");
                        pushRaw(x);
                        pushRaw(y);
                        break;
                    }

                    default:
                        shouldNotReachHere(op.toString());
                }
            }

            @Override
            public void pushCPEntry(int index) {
                final Tag tag = cp.getTag(index);
                switch (tag) {
                    case INTEGER:
                        push(types.INT());
                        break;
                    case FLOAT:
                        push(types.FLOAT());
                        break;
                    case LONG:
                        push(types.LONG());
                        break;
                    case DOUBLE:
                        push(types.DOUBLE());
                        break;
                    case CLASS:
                        push(types.CLASS());
                        break;
                    case STRING:
                        push(types.STRING());
                        break;
                    case METHOD_TYPE:
                        push(types.METHOD_TYPE());
                        break;
                    case METHOD_HANDLE:
                        push(types.METHOD_HANDLE());
                        break;

                    default:
                        shouldNotReachHere(tag.toString());
                        break;
                }
            }

            @Override
            public void pushConst(BytecodeTypeKind tkind, int value) {
                if (tkind == BytecodeTypeKind$.CLASS) {
                    assert value == 0;
                    push(types.NULL());
                } else {
                    push(primitive(tkind));
                }
            }

            @Override
            public void arithOp(BytecodeTypeKind tkind, ArithOp op) {
                if (op == ArithOp$.NEG) {
                    // only one arg
                } else if (op.isShift()) {
                    // second arg is a shift distance
                    pop(types.INT());
                } else {
                    pop(primitive(tkind));
                }
                pop(primitive(tkind));

                if (op.isCmp()) {
                    push(types.INT());
                } else {
                    push(primitive(tkind));
                }
            }

            @Override
            public void convert(ConvertOp op) {
                pop(primitive(op.srcKind()));
                push(primitive(op.dstKind()));
            }

            @Override
            public void increment(int local, int delta) {
                read(types.INT(), local);
            }

            private void verifyProtectedAccess(int memberIndex, VerificationType accessedType) {
                if (!accessedType.isClass()) {
                    return;
                }

                final boolean protectedAccess = protectedAccessCache.getOrElseUpdate(memberIndex, () -> {
                    final XString refClassName = cp.getClassNameValue(cp.getClassIndex(memberIndex));
                    final VerifiableType refClass = findSuperOfThisClass(refClassName);
                    if (refClass == null || refClass.isSamePackage(thisClass)) {
                        return false;
                    }

                    final XString name = cp.getRefName(memberIndex);
                    final XString sig = cp.getRefSignature(memberIndex);
                    switch (cp.getTag(memberIndex)) {
                        case FIELDREF:
                            return refClass.containsProtectedField(name, sig);
                        case METHODREF:
                            return refClass.containsProtectedMethod(name, sig);
                        default:
                            return shouldNotReachHere();
                    }
                });

                if (protectedAccess) {
                    verifyAssignCompatible(thisType, accessedType);
                }
            }

            @Override
            public void fieldOp(int index, FieldAccessKind akind) {
                final VerificationType valueType = types.parseSingle(cp.getRefSignatureTraverser(index));
                if (akind.isStatic()) {
                    if (akind.isWrite()) {
                        pop(valueType);
                    } else {
                        push(valueType);
                    }
                } else {
                    final VerificationType formalObjType = classAt(cp.getClassIndex(index));
                    final VerificationType objType;
                    if (akind.isWrite()) {
                        pop(valueType);

                        objType = popRaw();
                        if (objType.isUninitializedThis()) {
                            verifyThat(formalObjType.equals(thisType),
                                    "Only fields declared in this class may be assigned in constructor before initialization");
                        } else {
                            verifyAssignCompatible(formalObjType, objType);
                        }
                    } else {
                        objType = pop(formalObjType);
                        push(valueType);
                    }
                    verifyProtectedAccess(index, objType);
                }
            }

            private boolean isConstructor(int methodIndex) {
                final XString methodName = cp.getRefName(methodIndex);
                return methodName.equals(XString.apply("<init>"));
            }

            private VerifiableType findSuperOfThisClass(XString superClassName) {
                VerifiableType superClass = thisClass;
                while (superClass != null) {
                    if (superClass.getXName().equals(superClassName)) {
                        return superClass;
                    }
                    superClass = superClass.getSuperClass();
                }
                return null;
            }

            private boolean goodInvokeSpecialRefClass(VerifiableType thisOrHostClass, XString refClassName) {
                if (thisOrHostClass.getXName().equals(refClassName)) {
                    return true;
                }
                if (refClassName.equals(types.OBJECT_NAME())) {
                    return true;
                }
                if (thisOrHostClass.isClass()) {
                    if (findSuperOfThisClass(refClassName) != null) {
                        return true;
                    }
                }
                return thisOrHostClass.getDeclaredSuperInterfaces().exists(si -> si.getXName().equals(refClassName));
            }

            @Override
            public void invoke(int index, MethodAccessKind akind) {
                final IndexedSeq<VerificationType> params = types.parseSignature(cp.getRefSignatureTraverser(index)).toIndexedSeq();
                final VerificationType retType = params.last();

                for (int i = params.size() - 2; i >= 0; i--) {
                    pop(params.apply(i));
                }

                if (akind.hasObjectArg()) {
                    final XString refClassName = cp.getClassNameValue(cp.getClassIndex(index));
                    VerificationType rcvType = popRaw();
                    if (akind == MethodAccessKind.SPECIAL) {
                        if (isConstructor(index)) {
                            rcvType = initUninitialized(rcvType, refClassName);
                        } else {
                            if (!thisClass.isAnonymous()) {
                                // It may be moved to static constraints but by specification it is structural constraint and
                                // thus should not be checked in unreachable code.
                                verifyThat(goodInvokeSpecialRefClass(thisClass, refClassName), "Illegal class reference in invokespecial");

                                verifyAssignCompatible(thisType, rcvType);
                            } else {
                                //anonymous classes treated as a part of host class.
                                final VerifiableType hostClass = thisClass.getHostClass();
                                verifyThat(goodInvokeSpecialRefClass(hostClass, refClassName), "Illegal class reference in invokespecial");

                                verifyAssignCompatible(types.classOf(hostClass), rcvType);
                            }
                        }
                    } else {
                        verifyAssignCompatible(types.classOf(refClassName), rcvType);
                    }

                    if (akind == MethodAccessKind.SPECIAL || akind == MethodAccessKind.VIRTUAL) {
                        verifyProtectedAccess(index, rcvType);
                    }
                }

                if (!retType.equals(types.VOID())) {
                    push(retType);
                }
            }

            @Override
            public void monitorEnter() {
                pop(types.OBJECT());
            }

            @Override
            public void monitorExit() {
                pop(types.OBJECT());
            }

            @Override
            public void doNew(int index) {
                push(types.uninitialized(curBC));
            }

            @Override
            public void instanceOf(int index) {
                pop(types.OBJECT());
                push(types.INT());
            }

            @Override
            public void checkCast(int index) {
                pop(types.OBJECT());
                push(classAt(index));
            }

            @Override
            public void doThrow() {
                pop(types.baseThrowableType());
            }

            @Override
            public void newPrimitiveArray(BytecodeTypeKind btkind) {
                pop(types.INT());
                push(primitiveArray(btkind));
            }

            @Override
            public void newObjectArray(int index) {
                pop(types.INT());
                push(types.array(classAt(index), 1));
            }

            @Override
            public void newMultiObjectArray(int index, int dimNum) {
                for (int i = 0; i < dimNum; i++) {
                    pop(types.INT());
                }
                push(classAt(index));
            }

            @Override
            public void arrayGet(BytecodeTypeKind tkind) {
                pop(types.INT());
                final VerificationType arrayType = pop(primitiveOrObjectArray(tkind));
                final VerificationType arrayElement;
                if (arrayType.isNull()) {
                    arrayElement = primitiveOrNull(tkind);
                } else {
                    arrayElement = arrayType.getArrayElement();
                }
                push(arrayElement);
            }

            @Override
            public void arrayPut(BytecodeTypeKind tkind) {
                pop(primitiveOrObject(tkind));
                pop(types.INT());
                pop(primitiveOrObjectArray(tkind));
            }

            @Override
            public void arrayLength() {
                final VerificationType type = popRaw();
                verifyThat(type.isArrayOrNull(), "Array is expected on top of the stack");
                push(types.INT());
            }

            @Override
            public void unaryIf(BytecodeTypeKind tkind, CompareOp op, int bc) {
                pop(primitiveOrReference(tkind));
            }

            @Override
            public void binaryIf(BytecodeTypeKind tkind, CompareOp op, int bc) {
                pop(primitiveOrReference(tkind));
                pop(primitiveOrReference(tkind));
            }

            @Override
            public void jump(int bc) {
            }

            @Override
            public void doReturn(BytecodeTypeKind tkind, boolean isLastBytecode) {
                final VerificationType instructionType;
                if (!tkind.isVoid()) {
                    pop(returnType);
                    instructionType = primitiveOrObject(tkind);
                } else {
                    instructionType = types.VOID();
                }
                verifyThat(instructionType.isAssignableFrom(returnType, verificationContext()), "Illegal return instruction");

                verifyThat(!state.uninitializedThis, "Return before this initialization");
            }

            @Override
            public void tableSwitch(int bcDefault, int lowMatch, int highMatch, int[] bcTargets) {
                anySwitch(bcDefault, bcTargets);
            }

            @Override
            public void lookupSwitch(int bcDefault, int[] matches, int[] bcTargets) {
                anySwitch(bcDefault, bcTargets);
            }

            protected void anySwitch(int bcDefault, int[] bcTargets) {
                pop(types.INT());
            }

            @Override
            public void jsr(int bc) {
                // Specification of structural constraints says:
                //   There must never be an uninitialized class instance on the operand stack or in a
                //   local variable when a jsr or jsr_w instruction is executed.
                //
                // JCK 8b does not check this.
                // J9 completely ignores this.
                // HotSpot ignores uninitialized this and replaces other uninitialized by top type (without errors!).
                //
                // So... JET completely ignores this.
                // Because it's better not to crash if application works on reference implementation.
                // However it would be very easy to do full check.
                push(types.RETURN_ADDRESS());
            }

            @Override
            public void ret(int var) {
                read(types.RETURN_ADDRESS(), var);
            }
        }
    }
}
