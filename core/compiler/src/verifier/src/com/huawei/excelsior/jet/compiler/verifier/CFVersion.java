/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.verifier;

class CFVersion {
    private static final int JAVA_5 = 49;
    private static final int JAVA_6 = 50;
    private static final int JAVA_7 = 51;
    private static final int JAVA_8 = 52;

    public static final int LDC_CLASS = JAVA_5;

    public static final int MIN_SPLIT_VERIFIER = JAVA_6;
    public static final int MAX_INFERENCE_VERIFIER = JAVA_6;

    public static final int NO_JSR = JAVA_7;
    public static final int JSR292 = JAVA_7;

    public static final int INTERFACE_METHODS = JAVA_8;

}
