/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.cangjie.interop.java

import xscala.util.Feature
import com.huawei.excelsior.jet.compiler.cangjie.interop.java.JavaSymbols.{Class, Method}

/** An abstraction of processor of Cangjie classes annotated by `@java` macro. */
abstract class JavaAnnotatedClassProcessor {
  /** Process classes annotated by `@java` macro.
    *
    * May only be called if Cangjie-Java interop is supported.
    *
    * @param javaClass           representation of `@java`-class in Java world
    * @param javaHelperName      name of Cangjie (helper) class for `@java`-class
    * @param delegateConstructor delegate (this- or super-) constructor
    */
    def process(symbols: JavaSymbols, javaClass: Class, javaHelperName: String, sourceFile: String,
                delegateConstructor: Method => Option[Method]): Unit
}

object JavaAnnotatedClassProcessor extends Feature[JavaAnnotatedClassProcessor]
