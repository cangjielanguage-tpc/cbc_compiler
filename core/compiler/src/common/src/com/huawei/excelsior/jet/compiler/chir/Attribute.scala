/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.chir

enum Attribute {
  case STATIC                   // Mark whether a member is a static one.
  case PUBLIC                   // Mark whether a member is a public one.
  case PRIVATE                  // Mark whether a member is a private one.
  case PROTECTED                // Mark whether a member is a protected one.

  case ABSTRACT                 // Mark whether a function is an abstract one.
  case VIRTUAL                  // Mark whether a declaration is in fact open (even if the user does not use `open` keyword).

  case OVERRIDE                 // Mark whether a declaration in fact overrides the inherited one (even if the user does not use `override` keyword).

  case REDEF                    // Mark whether a declaration in fact overrides the inherited one (even if the user does not use `redef` keyword).

  case SEALED                   // Mark whether a declaration is a sealed one.
  case FOREIGN                  // Mark whether a declaration is a foreign one.

  case MUT                      // Mark whether a declaration is a mutable one.
  case FINAL                    // Mark a func override a parent class's func, and this func self does not have VIRTUAL Attribute.
  case OPERATOR                 // Mark whether a declaration is a operator one.
  case READONLY                 // 'let x = xxx', 'x' enable READONLY attribute
  case CONST                    // correspond `const` keyword in Cangjie source code.
  case IMPORTED                 // Mark whether variable、func、enum、struct、class is imported from other package.
  case GENERIC_INSTANTIATED     // Mark whether a `GlobalVar/Function/Type` is instantiated.
  case NO_DEBUG_INFO            // Mark a `Value` doesn't contain debug info, like line/column number.
  case GENERIC                  // Mark a declaration is generic
  case INTERNAL                 // GlobalVar/Function/Enum/Class/Struct/Interface is visible in current and sub package.
  case COMPILER_ADD             // Mark a `Value` is added by compiler, like "copied default func from interface".

  // compiler attribute
  case NO_REFLECT_INFO          // Mark a `Value` is't used by `reflect` feature.
  case NO_INLINE                // Mark a Function can't be inlined.
  case NON_RECOMPILE            // only used in imported global var/func in incremental compilation, indicate this value is converted from a decl in current package that is not recompiled.
  case UNREACHABLE              // Mark a Block is unreachable.
  case NO_SIDE_EFFECT           // Mark a Function does't have side effect.
  case COMMON                   // Mark whether it's common declaration.
  case SPECIFIC                 // Mark whether it's specific declaration.
  case SKIP_ANALYSIS            // Mark node that is not used for analysis e.g. Node can be skiped if it has no body when creating 'common part'
  case DESERIALIZED             // Node deserialized from .chir file
  case INITIALIZER              // Mark nodes that related to initialization process. 
                                // Marked functions are package initializer, file initializers, variable initializer or so.
                                // On the block is used to search for it among other blocks of the function.
  case UNSAFE                   // Mark whether a function that was marked as `unsafe`
  // Native FFI attributes
  case JAVA_MIRROR              // Mark whether it's @JavaMirror declaration (binding for a java type).
  case JAVA_IMPL                // Mark whether it's @JavaImpl declaration.
  case OBJ_C_MIRROR             // Mark whether it's @ObjCMirror declaration (binding for an Objective-C type).
  case HAS_INITED_FIELD         // Mark whether a node is a special flag, which marks the class instance as initialized.
  case JAVA_HAS_DEFAULT         // Mark whether JAVA_MIRROR interface has default method.
  case PREVIOUSLY_DESERIALIZED  // Mark that deserialization occurs not in the newly created node, but in an existing one.

  case ATTR_END


  def in(attrs: Long): Boolean = {
    (attrs & (1L << ordinal)) != 0L
  }

  def name: String = this match {
    case READONLY => "readOnly"
    case CONST => "compileTimeVal"
    case COMPILER_ADD => "compilerAdd"
    case NON_RECOMPILE => "nonRecompile"
    case NO_REFLECT_INFO => "noReflectInfo"
    case NO_DEBUG_INFO => "noDebugInfo"
    case NO_INLINE => "noInline"
    case NO_SIDE_EFFECT => "noSideEffect"
    case JAVA_MIRROR => "javaMirror"
    case JAVA_IMPL => "javaImpl"
    case _ => toString.toLowerCase
  }

}