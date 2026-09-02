/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.bytecode

/** Kind of bytecode operation
  *
  * @author paul
  */
enum OpKind {
  case CONST      // push immediate const
  case LOAD       // load local to operand stack
  case STORE      // store top of operand stack into local
  case ARITH      // arithmetic or logical instruction, see ArithOp
  case CONVERT    // value conversion operation
  case STACK      // stack manipulation instruction
  case ARRAYGET   // get element from array
  case ARRAYPUT   // put element to array
  case XRETURN    // return from current method
  case UNARY_IF   // if with one argument
  case BINARY_IF  // if with two arguments
  case CONTROL    // other control instructions: GOTOxx, JSRxx, RET, ATHROW, xxSWITCH
  case RESERVED   //
  case OTHER      // all other instructions: LDCxx, IINC,
                  //     + get/put static/instance fields, invoke methods
                  //     + misc. object operations: newxx, instanceof, cast, arraylength, monitors
}
