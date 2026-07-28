/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib

import com.huawei.excelsior.o2s.runtime.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*
import xscala.util.{Set32, UShort}

/*
    This module contains definitions of constants shared between
    RTS, compiler and linker.
*/
object xjRTSModule {
  type ClassCode = UShort
  val X2C_ClassCircularityError: ClassCode = UShort(8)
  val X2C_ClassFormatError: ClassCode = UShort(9)
  val X2C_UnsupportedClassVersionError: ClassCode = UShort(10)
  val X2C_IncompatibleClassChangeError: ClassCode = UShort(12)
  val X2C_IllegalAccessError: ClassCode = UShort(14)
  val X2C_NoClassDefFoundError: ClassCode = UShort(18)
  val X2C_VerifyError: ClassCode = UShort(20)

  /* 1-dim array */
  val mdf_public: Int = 0  //   +       +      +
  val mdf_private: Int = 1  //   +       +      + 
  val mdf_protected: Int = 2  //   +       +      + 
  val mdf_static: Int = 3  //   +       +      + 
  val mdf_final: Int = 4  //   +       +      + 
  val mdf_synchron: Int = 5  //   +       -      - 
  val mdf_volatile: Int = 6  //   +       +      - 
  val mdf_transient: Int = 7  //   +       +      - 
  val mdf_native: Int = 8  //   +       -      - 
  val mdf_interface: Int = 9  //   -       -      + 
  val mdf_abstract: Int = 10 //   +       -      + 
  val mdf_strictfp: Int = 11 //   +       -      - 
  val mdf_synthetic: Int = 12 //   +       +      + 
  val mdf_annotation: Int = 13 //   -       -      + 
  val mdf_enum: Int = 14 //   -       +      + 
  val mdf_bridge: Int = mdf_volatile
  val mdf_varargs: Int = mdf_transient
  val JMDF_METHOD_MASK: Set32 = Set32.of(mdf_public.toUByte, mdf_private.toUByte, mdf_protected.toUByte, mdf_static.toUByte, mdf_final.toUByte, mdf_synchron.toUByte, mdf_bridge.toUByte, mdf_varargs.toUByte, mdf_native.toUByte, mdf_abstract.toUByte, mdf_strictfp.toUByte, mdf_synthetic.toUByte)
  val JMDF_FIELD_MASK: Set32 = Set32.of(mdf_public.toUByte, mdf_private.toUByte, mdf_protected.toUByte, mdf_static.toUByte, mdf_final.toUByte, mdf_volatile.toUByte, mdf_transient.toUByte, mdf_synthetic.toUByte, mdf_enum.toUByte)
  val JMDF_TYPE_MASK: Set32 = Set32.of(mdf_public.toUByte, mdf_private.toUByte, mdf_protected.toUByte, mdf_static.toUByte, mdf_final.toUByte, mdf_interface.toUByte, mdf_abstract.toUByte, mdf_synthetic.toUByte, mdf_annotation.toUByte, mdf_enum.toUByte)
  val acc_super: Int = 5
  val ACC_FLAGS_MASK: Set32 = Set32.of(mdf_public.toUByte, mdf_final.toUByte, mdf_interface.toUByte, mdf_abstract.toUByte, mdf_synthetic.toUByte, mdf_annotation.toUByte, mdf_enum.toUByte, acc_super.toUByte)
  val mdf_deprecated: Int = 13 /** method/field is deprecated */
  val mdf_statini: Int = 8  /** need static initialization             --FOR JIT */
  val mdf_injected: Int = 9  /** field was injected */
  val mdf_constval: Int = 10 /** field has a ConstantValue bytecode attribute */
  val mdf_constr: Int = 14 /** constructor */

  val MDF_FIELD_MASK: Set32 = JMDF_FIELD_MASK | Set32.of(mdf_statini.toUByte, mdf_deprecated.toUByte, mdf_injected.toUByte, mdf_constval.toUByte)
  val MDF_METHOD_MASK: Set32 = JMDF_METHOD_MASK | Set32.of(mdf_constr.toUByte, mdf_deprecated.toUByte)

  val EDITION_ENTERPRISE: Int = 5

  val VCF_MAGIC: Short = 0x6376 // reversed "vc"
  val VCF_DATA_LEN: Short = 0xC // VCFData remaining length that is 12 bytes
}