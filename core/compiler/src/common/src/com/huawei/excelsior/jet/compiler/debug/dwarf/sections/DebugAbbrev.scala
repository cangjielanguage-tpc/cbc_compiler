/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.debug.dwarf.sections

import com.huawei.excelsior.jet.assembler.Segment
import com.huawei.excelsior.jet.compiler.debug.dwarf.Dwarf
import com.huawei.excelsior.jet.compiler.debug.dwarf.sections.AbbreviationsElements.*

/** Abbreviations describe popular encoded structures in format:
  * - abbreviation tag
  * - list of attributes, each one with corresponding encoding format
  *
  * Content of .debug_abbrev section.
  *
  * 7.5.3 Abbreviations Tables
  * 7.5.4 Attribute Encodings
  *
  * @author gatimosh
  * @author conwor
  */
object AbbreviationsElements {

  /////////////////////////////////////////////////////////////////////////////
  // Abbreviations description elements

  case class Tag(encoding: Int)

  object DW_TAG_class_type            extends Tag(0x02)
  object DW_TAG_enumeration_type      extends Tag(0x04)
  object DW_TAG_formal_parameter      extends Tag(0x05)
  object DW_TAG_lexical_block         extends Tag(0x0b)
  object DW_TAG_member                extends Tag(0x0d)
  object DW_TAG_pointer_type          extends Tag(0x0f)
  object DW_TAG_compile_unit          extends Tag(0x11)
  object DW_TAG_structure_type        extends Tag(0x13)
  object DW_TAG_inheritance           extends Tag(0x1c)
  object DW_TAG_inlined_subroutine    extends Tag(0x1d)
  object DW_TAG_base_type             extends Tag(0x24)
  object DW_TAG_const_type            extends Tag(0x26)
  object DW_TAG_enumerator            extends Tag(0x28)
  object DW_TAG_subprogram            extends Tag(0x2e)
  object DW_TAG_variable              extends Tag(0x34)
  object DW_TAG_namespace             extends Tag(0x39)
  object DW_TAG_unspecified_type      extends Tag(0x3b)


  case class Form(encoding: Int, hasParams: Boolean = true)

  object DW_FORM_addr         extends Form(0x01)
  object DW_FORM_data2        extends Form(0x05)
  object DW_FORM_data4        extends Form(0x06)
  object DW_FORM_string       extends Form(0x08)
  object DW_FORM_data1        extends Form(0x0b)
  object DW_FORM_strp         extends Form(0x0e)
  object DW_FORM_udata        extends Form(0x0f)
  object DW_FORM_ref_addr     extends Form(0x10)
  object DW_FORM_ref_udata    extends Form(0x15)
  object DW_FORM_sec_offset   extends Form(0x17)
  object DW_FORM_exprloc      extends Form(0x18)
  object DW_FORM_flag_present extends Form(0x19, hasParams = false)


  case class Attribute(encoding: Int, form: Form) // TODO-DWARF: support attributes with custom encoding form

  object DW_AT_location              extends Attribute(0x02, DW_FORM_exprloc      )
  object DW_AT_name                  extends Attribute(0x03, DW_FORM_strp         )
  object DW_AT_byte_size             extends Attribute(0x0b, DW_FORM_udata        )
  object DW_AT_bit_size              extends Attribute(0x0d, DW_FORM_udata        )
  object DW_AT_stmt_list             extends Attribute(0x10, DW_FORM_sec_offset   )
  object DW_AT_low_pc                extends Attribute(0x11, DW_FORM_addr         )
  object DW_AT_high_pc               extends Attribute(0x12, DW_FORM_data4        )
  object DW_AT_language              extends Attribute(0x13, DW_FORM_data2        )
  object DW_AT_comp_dir              extends Attribute(0x1b, DW_FORM_strp         )
  object DW_AT_const_value           extends Attribute(0x1c, DW_FORM_udata        )
  object DW_AT_inline                extends Attribute(0x20, DW_FORM_data1        )
  object DW_AT_producer              extends Attribute(0x25, DW_FORM_strp         )
  object DW_AT_abstract_origin       extends Attribute(0x31, DW_FORM_ref_udata    )
  object DW_AT_accessibility         extends Attribute(0x32, DW_FORM_data1        )
  object DW_AT_artificial            extends Attribute(0x34, DW_FORM_flag_present )
  object DW_AT_calling_convention    extends Attribute(0x36, DW_FORM_data1        )
  object DW_AT_data_member_location  extends Attribute(0x38, DW_FORM_udata        )
  object DW_AT_decl_file             extends Attribute(0x3a, DW_FORM_udata        )
  object DW_AT_decl_line             extends Attribute(0x3b, DW_FORM_udata        )
  object DW_AT_declaration           extends Attribute(0x3c, DW_FORM_flag_present )
  object DW_AT_encoding              extends Attribute(0x3e, DW_FORM_data1        )
  object DW_AT_external              extends Attribute(0x3f, DW_FORM_flag_present )
  object DW_AT_frame_base            extends Attribute(0x40, DW_FORM_exprloc      )
  object DW_AT_specification         extends Attribute(0x47, DW_FORM_ref_udata    )
  object DW_AT_spec_addr             extends Attribute(0x47, DW_FORM_ref_addr     )
  object DW_AT_type                  extends Attribute(0x49, DW_FORM_ref_addr     )
  object DW_AT_call_file             extends Attribute(0x58, DW_FORM_udata        )
  object DW_AT_call_line             extends Attribute(0x59, DW_FORM_udata        )
  object DW_AT_main_subprogram       extends Attribute(0x6a, DW_FORM_flag_present )
  object DW_AT_enum_class            extends Attribute(0x6d, DW_FORM_flag_present )
  object DW_AT_linkage_name          extends Attribute(0x6e, DW_FORM_strp         )

  val DW_CC_PASS_BY_REF = 4 // value for DW_AT_calling_convention

  val DW_ATE_address        = 0x01
  val DW_ATE_boolean        = 0x02
  val DW_ATE_float          = 0x04
  val DW_ATE_signed         = 0x05
  val DW_ATE_unsigned       = 0x07
  val DW_ATE_unsigned_char  = 0x08
  val DW_ATE_UTF            = 0x10
}

object DebugAbbrev extends Dwarf.Section {

  private var nextIndex: Int = 1 // the first abbr has id == 1

  final class Abbreviation(val tag: Tag, val hasChildren: Boolean, val attributes: Attribute*) {
    val index = nextIndex
    nextIndex += 1

    uleb128(index)
    uleb128(tag.encoding)
    if (hasChildren) {
      ubyte(0x01) // DW_CHILDREN_yes
    } else {
      ubyte(0x00) // DW_CHILDREN_no
    }
    for (attribute <- attributes) {
      uleb128(attribute.encoding)
      uleb128(attribute.form.encoding)
    }
    uhalf(0) // 2 zero bytes terminate each abbreviation description
  }

  override def close(): Segment = {
    ubyte(0x00) // 1 zero byte terminates the abbreviations table for a compile_unit
    super.close()
  }

  val commonAbbreviations = newBoundLabel

  val CompUnit                  = new Abbreviation(DW_TAG_compile_unit,         true,   DW_AT_name, DW_AT_language, DW_AT_producer, DW_AT_comp_dir, DW_AT_stmt_list)
  val CompUnitAux               = new Abbreviation(DW_TAG_compile_unit,         true,   DW_AT_name, DW_AT_language)
  val CompUnitWithMain          = new Abbreviation(DW_TAG_compile_unit,         true,   DW_AT_name, DW_AT_language, DW_AT_producer, DW_AT_comp_dir, DW_AT_stmt_list, DW_AT_main_subprogram)
  val ClassTypeSpec             = new Abbreviation(DW_TAG_structure_type,       true,   DW_AT_calling_convention, DW_AT_name, DW_AT_byte_size)
  val SubProgramSpec            = new Abbreviation(DW_TAG_subprogram,           true,   DW_AT_name, DW_AT_linkage_name, DW_AT_declaration, DW_AT_decl_file, DW_AT_decl_line, DW_AT_type, DW_AT_frame_base)
  val SubProgramInlSpec         = new Abbreviation(DW_TAG_subprogram,           false,  DW_AT_name, DW_AT_linkage_name, DW_AT_declaration, DW_AT_decl_file, DW_AT_decl_line, DW_AT_type, DW_AT_inline)
  val SubInst                   = new Abbreviation(DW_TAG_subprogram,           false,  DW_AT_specification,   DW_AT_low_pc, DW_AT_high_pc)
  val SubInstChldYes            = new Abbreviation(DW_TAG_subprogram,           true,   DW_AT_specification,   DW_AT_low_pc, DW_AT_high_pc)
  val SubInlInst                = new Abbreviation(DW_TAG_inlined_subroutine,   false,  DW_AT_abstract_origin, DW_AT_low_pc, DW_AT_high_pc, DW_AT_call_file, DW_AT_call_line)
  val SubInlInstChldYes         = new Abbreviation(DW_TAG_inlined_subroutine,   true,   DW_AT_abstract_origin, DW_AT_low_pc, DW_AT_high_pc, DW_AT_call_file, DW_AT_call_line)
  val TypeStaticMember          = new Abbreviation(DW_TAG_member,               false,  DW_AT_name, DW_AT_type, DW_AT_external, DW_AT_declaration, DW_AT_accessibility)
  val TypeMember                = new Abbreviation(DW_TAG_member,               false,  DW_AT_name, DW_AT_type, DW_AT_data_member_location)
  val FakeTypeMember            = new Abbreviation(DW_TAG_member,               false,  DW_AT_name, DW_AT_type, DW_AT_data_member_location, DW_AT_artificial)
  val TypeBase                  = new Abbreviation(DW_TAG_inheritance,          false,  DW_AT_type, DW_AT_data_member_location)
  val FormalParameter           = new Abbreviation(DW_TAG_formal_parameter,     false,  DW_AT_name, DW_AT_location, DW_AT_type)
  val FormalParameterWithDecl   = new Abbreviation(DW_TAG_formal_parameter,     false,  DW_AT_name, DW_AT_location, DW_AT_type, DW_AT_decl_file, DW_AT_decl_line)
  val GlobalStatic              = new Abbreviation(DW_TAG_variable,             false,  DW_AT_spec_addr, DW_AT_location)
  val GlobalWithLocAndLn        = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type, DW_AT_linkage_name)
  val GlobalWithLocDeclAndLn    = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type, DW_AT_decl_file, DW_AT_decl_line, DW_AT_linkage_name)
  val GlobalWithLoc             = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type)
  val GlobalWithLocAndDecl      = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type, DW_AT_decl_file, DW_AT_decl_line)
  val GlobalExtWithLocAndLn     = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type, DW_AT_external, DW_AT_linkage_name)
  val GlobalExtWithLocDeclAndLn = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type, DW_AT_external, DW_AT_decl_file, DW_AT_decl_line, DW_AT_linkage_name)
  val GlobalExtWithLoc          = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type, DW_AT_external)
  val GlobalExtWithLocAndDecl   = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type, DW_AT_external, DW_AT_decl_file, DW_AT_decl_line)
  val VarWithLoc                = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type)
  val VarWithLocAndDecl         = new Abbreviation(DW_TAG_variable,             false,  DW_AT_name, DW_AT_location, DW_AT_type, DW_AT_decl_file, DW_AT_decl_line)
  val Namespace                 = new Abbreviation(DW_TAG_namespace,            true,   DW_AT_name)
  val CangjieSubprogram         = new Abbreviation(DW_TAG_subprogram,           true,   DW_AT_name, DW_AT_linkage_name, DW_AT_declaration, DW_AT_decl_file, DW_AT_decl_line, DW_AT_type, DW_AT_frame_base, DW_AT_low_pc, DW_AT_high_pc)
  val CangjieSubprogramMain     = new Abbreviation(DW_TAG_subprogram,           true,   DW_AT_name, DW_AT_linkage_name, DW_AT_declaration, DW_AT_decl_file, DW_AT_decl_line, DW_AT_type, DW_AT_frame_base, DW_AT_low_pc, DW_AT_high_pc, DW_AT_main_subprogram)
  val CangjieVoidType           = new Abbreviation(DW_TAG_structure_type,       false,  DW_AT_name, DW_AT_byte_size)
  val JavaClassType             = new Abbreviation(DW_TAG_class_type,           true,   DW_AT_name, DW_AT_byte_size, DW_AT_decl_file)
  val JavaVoidType              = new Abbreviation(DW_TAG_unspecified_type,     false,  DW_AT_name)
  val JavaSubprogramSpec        = new Abbreviation(DW_TAG_subprogram,           true,   DW_AT_name, DW_AT_declaration, DW_AT_decl_file, DW_AT_decl_line, DW_AT_type)
  val JavaSubprogramInstance    = new Abbreviation(DW_TAG_subprogram,           true,   DW_AT_external, DW_AT_low_pc, DW_AT_high_pc, DW_AT_frame_base, DW_AT_spec_addr)
  val AbbrDTCustomByteSized     = new Abbreviation(DW_TAG_base_type,            false,  DW_AT_name, DW_AT_encoding, DW_AT_byte_size)
  val AbbrDTArray               = new Abbreviation(DW_TAG_structure_type,       true,   DW_AT_name, DW_AT_byte_size) // TODO-DWARF AT_alignment
  val AbbrDTRecord              = new Abbreviation(DW_TAG_structure_type,       true,   DW_AT_name, DW_AT_byte_size) // TODO-DWARF can we use also for arrays?
  val AbbrDTConst               = new Abbreviation(DW_TAG_const_type,           false,  DW_AT_type)
  val AbbrDTPointer             = new Abbreviation(DW_TAG_pointer_type,         false,  DW_AT_type)
  val AbbrDTEnumeration         = new Abbreviation(DW_TAG_enumeration_type,     true,   DW_AT_type, DW_AT_enum_class, DW_AT_name, DW_AT_byte_size)
  val AbbrDTEnumerator          = new Abbreviation(DW_TAG_enumerator,           false,  DW_AT_name, DW_AT_const_value)
  val LexicalBlock              = new Abbreviation(DW_TAG_lexical_block,        true,   DW_AT_low_pc, DW_AT_high_pc)
}