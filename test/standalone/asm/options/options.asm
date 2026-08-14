@main_type "default"

@type std.core:Object
  @flags PUBLIC AOT
@end

@type default:MyOption
  @flags PUBLIC ENUM
  @enum_kind Option0
  @type_vars 1
  @enum %0
@end

@type default:OptionHolder
  @flags PUBLIC
  @super std.core:Object@aref
  @type_vars 1

  @field third default:MyOption[%0]@nopt
    @flags PUBLIC
  @end
  @field first default:MyOption[I64]@uopt
    @flags PUBLIC
  @end
  @field second default:MyOption[std.core:Object@aref]@nopt
    @flags PUBLIC
  @end
@end

@field_ref second = default:OptionHolder[std.core:Object@aref]@ref second default:MyOption[std.core:Object@aref]@nopt
@field_ref first = default:OptionHolder[std.core:Object@aref]@ref first default:MyOption[I64]@nopt

@type default
  @method main()I64
    @saved_iregs IR12
    @code
      newobj default:OptionHolder[I64]@ref
      @dead IR1
      newobj default:OptionHolder[std.core:Object@aref]@ref
      mov.ref IR11, IR1
      @dead IR1

      movi.64 IR1, 0

      ;; load none of union option
      load.type.info IR2, default:MyOption[I64]@uopt
      load.type.info IR5, I64

      ms.hd.obj IR11
        ms.field #first
        ms.ld.g IR3, IR2

      tag.g IR4, IR3, IR5, default:MyOption[%0]@nopt

      ; note that tag == 0 designates Some case
      bcci.64 NE, IR4, 0, fail
      @dead IR2, IR3, IR4, IR5

      ;; load none of nullable option
      load.type.info IR2, default:MyOption[std.core:Object@aref]@nopt
      load.type.info IR5, std.core:Object@aref

      ms.hd.obj IR11
        ms.field #second
        ms.ld.g IR3, IR2

      tag.g IR4, IR3, IR5, default:MyOption[%0]@nopt

      bcci.64 EQ, IR4, 0, fail
      @dead IR2, IR3, IR4, IR5

      ;; load some of nullable option
      load.type.info IR2, default:MyOption[std.core:Object@aref]@nopt
      load.type.info IR5, std.core:Object@aref

      ms.hd.obj IR11
        ms.field #second
        ms.st IR11

      ms.hd.obj IR11
        ms.field #second
        ms.ld.g IR3, IR2

      tag.g IR4, IR3, IR5, default:MyOption[%0]@nopt
      bcci.64 NE, IR4, 0, fail

      payload.g IR6, IR3, IR5, IR2, default:MyOption[%0]@nopt
      bcc.64 RNE, IR6, IR11, fail
      @dead IR2, IR3, IR4, IR5, IR6

      ;; load some of union option
      ;; tag 0 is already present
      load.type.info IR2, default:MyOption[I64]@uopt
      load.type.info IR5, I64

      ; store directly
      ms.hd.obj IR11
        ms.field #first
        ms.const.idx 1, [U8, I64]
        ms.st.imm 42

      ; load boxed
      ms.hd.obj IR11
        ms.field #first
        ms.ld.g IR3, IR2

      ; check for Some
      tag.g IR4, IR3, IR5, default:MyOption[%0]@nopt
      bcci.64 NE, IR4, 0, fail

      ; get Box<Int64>
      payload.g IR6, IR3, IR5, IR2, default:MyOption[%0]@nopt

      ; unbox value
      unbox IR7, IR6, I64
      bcci.64 NE, IR7, 42, fail
      @dead IR2, IR3, IR4, IR5, IR6, IR7

      ;; option creation
      load.type.info IR2, default:MyOption[std.core:Object@aref]@nopt
      load.type.info IR5, std.core:Object@aref

      new.some.g IR4, IR11, IR5, IR2, default:MyOption[%0]@nopt
      new.none.g IR6, IR5, IR2, default:MyOption[%0]@nopt

      tag.g IR7, IR4, IR5, default:MyOption[%0]@nopt
      tag.g IR8, IR6, IR5, default:MyOption[%0]@nopt

      ; kind is Option1, so tags are inverted
      bcci.64 EQ, IR7, 1, fail
      bcci.64 EQ, IR8, 0, fail

      jmp exit
  fail:
      addi.64 IR1, IR1, 1
  exit:
      @dead IR11
      ret.64 IR1
    @end
  @end
@end
