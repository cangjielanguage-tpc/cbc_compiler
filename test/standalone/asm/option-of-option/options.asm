@main_type "default"

@type std.core:Object
  @flags PUBLIC AOT
@end

@type default:MyOption
  @flags PUBLIC ENUM
  @enum_kind Option1
  @type_vars 1
  @enum %0
@end

@type default:OptionHolder
  @flags PUBLIC
  @super std.core:Object@aref
  @type_vars 1

  @field first default:MyOption[%0]@nopt
    @flags PUBLIC
  @end
@end

@field_ref first = default:OptionHolder[default:MyOption[std.core:Object@aref]@nopt]@ref first default:MyOption[default:MyOption[std.core:Object@aref]@nopt]@nopt

@type default
  @method main()I64
    @code
      newobj default:OptionHolder[default:MyOption[std.core:Object@aref]@nopt]@ref
      mov.ref IR10, IR1
      @dead IR1

      ; store directly
      ; zero-initialized tag is Some for Option1
      ms.hd.obj IR10
        ms.field #first
        ms.const.idx 1, [Bool, default:MyOption[std.core:Object@aref]@nopt]
        ms.st IR10

      load.type.info IR11, default:MyOption[default:MyOption[std.core:Object@aref]@nopt]@uopt
      load.type.info IR12, default:MyOption[std.core:Object@aref]@nopt
      load.type.info IR13, std.core:Object@aref

      ms.hd.obj IR10
        ms.field #first
        ms.ld.g IR2, IR11

      tag.g IR3, IR2, IR12, default:MyOption[%0]@nopt
      bcci.64 NE, IR3, 0, fail

      ; extract value of type default:MyOption[std.core:Object@aref]@nopt
      ; but in generic context (so it will be boxed)
      payload.g IR4, IR2, IR12, IR11, default:MyOption[%0]@nopt

      ; extract again
      payload.g IR5, IR4, IR13, IR12, default:MyOption[%0]@nopt
      bcc.64 RNE, IR5, IR10, fail

      jmp exit
  fail:
      movi.64 IR1, 1
      ret.64 IR1
  exit:
      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
