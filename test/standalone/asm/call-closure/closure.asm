;strict
@main_type "default"

@aot_deps "aot:cangjie-std-core"

@aot.direct print  = "_CN3aot5printHRNat6StringE"
@aot.direct guess  = "_CN3aot9guessSRetHll"
@aot.direct sret   = "_CN3aot4sretHv"
@aot.direct getstr = "_CN3aot6getstrHv"

@method_ref print  = aot@aref print(std.core:String@arec)I64 #print
@method_ref guess  = aot@aref guess(I64, I64)Unit #guess
@method_ref sret   = aot@aref sret()Bool #sret
@method_ref getstr = aot@aref getstr()std.core:String@arec #getstr

@field_ref sret_flag = default@ref sret I64

@type default:MyLambda
  @flags PUBLIC LAMBDA
  @super ()I64

  @field `$gf` I64
    @flags PUBLIC
  @end
  @field `$if` I64
    @flags PUBLIC
  @end

  @method `$g`()Box[I64]
    @flags VIRTUAL SRET HAS_OUTER_TI
    @code
      ld.static IR11, #sret_flag
      bcci.64 EQ, IR11, 0, aarch64
      @dead IR11

    x64:
      @live.rec IR1 ; sret slot
      @live.ref IR2 ; lambda
      @live.prim IR3 ; outer ti

      mov.ref IR11, IR1
      mov.ref IR12, IR2
      @dead IR1, IR2, IR3

      jmp shared
    aarch64:
      @live.rec IR9 ; sret slot
      @live.ref IR1 ; lambda
      @live.prim IR2 ; outer ti

      @dead IR11, IR12 ; manual marking is dumb
      mov.ref IR11, IR9
      mov.ref IR12, IR1
      @dead IR1, IR2, IR9
    shared:
      mov.ref IR1, IR12
      call.closure ()I64
      @dead IR1
      @live.prim IR1

      box IR1, IR2, I64

      ms.hd.rec IR11
        ms.const.idx 0, [Box[I64]]
        ms.st IR2
      @dead IR11, IR2, IR1

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end

  @method `$i`()I64
    @flags VIRTUAL
    @code
      movi.64 IR1, 42
      ret.64 IR1
    @end
  @end
@end

@type default:MyLambda2
  @flags PUBLIC LAMBDA
  @super ()std.core:String@arec

  @field `$gf` I64
    @flags PUBLIC
  @end
  @field `$if` I64
    @flags PUBLIC
  @end

  @method `$g`()Box[std.core:String@arec]
    @flags VIRTUAL SRET HAS_OUTER_TI
    @code
      ; incorrect code
      movi.64 IR1, 0x20
      ret.64 IR1
    @end
  @end

  @method `$i`()std.core:String@arec
    @flags VIRTUAL SRET
    @code
      ;; pass current args as args to next call
      call.direct IR1, #getstr
      @live.prim IR1
      ret.64 IR1
    @end
  @end
@end

@type std.core:String
  @flags PUBLIC AOT RECORD

  @field `mydata` std.core:Object@aref
    @flags PUBLIC
  @end
  @field `start` I32
    @flags PUBLIC
  @end
  @field `len` I32
    @flags PUBLIC
  @end
@end

@type std.core:Object
  @flags PUBLIC AOT
@end

@type default

  @field `sret` I64
    @flags PUBLIC STATIC
  @end

  @method main()I64
    @typed_slots std.core:String@arec, [std.core:Object@aref]
    @code
      movi.64 IR1, 0
      movi.64 IR2, 1
      call.direct IR1, #guess
      @dead IR1, IR2

      ; dynamically test whether we are executing on aarch64 or x64
      call.direct IR1, #sret
      @live.prim IR1
      st.static IR1, #sret_flag
      @dead IR1

      new.closure default:MyLambda@ref
      mov.ref IR11, IR1
      @dead IR1

      call.closure ()I64
      @live.prim IR1

      bcci.64 EQ, IR1, 42, success1
      @dead IR1, IR11

      movi.64 IR1, 0x1
      ret.64 IR1
      @dead IR1
success1:
      new.closure default:MyLambda2@ref
      mov.ref IR11, IR1
      @dead IR1

      ld.static IR1, #sret_flag

      bcci.64 EQ, IR1, 0, no_sret_shift
      jmp has_sret_shift
      @dead IR1
no_sret_shift:
      ; check call.closure sret in concrete context
      prepare.rec $0
      ld.stack.rec IR9, $0
      mov.ref IR1, IR11
      call.closure ()std.core.String@arec
      @dead IR1, IR9

      ld.stack.rec IR1, $0
      call.direct IR1, #print
      @dead IR1

      ; check generic call closure
      @dead IR11
      new.closure default:MyLambda@ref

      ; ! this pattern is not the same as in cjnative !
      ; CJNative creates box in callsite and passes it to aot code
      ; through stack-allocated slot.
      ; In case of references, this slot would hold reference itself (not box).
      ;
      ; The result is always being re-read from stack-slot, so "reallocating box inside of generic code
      ; and storing it to the slot" is compatible behavior

      prepare.rec $1
      ld.stack.rec IR9, $1
      load.type.info.obj IR2, IR1
      call.closure.g ()%0
      @dead IR1, IR2, IR9

      ms.hd.typed $1
        ms.const.idx 0, [std.core:Object@aref]
        ms.ld IR1

      unbox IR2, IR1, I64
      @dead IR1

      bcci.64 EQ, IR2, 42, no_shift_end

      movi.64 IR1, 0x1
      ret.64 IR1
      @dead IR1
no_shift_end:
      movi.64 IR1, 0x0
      ret.64 IR1
      @dead IR1
has_sret_shift:
      @dead IR2
      @live.ref IR11
      ; check call.closure sret in concrete context
      prepare.rec $0
      ld.stack.rec IR1, $0
      mov.ref IR2, IR11
      call.closure ()std.core:String@arec
      @dead IR1, IR2

      ld.stack.rec IR1, $0
      call.direct IR1, #print
      @dead IR1

      ; check generic call closure
      @dead IR11
      new.closure default:MyLambda@ref
      mov.ref IR2, IR1
      @dead IR1

      ; ! not correct pattern !
      ; CJNative creates box in callsite and passes it to aot code

      prepare.rec $1
      ld.stack.rec IR1, $1
      load.type.info.obj IR3, IR2
      call.closure.g ()%0
      @dead IR1, IR2, IR3

      ms.hd.typed $1
        ms.const.idx 0, [std.core:Object@aref]
        ms.ld IR1

      unbox IR2, IR1, I64
      @dead IR1

      bcci.64 EQ, IR2, 42, shift_end

      movi.64 IR1, 0x1
      ret.64 IR1
      @dead IR1
shift_end:

      movi.64 IR1, 0x0
      ret.64 IR1
    @end
  @end
@end
