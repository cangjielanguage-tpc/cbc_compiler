;strict
@main_type "default"

@aot_deps "cangjie-std-core"

@aot.direct printlnLnk = "_CNat7printlnHl"

@method_ref println = std.core@aref println(I64)Unit [SRET, AOT] #printlnLnk

@method_ref sizeOpen       = default@ref sizeOpen()I64 (%0)
@method_ref alignOpen      = default@ref alignOpen()I64 (%0)
@method_ref sizeOpenCPtr   = default@ref sizeOpenCPtr()I64 (%0)
@method_ref sizeOpenCPtrN  = default@ref sizeOpenCPtrN()I64 (%0)
@method_ref sizeG          = default@ref sizeG()I64 (%0)
@method_ref alignG         = default@ref alignG()I64 (%0)

; generic two-field record; layout (I32 then I64) -> size 16, align 8
@type default:Compound
  @flags PUBLIC RECORD
  @type_vars 2

  @field a I32
    @flags PUBLIC
  @end

  @field b I64
    @flags PUBLIC
  @end
@end

; generic record without fields -> size 0, align 1
@type default:Foo
  @flags PUBLIC RECORD
  @type_vars 1
@end

@type default

  ; open type operand: the hole binds at runtime from the incoming
  ; GenericFuncParams slot (IR1); the type is not re-read by any
  ; instruction here, so IR1 needs no liveness declaration
  @method sizeOpen()I64
    @generic_params 1
    @code
      size IR2, default:Foo[%0]@rec
      mov.64 IR1, IR2
      ret.64 IR1
    @end
  @end

  @method alignOpen()I64
    @generic_params 1
    @code
      align IR2, default:Foo[%0]@rec
      mov.64 IR1, IR2
      ret.64 IR1
    @end
  @end

  ; CPointer operands are hole-independent: folded at rewrite time
  @method sizeOpenCPtr()I64
    @generic_params 1
    @code
      size IR2, CPtr[%0]
      mov.64 IR1, IR2
      ret.64 IR1
    @end
  @end

  @method sizeOpenCPtrN()I64
    @generic_params 1
    @code
      size IR2, CPtr[default:Compound[I32, %0]@rec]
      mov.64 IR1, IR2
      ret.64 IR1
    @end
  @end

  ; immediate GenericFuncParams index: slot 0 of this method
  @method sizeG()I64
    @generic_params 1
    @code
      size.g IR1, 0
      ret.64 IR1
    @end
  @end

  @method alignG()I64
    @generic_params 1
    @code
      align.g IR1, 0
      ret.64 IR1
    @end
  @end

  @method main()I64
    @code
      @live.prim IR1                        ; call results arrive in IR1

      ; ---- closed size / align rows ----
      size IR2, U32
      call.direct #println, IRZ, IR2       ; 4
      @dead IR2

      size IR2, I64
      call.direct #println, IRZ, IR2       ; 8
      @dead IR2

      size IR2, Unit
      call.direct #println, IRZ, IR2       ; 0
      @dead IR2

      align IR2, Unit
      call.direct #println, IRZ, IR2       ; 1
      @dead IR2

      size IR2, Bool
      call.direct #println, IRZ, IR2       ; 1
      @dead IR2

      size IR2, [I32, I64]
      call.direct #println, IRZ, IR2       ; 16
      @dead IR2

      align IR2, [I32, I64]
      call.direct #println, IRZ, IR2       ; 8
      @dead IR2

      size IR2, CPtr[U32]
      call.direct #println, IRZ, IR2       ; 8
      @dead IR2

      size IR2, CPtr[CPtr[U32]]
      call.direct #println, IRZ, IR2       ; 8
      @dead IR2

      size IR2, default:Compound[I32, I64]@rec
      call.direct #println, IRZ, IR2       ; 16
      @dead IR2

      align IR2, default:Compound[I32, I64]@rec
      call.direct #println, IRZ, IR2       ; 8
      @dead IR2

      ; ---- open type operand rows (generic callees) ----
      load.type.info IR2, I64
      call.direct #sizeOpen, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 0

      load.type.info IR2, I64
      call.direct #alignOpen, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 1

      load.type.info IR2, I64
      call.direct #sizeOpenCPtr, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 8

      load.type.info IR2, I64
      call.direct #sizeOpenCPtrN, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 8

      ; ---- size.g / align.g rows: same callee, two specializations ----
      load.type.info IR2, I64
      call.direct #sizeG, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 8

      load.type.info IR2, U32
      call.direct #sizeG, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 4

      load.type.info IR2, I64
      call.direct #alignG, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 8

      load.type.info IR2, U32
      call.direct #alignG, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 4

      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
