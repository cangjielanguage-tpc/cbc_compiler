;strict
@main_type "default"

@aot_deps "cangjie-std-core"

@aot.direct printlnLnk = "_CNat7printlnHl"

@method_ref println = std.core@aref println(I64)Unit [SRET, AOT] #printlnLnk

@method_ref sizeOpen  = default@ref sizeOpen(UAddr)I64
@method_ref alignOpen = default@ref alignOpen(UAddr)I64
@method_ref sizeG     = default@ref sizeG(UAddr)I64
@method_ref alignG    = default@ref alignG(UAddr)I64

; Both fields depend on the supplied type arguments.
@type default:Compound
  @flags PUBLIC RECORD
  @type_vars 2

  @field a %0
    @flags PUBLIC
  @end

  @field a2 %0
    @flags PUBLIC
  @end

  @field b %1
    @flags PUBLIC
  @end
@end

; A nested instantiation must preserve the layout of its argument.
@type default:Foo
  @flags PUBLIC RECORD
  @type_vars 1
  @field value %0
    @flags PUBLIC
  @end
@end

@type default

  ; The positional template binds the TypeInfo staged in IR1.
  @method sizeOpen(UAddr)I64
    @code
      @live.prim IR1
      load.type.info.g IR2, default:Foo[%0]@rec
      @dead IR1
      size.g IR2, IR2
      mov.64 IR1, IR2
      ret.64 IR1
    @end
  @end

  @method alignOpen(UAddr)I64
    @code
      @live.prim IR1
      load.type.info.g IR2, default:Foo[%0]@rec
      @dead IR1
      align.g IR2, IR2
      mov.64 IR1, IR2
      ret.64 IR1
    @end
  @end

  ; TypeInfo pointers are explicit primitive operands.
  @method sizeG(UAddr)I64
    @code
      @live.prim IR1
      size.g IR1, IR1
      ret.64 IR1
    @end
  @end

  @method alignG(UAddr)I64
    @code
      @live.prim IR1
      align.g IR1, IR1
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

      align IR2, I64
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

      size IR2, default:Compound[I32, I64]@rec
      call.direct #println, IRZ, IR2       ; 16
      @dead IR2

      align IR2, default:Compound[I32, I64]@rec
      call.direct #println, IRZ, IR2       ; 8
      @dead IR2

      ; ---- construct compound TypeInfos from explicit arguments ----
      load.type.info IR2, I64
      call.direct #sizeOpen, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 8

      load.type.info IR2, I64
      call.direct #alignOpen, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 8

      ; ---- parameter queries: same callee, two specializations ----
      load.type.info IR2, I64
      call.direct #sizeG, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 8

      load.type.info IR2, U8
      call.direct #sizeG, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 1

      load.type.info IR2, I64
      call.direct #alignG, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 8

      load.type.info IR2, U8
      call.direct #alignG, IR2
      @dead IR2
      call.direct #println, IRZ, IR1       ; 1

      ; Mixed, reordered, repeated and nested positional arguments.
      @dead IR1
      load.type.info IR1, U8
      load.type.info IR2, I64
      load.type.info.g IR3, default:Compound[%0, %1]@rec
      @dead IR1, IR2
      size.g IR3, IR3
      call.direct #println, IRZ, IR3       ; 16
      @dead IR3

      load.type.info IR1, I64
      load.type.info IR2, U8
      load.type.info.g IR3, default:Compound[%0, %1]@rec
      @dead IR1, IR2
      size.g IR3, IR3
      call.direct #println, IRZ, IR3       ; 24
      @dead IR3

      load.type.info IR1, U8
      mov.64 IR2, IR1
      load.type.info.g IR3, default:Compound[%0, %1]@rec
      @dead IR1, IR2
      size.g IR3, IR3
      call.direct #println, IRZ, IR3       ; 3
      @dead IR3

      load.type.info IR1, I32
      load.type.info.g IR2, default:Foo[%0]@rec
      @dead IR1
      mov.64 IR1, IR2
      @dead IR2
      load.type.info IR2, U8
      load.type.info.g IR3, default:Compound[%0, %1]@rec
      @dead IR1, IR2
      size.g IR3, IR3
      call.direct #println, IRZ, IR3       ; 12
      @dead IR3

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
