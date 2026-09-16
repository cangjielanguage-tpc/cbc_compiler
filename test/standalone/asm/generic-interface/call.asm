;strict
@main_type "default"

@aot_deps "cangjie-std-core"

@type std.core:Object
    @flags AOT
@end

@type default:I
  @flags PUBLIC INTERFACE
  @type_vars 2

  @method foo(%0)I64
    @flags VIRTUAL ABSTRACT
  @end

  @method foo(%1)I64
    @flags VIRTUAL ABSTRACT
  @end
@end

@type default:Foo
  @flags PUBLIC
  @super std.core:Object@aref
  @interfaces default:I[I64, %0]@ref default:I[%0, U64]@ref default:I[I32, std.core:Object@aref]@ref
  @type_vars 1

  @method foo(Box[I64])I64
    @flags VIRTUAL
    @code
      movi.64 IR1, 0x1
      ret.64 IR1
    @end
  @end

  @method foo(%0)I64
    @flags VIRTUAL
    @code
      movi.64 IR1, 0x2
      ret.64 IR1
    @end
  @end

  @method foo(Box[U64])I64
    @flags VIRTUAL
    @code
      movi.64 IR1, 0x3
      ret.64 IR1
    @end
  @end

  @method foo(std.core:Object@aref)I64
    @flags VIRTUAL
    @code
      movi.64 IR1, 0x4
      ret.64 IR1
    @end
  @end
@end

@method_ref fooi64 = default:I[I64, I32]@ref foo(Box[I64])I64
@method_ref fooi32 = default:I[I64, I32]@ref foo(Box[I32])I64
@method_ref foou64 = default:I[I32, U64]@ref foo(Box[U64])I64
@method_ref fooobj = default:I[I32, std.core:Object@aref]@ref foo(std.core:Object@aref)I64

@type default
  @method main()I64
    @saved_iregs IR11
    @code
      newobj default:Foo[I32]@ref
      mov.ref IR11, IR1

      ; i64 case
      call.interf IR1, #fooi64
      @dead IR1
      @live.prim IR1

      bcci.64 NE, IR1, 0x1, fail
      @dead IR1

      ; i32 case
      mov.ref IR1, IR11

      call.interf IR1, #fooi32
      @dead IR1
      @live.prim IR1

      bcci.64 NE, IR1, 0x2, fail
      @dead IR1

      ; u64 case
      mov.ref IR1, IR11

      call.interf IR1, #foou64
      @dead IR1
      @live.prim IR1

      bcci.64 NE, IR1, 0x3, fail
      @dead IR1

      ; obj case
      mov.ref IR1, IR11

      call.interf IR1, #fooobj
      @dead IR1
      @live.prim IR1

      bcci.64 NE, IR1, 0x4, fail
      @dead IR1

      movi.64 IR1, 0
      jmp exit
      @dead IR1
  fail:
      movi.64 IR1, 1
  exit:
      ret.64 IR1
    @end
  @end
@end
