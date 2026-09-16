;strict

@main_type "default"

@aot_deps "cangjie-std-core"

@type std.core:Object
  @flags PUBLIC AOT
@end

@type default:Foo
  @flags PUBLIC
  @super std.core:Object@aref

  @field x [I64, I32]
    @flags PUBLIC
  @end
  @field y I64
    @flags PUBLIC
  @end
  @field z [I64, I32]
    @flags PUBLIC
  @end
@end

@field_ref zfr = default:Foo@ref z [I64, I32]

@type default
  @method main()I64
    @typed_slots [I64, I32], [I64, I32]
    @code
      newobj default:Foo@ref

      movi.64 IR2, -2
      movi.64 IR3, 1

      ms.hd.typed $0
        ms.const.idx 0, [I64, I32]
        ms.st IR3

      ms.hd.typed $1
        ms.const.idx 1, [I64, I32]
        ms.st IR2

      ms.hd.obj IR1
        ms.field #zfr
        ms.const.idx 1, [I64, I32]
        ms.st IR2

      ms.hd.typed $0
        ms.const.idx 0, [I64, I32]
        ms.ld IR4

      ms.hd.typed $1
        ms.const.idx 1, [I64, I32]
        ms.ld IR5

      ms.hd.obj IR1
        ms.field #zfr
        ms.const.idx 1, [I64, I32]
        ms.ld IR6

      @dead IR1
      add.32 IR1, IR4, IR5
      add.32 IR1, IR1, IR6
      ret.32 IR1
    @end
  @end
@end
