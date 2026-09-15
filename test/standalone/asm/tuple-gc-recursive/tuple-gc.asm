;strict
@main_type "default"

@aot_deps "aot:cangjie-std-core"

@aot.direct collectLnk = "_CN3aot7collectHv"
@method_ref collect = aot@aref collect()I64 #collectLnk

@type std.core:Object
  @flags PUBLIC AOT
@end

@type default:Node
  @flags PUBLIC
  @super std.core:Object@aref

  @field prefix I64
    @flags PUBLIC
  @end
  @field state [I16, [I8, default:Node@ref], default:Node@ref, I64]
    @flags PUBLIC
  @end
@end

@field_ref prefix = default:Node@ref prefix I64
@field_ref state = default:Node@ref state [I16, [I8, default:Node@ref], default:Node@ref, I64]

@type default
  @method main()I64
    @saved_iregs IR12

    ; Frame layout requests the inner tuple TypeInfo before rewriting newobj.
    ; Construction recurses: inner tuple -> Node -> state tuple -> inner tuple.
    ; The last tuple TypeInfo is still pending, without a completed GC bitmap.
    ; FillRefOffsets must traverse its field layout instead of reading that bitmap.
    @typed_slots [I8, default:Node@ref]

    @code
      zero.refs $0

      newobj default:Node@ref
      @live.ref IR1
      mov.ref IR12, IR1
      @dead IR1

      ; Scalar sentinels surround references, exercising alignment and the
      ; containing-field displacement (state follows the object header + prefix).
      movi.64 IR2, 7
      st.ref.field IR2, IR12, #prefix
      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 0, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.st.imm 5
      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 1, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.const.idx 0, [I8, default:Node@ref]
        ms.st.imm 3
      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 3, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.st.imm 11
      @dead IR2

      ; Two distinct children are reachable only through the root's tuples.
      newobj default:Node@ref
      @live.ref IR1
      movi.64 IR2, 41
      st.ref.field IR2, IR1, #prefix
      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 1, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.const.idx 1, [I8, default:Node@ref]
        ms.st IR1
      @dead IR1 IR2

      newobj default:Node@ref
      @live.ref IR1
      movi.64 IR2, 42
      st.ref.field IR2, IR1, #prefix
      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 2, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.st IR1
      @dead IR1 IR2

      ; Only IR12 is a live register root at collection; the typed slot is null.
      call.direct IR1, #collect
      @dead IR1

      ld.ref.field IR2, IR12, #prefix
      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 0, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.ld IR3
      add.64 IR2, IR2, IR3
      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 1, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.const.idx 0, [I8, default:Node@ref]
        ms.ld IR3
      add.64 IR2, IR2, IR3
      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 3, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.ld IR3
      add.64 IR2, IR2, IR3

      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 1, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.const.idx 1, [I8, default:Node@ref]
        ms.ld IR1
      ld.ref.field IR3, IR1, #prefix
      add.64 IR2, IR2, IR3
      @dead IR1
      ms.hd.obj IR12
        ms.field #state
        ms.const.idx 2, [I16, [I8, default:Node@ref], default:Node@ref, I64]
        ms.ld IR1
      ld.ref.field IR3, IR1, #prefix
      add.64 IR1, IR2, IR3
      @dead IR2 IR3 IR12

      ; 7 + 5 + 3 + 11 + 41 + 42 = 109 (launcher exit status).
      ret.64 IR1
    @end
  @end
@end
