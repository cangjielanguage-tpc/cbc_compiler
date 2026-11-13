;strict
@main_type "default"

@aot_deps "cangjie-std-core"

@aot.direct printlnLnk = "_CNat7printlnHl"

@method_ref println = std.core@aref println(I64)Unit [SRET, AOT] #printlnLnk

@type default
  @method main()I64
    @code
      ; ---- size / align of I64 ----
      size IR2, I64
      align IR3, I64
      call.direct #println, IRZ, IR2       ; 8
      @dead IR2
      call.direct #println, IRZ, IR3       ; 8
      @dead IR3

      ; ---- size / align of U8 ----
      size IR2, U8
      align IR3, U8
      call.direct #println, IRZ, IR2       ; 1
      @dead IR2
      call.direct #println, IRZ, IR3       ; 1
      @dead IR3

      ; ---- two U8 arrays for the refeq identity compare below ----
      ; (vcmp.32 / vidx.32 ISA opcodes were removed, the intrinsics are
      ; unrolled into a primitive loop by the compiler)
      movi.64 IR2, 40
      newarr @arr[U8]
      mov.ref IR4, IR1
      @dead IR1, IR2
      movi.64 IR2, 40
      newarr @arr[U8]
      mov.ref IR7, IR1
      @dead IR1, IR2

      ; ---- refeq identity compare: what the refeq intrinsics lower to ----
      ; the array references are re-marked as raw pointers for the compares
      @dead IR4, IR7
      @live.prim IR4, IR7
      ; the same reference twice compares equal -> 1
      scc.64 EQ, IR8, IR4, IR4
      call.direct #println, IRZ, IR8       ; 1
      @dead IR8
      ; two distinct arrays compare unequal -> 0
      scc.64 EQ, IR8, IR4, IR7
      call.direct #println, IRZ, IR8       ; 0
      @dead IR8

      ; ---- same-width bitcast: i64 <-> f64 bit reinterpret round trip ----
      movi.64 IR5, 0x3FF0000000000000      ; bits of f64 1.0
      movi2f.64 FR1, IR5
      movf2i.64 IR6, FR1
      scc.64 EQ, IR8, IR6, IR5              ; bits preserved -> 1
      call.direct #println, IRZ, IR8       ; 1
      @dead IR8, IR5, IR6, IR4, IR7

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
