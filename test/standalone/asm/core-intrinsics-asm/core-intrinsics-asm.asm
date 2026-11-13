;strict
@main_type "default"

@aot_deps "cangjie-std-core"

@aot.direct printlnLnk = "_CNat7printlnHl"

@method_ref println = std.core@aref println(I64)Unit [SRET, AOT] #printlnLnk

@type default
  @method main()I64
    @code
      ; ---- ti.size / ti.align of I64 ----
      load.type.info IR1, I64
      ti.size IR2, IR1
      ti.align IR3, IR1
      @dead IR1
      call.direct #println, IRZ, IR2       ; 8
      @dead IR2
      call.direct #println, IRZ, IR3       ; 8
      @dead IR3

      ; ---- ti.size / ti.align of U8 ----
      load.type.info IR1, U8
      ti.size IR2, IR1
      ti.align IR3, IR1
      @dead IR1
      call.direct #println, IRZ, IR2       ; 1
      @dead IR2
      call.direct #println, IRZ, IR3       ; 1
      @dead IR3

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
