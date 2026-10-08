;strict
@main_type "default"

@aot_deps "cangjie-std-core"

@aot.direct printlnLnk = "_CNat7printlnHl"

@method_ref println = std.core@aref println(I64)Unit [SRET, AOT] #printlnLnk

@type default

  @method main()I64
    @code
      movi.64 IR1, 42
      call.direct #println, IRZ, IR1
      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
