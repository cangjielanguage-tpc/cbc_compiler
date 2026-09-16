;strict
@main_type "default"

@aot_deps "cangjie-std-core"

@aot.direct printlnLnk = "_CNat7printlnHl"

@method_ref println = std.core@aref println()Unit #printlnLnk

@type default

  @method main()I64
    @code
      movi.64 IR1, 42 ; first arg on aarch64
      movi.64 IR2, 42 ; first arg on x86_64 (due to Unit ret-by-val taking up IR1)
      call.direct IR1, #println
      @dead IR1
      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
