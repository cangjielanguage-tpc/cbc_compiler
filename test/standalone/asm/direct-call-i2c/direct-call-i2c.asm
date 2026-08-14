;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printlnLnk = "_CN3aot9testPrintHl"

@method_ref testPrint = aot@aref testPrint()I64 #printlnLnk

@type default

  @method main()I64
    @code
      movi.64 IR1, 42
      call.direct IR1, #testPrint
      ret.64 IR1
    @end
  @end
@end
