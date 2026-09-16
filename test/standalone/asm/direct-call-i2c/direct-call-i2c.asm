;strict
@main_type "default"

@aot_deps "aot"

@aot.direct printlnLnk  = "_CN3aot9testPrintHl"
@aot.direct packageInit = "_CGP3aotiiHv"

@method_ref testPrint      = aot@aref testPrint()I64 #printlnLnk
@method_ref packageInitRef = aot@aref packageInit()Unit #packageInit

@type default

  @method main()I64
    @code
      call.direct IR1, #packageInitRef

      movi.64 IR1, 42
      call.direct IR1, #testPrint
      ret.64 IR1
    @end
  @end
@end
