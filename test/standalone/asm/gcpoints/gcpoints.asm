;strict
@main_type "default"

@aot_deps "aot"

@aot.direct gcPointLnk = "_CN3aot11testGcPointHv"

@method_ref testGcPoint = aot@aref testGcPoint()I64 #gcPointLnk

@type default

  @method main()I64
    @code
      call.direct IR1, #testGcPoint
      movi.64 IR1, 0x10000
      movi.64 IR3, 0x1
l:
      sub.64 IR1, IR1, IR3
      gcpoint
      bcc.64 NE, IR1, IRZ, l
      ret.64 IR1
    @end
  @end
@end
