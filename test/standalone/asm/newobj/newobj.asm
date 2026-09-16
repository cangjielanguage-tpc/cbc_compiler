;strict

@main_type "default"

@aot_deps "aot"

@type default
  @method main()I64
    @code
      newobj aot:Foo@aref
      @dead IR1
      movi.64 IR1, 42
      ret.64 IR1
    @end
  @end
@end
