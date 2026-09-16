;strict
@main_type "default"

@aot_deps "aot:cangjie-std-core"

@aot.direct iniLnk = "_CGP3aotiiHv"
@aot.direct waitLnk = "_CN3aot4waitHv"
@aot.direct notifyLnk = "_CN3aot6notifyHv"

@method_ref init   = aot@aref ini()Void #iniLnk
@method_ref wait   = aot@aref wait()I32 #waitLnk
@method_ref notify = aot@aref notify()I32 #notifyLnk

@type default:MyLambda
  @flags PUBLIC LAMBDA
  @super ()I64

  @field `$gf` I64
    @flags PUBLIC
  @end
  @field `$if` I64
    @flags PUBLIC
  @end

  @method `$g`()I64 ; incorrect
    @flags VIRTUAL
    @code
      movi.64 IR1, 0x20
      ret.64 IR1
    @end
  @end

  @method `$i`()I64
    @flags VIRTUAL
    @code
      movi.64 IR1, 42 ; first arg on aarch64
      movi.64 IR2, 42 ; first arg on x86_64 (due to Unit ret-by-val taking up IR1)
      call.direct IR1, #notify
      @dead IR1

      movi.64 IR1, 0x0
      ret.64 IR1
    @end
  @end
@end

@type default
  @method main()I64
    @code
      call.direct IR1, #init

      new.closure default:MyLambda@ref

      spawn IR1, ()I64
      @dead IR1

      call.direct IR1, #wait

      movi.64 IR1, 0x0
      ret.64 IR1
    @end
  @end
@end
