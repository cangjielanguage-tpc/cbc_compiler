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

      ; ---- vcmp.32 / vidx.32 over U8 arrays ----
      ; allocate two 40-byte arrays so that reads at byte offsets 0 and 1
      ; stay within the array payload
      movi.64 IR2, 40
      newarr @arr[U8]
      mov.ref IR4, IR1
      @dead IR1, IR2
      movi.64 IR2, 40
      newarr @arr[U8]
      mov.ref IR7, IR1
      @dead IR1, IR2

      ; fill both arrays with arr[i] = i (value and index share a register)
      movi.64 IR5, 0
fill:
      st.arr.8 IR5, IR4, IR5
      st.arr.8 IR5, IR7, IR5
      addi.64 IR5, IR5, 1
      bcci.64 LT, IR5, 40, fill
      @dead IR5

      ; equal blocks -> 32
      vcmp.32 IR8, IR4, IRZ, IR7, IRZ
      call.direct #println, IRZ, IR8       ; 32
      @dead IR8

      ; arrA[0..31] vs arrA[1..32] differ at index 0 -> 0
      movi.64 IR6, 1
      vcmp.32 IR8, IR4, IRZ, IR4, IR6
      call.direct #println, IRZ, IR8       ; 0
      @dead IR8, IR6

      ; search hit: arrA[10] = 10 -> 10
      movi.64 IR6, 10
      vidx.32 IR8, IR4, IRZ, IR6
      call.direct #println, IRZ, IR8       ; 10
      @dead IR8, IR6

      ; search miss: 200 is not in arrA -> 32
      movi.64 IR6, 200
      vidx.32 IR8, IR4, IRZ, IR6
      call.direct #println, IRZ, IR8       ; 32
      @dead IR8, IR6, IR4, IR7

      movi.64 IR1, 0
      ret.64 IR1
    @end
  @end
@end
