;strict
@main_type "default"

; TODO add variants of memexprs with copying when they are supported

@aot_deps "aot"

; section: fields info

@aot.instance Basic_x_idx = 0

@aot.instance L1a_f1_idx = 0
@aot.instance L1b_f1_idx = 0
@aot.instance L2a_f1_idx = 0
@aot.instance L2a_f2_idx = 1
@aot.instance L2b_f1_idx = 0
@aot.instance L2b_f2_idx = 1
@aot.instance L3_f1_idx  = 0
@aot.instance L3_f2_idx  = 1
@aot.instance L4_f1_idx  = 0
@aot.instance L4_f2_idx  = 1
@aot.instance L4_f3_idx  = 2

@aot.instance L4C_f1_idx = 0
@aot.instance L4C_f2_idx = 1
@aot.instance L4C_f3_idx = 2


@type std.core:Object
  @flags PUBLIC AOT
@end

@type aot:Basic
  @flags PUBLIC AOT
  @super std.core:Object@aref

  @field x I64
    @flags PUBLIC
  @end
  @field y std.core:Option[aot:Basic@aref]@nopt
    @flags PUBLIC
  @end
@end

@type aot:L1a
  @flags PUBLIC AOT RECORD
  @field f1 I64
    @flags PUBLIC
  @end
@end
@type aot:L1b
  @flags PUBLIC AOT RECORD
  @field f1 aot:Basic@aref
    @flags PUBLIC
  @end
@end

@type aot:L2a
  @flags PUBLIC AOT RECORD
  @field f1 aot:Basic@aref
    @flags PUBLIC
  @end
  @field f2 aot:L1a@arec
    @flags PUBLIC
  @end
@end
@type aot:L2b
  @flags PUBLIC AOT RECORD
  @field f1 I64
    @flags PUBLIC
  @end
  @field f2 aot:L1b@arec
    @flags PUBLIC
  @end
@end

@type aot:L3
  @flags PUBLIC AOT RECORD
  @field f1 aot:L2a@arec
    @flags PUBLIC
  @end
  @field f2 aot:L2b@arec
    @flags PUBLIC
  @end
@end

@type aot:L4
  @flags PUBLIC AOT RECORD
  @field f1 I64
    @flags PUBLIC
  @end
  @field f2 I64
    @flags PUBLIC
  @end
  @field f3 aot:L3@arec
    @flags PUBLIC
  @end
@end

@type aot:L4c
  @flags PUBLIC AOT
  @super std.core:Object@aref
  @field f1 I64
    @flags PUBLIC
  @end
  @field f2 I64
    @flags PUBLIC
  @end
  @field f3 aot:L4@arec
    @flags PUBLIC
  @end
@end

@field_ref Basic_x = aot:Basic@aref x I64 #Basic_x_idx

@field_ref L1a_f1 = aot:L1a@arec f1 I64            #L1a_f1_idx
@field_ref L1b_f1 = aot:L1b@arec f1 aot:Basic@aref #L1b_f1_idx
@field_ref L2a_f1 = aot:L2a@arec f1 aot:Basic@aref #L2a_f1_idx
@field_ref L2a_f2 = aot:L2a@arec f2 aot:L1a@arec   #L2a_f2_idx
@field_ref L2b_f1 = aot:L2b@arec f1 I64            #L2b_f1_idx
@field_ref L2b_f2 = aot:L2b@arec f2 aot:L1b@arec   #L2b_f2_idx
@field_ref L3_f1  = aot:L3@arec  f1 aot:L2a@arec   #L3_f1_idx
@field_ref L3_f2  = aot:L3@arec  f2 aot:L2b@arec   #L3_f2_idx
@field_ref L4_f1  = aot:L4@arec  f1 I64            #L4_f1_idx
@field_ref L4_f2  = aot:L4@arec  f2 I64            #L4_f2_idx
@field_ref L4_f3  = aot:L4@arec  f3 aot:L3@arec    #L4_f3_idx

@field_ref L4C_f1 = aot:L4C@aref f1 I64         #L4C_f1_idx
@field_ref L4C_f2 = aot:L4C@aref f2 I64         #L4C_f2_idx
@field_ref L4C_f3 = aot:L4C@aref f3 aot:L4@arec #L4C_f3_idx

@field_ref basic_ref     = default@ref basic     aot:Basic@aref
@field_ref staticRec_ref = default@ref staticRec aot:L4@arec
@field_ref staticObj_ref = default@ref staticObj aot:L4C@aref

; section: methods info

@aot.direct checkObjLnk = "_CN3aot8checkObjHCNY_5BasicE"
@method_ref aot.checkObj = aot@aref checkObj(aot:Basic@aref)I64 #checkObjLnk

@aot.direct checkPrimLnk = "_CN3aot9checkPrimHll"
@method_ref aot.checkPrim = aot@aref checkPrim(I64, I64)I64 #checkPrimLnk

@method_ref default.prepare                 = default@ref prepare()I64
@method_ref default.testStoreRegHead        = default@ref testStoreRegHead()I64
@method_ref default.testLoadRegHead         = default@ref testLoadRegHead()I64
@method_ref default.testStoreStaticHead     = default@ref testStoreStaticHead()I64
@method_ref default.testLoadStaticHead      = default@ref testLoadStaticHead()I64
@method_ref default.testStoreLoadTypedHead  = default@ref testStoreLoadTypedHead()I64

; section: types

@type default
    @field basic aot:Basic@aref
        @flags STATIC
    @end

    @field staticRec aot:L4@arec
        @flags STATIC
    @end

    @field staticObj aot:L4C@aref
        @flags STATIC
    @end

    @method prepare()I64
        @code
            newobj aot:L4C@aref
            st.static IR1, #staticObj_ref
            @dead IR1

            movi.64 IR2, 42
            newobj aot:Basic@aref
            st.ref.field IR2, IR1, #Basic_x
            st.static IR1, #basic_ref
            @dead IR1, IR2

            movi.64 IR1, 0
            ret.64 IR1
        @end
    @end

    @method testStoreRegHead()I64
        @code
            ld.static IR1, #staticObj_ref
            movi.64 IR2, 42
            ld.static IR3, #basic_ref

            ; Filling level 4 container

            ; memexpr {
            ms.hd.obj IR1
            ms.field #L4C_f1
            ms.st.imm 4
            ; }

            ; memexpr {
            ms.hd.obj IR1
            ms.field #L4C_f2
            ms.st IR2
            ; }

            ; Filling level 4

            ; memexpr {
            ms.hd.obj IR1
            ms.fseq #L4C_f3, #L4_f1
            ms.st.imm 4
            ; }

            ; memexpr {
            ms.hd.obj IR1
            ms.fseq #L4C_f3, #L4_f2
            ms.st IR2
            ; }

            ; Filling level 2

            ; memexpr {
            ms.hd.obj IR1
            ms.fseq #L4C_f3, #L4_f3, #L3_f1, #L2a_f1
            ms.st IR3
            ; }

            ; memexpr {
            ms.hd.obj IR1
            ms.fseq #L4C_f3, #L4_f3, #L3_f2, #L2b_f1
            ms.st.imm 2
            ; }

            ; Filling level 1

            ; memexpr {
            ms.hd.obj IR1
            ms.fseq #L4C_f3, #L4_f3, #L3_f1, #L2a_f2, #L1a_f1
            ms.st.imm 1
            ; }

            ; memexpr {
            ms.hd.obj IR1
            ms.fseq #L4C_f3, #L4_f3, #L3_f2, #L2b_f2, #L1b_f1
            ms.st IR3
            ; }

            @dead IR1, IR2, IR3
            movi.64 IR1, 1
            ret.64 IR1
        @end
    @end

    @method testLoadRegHead()I64
        @code
            ld.static IR8, #staticObj_ref

            ; Reading level 4 container

            ; memexpr {
            ms.hd.obj IR8
            ms.field #L4C_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 4
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; memexpr {
            ms.hd.obj IR8
            ms.field #L4C_f2
            ms.ld IR1
            ; }
            movi.64 IR2, 42
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; Reading level 4

            ; memexpr {
            ms.hd.obj IR8
            ms.fseq #L4C_f3, #L4_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 4
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; memexpr {
            ms.hd.obj IR8
            ms.fseq #L4C_f3, #L4_f2
            ms.ld IR1
            ; }
            movi.64 IR2, 42
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; Reading level 2

            ; memexpr {
            ms.hd.obj IR8
            ms.fseq #L4C_f3, #L4_f3, #L3_f1, #L2a_f1
            ms.ld IR1
            ; }
            call.direct IR1, #aot.checkObj
            @dead IR1

            ; memexpr {
            ms.hd.obj IR8
            ms.fseq #L4C_f3, #L4_f3, #L3_f2, #L2b_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 2
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; Reading level 1

            ; memexpr {
            ms.hd.obj IR8
            ms.fseq #L4C_f3, #L4_f3, #L3_f1, #L2a_f2, #L1a_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 1
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; memexpr {
            ms.hd.obj IR8
            ms.fseq #L4C_f3, #L4_f3, #L3_f2, #L2b_f2, #L1b_f1
            ms.ld IR1
            ; }
            call.direct IR1, #aot.checkObj
            @dead IR1

            @dead IR8
            movi.64 IR1, 1
            ret.64 IR1
        @end
    @end

    @method testStoreStaticHead()I64
        @code
            movi.64 IR2, 42
            ld.static IR3, #basic_ref

            ; Filling level 4

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f1
            ms.st.imm 4
            ; }

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f2
            ms.st IR2
            ; }

            ; Filling level 2

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f3, #L3_f1, #L2a_f1
            ms.st IR3
            ; }

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f3, #L3_f2, #L2b_f1
            ms.st.imm 2
            ; }

            ; Filling level 1

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f3, #L3_f1, #L2a_f2, #L1a_f1
            ms.st.imm 1
            ; }

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f3, #L3_f2, #L2b_f2, #L1b_f1
            ms.st IR3
            ; }

            @dead IR2, IR3

            movi.64 IR1, 1
            ret.64 IR1
        @end
    @end

    @method testLoadStaticHead()I64
        @code
            ; Reading level 4

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 4
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f2
            ms.ld IR1
            ; }
            movi.64 IR2, 42
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; Reading level 2

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f3, #L3_f1, #L2a_f1
            ms.ld IR1
            ; }
            call.direct IR1, #aot.checkObj
            @dead IR1

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f3, #L3_f2, #L2b_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 2
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; Reading level 1

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f3, #L3_f1, #L2a_f2, #L1a_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 1
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; memexpr {
            ms.hd.static #staticRec_ref
            ms.fseq #L4_f3, #L3_f2, #L2b_f2, #L1b_f1
            ms.ld IR1
            ; }
            call.direct IR1, #aot.checkObj
            @dead IR1

            movi.64 IR1, 1
            ret.64 IR1
        @end
    @end

    @method testStoreLoadTypedHead()I64
        @typed_slots aot:L4@arec
        @code
            zero.refs $0
            movi.64 IR2, 42
            ld.static IR3, #basic_ref

            ; Filling level 4

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f1
            ms.st.imm 4
            ; }

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f2
            ms.st IR2
            ; }

            ; Filling level 2

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f3, #L3_f1, #L2a_f1
            ms.st IR3
            ; }

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f3, #L3_f2, #L2b_f1
            ms.st.imm 2
            ; }

            ; Filling level 1

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f3, #L3_f1, #L2a_f2, #L1a_f1
            ms.st.imm 1
            ; }

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f3, #L3_f2, #L2b_f2, #L1b_f1
            ms.st IR3
            ; }

            @dead IR2, IR3

            ; Reading level 4

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 4
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f2
            ms.ld IR1
            ; }
            movi.64 IR2, 42
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; Reading level 2

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f3, #L3_f1, #L2a_f1
            ms.ld IR1
            ; }
            call.direct IR1, #aot.checkObj
            @dead IR1

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f3, #L3_f2, #L2b_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 2
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; Reading level 1

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f3, #L3_f1, #L2a_f2, #L1a_f1
            ms.ld IR1
            ; }
            movi.64 IR2, 1
            call.direct IR1, #aot.checkPrim
            @dead IR1, IR2

            ; memexpr {
            ms.hd.typed $0
            ms.fseq #L4_f3, #L3_f2, #L2b_f2, #L1b_f1
            ms.ld IR1
            ; }
            call.direct IR1, #aot.checkObj
            @dead IR1

            movi.64 IR1, 1
            ret.64 IR1
        @end
    @end

    @method main()I64
        @saved_iregs IR11
        @code
            movi.64 IR11, 0

            call.direct IR1, #default.prepare

            call.direct IR1, #default.testStoreRegHead
            @live.prim IR1
            add.64 IR11, IR11, IR1
            @dead IR1

            call.direct IR1, #default.testLoadRegHead
            @live.prim IR1
            add.64 IR11, IR11, IR1
            @dead IR1

            call.direct IR1, #default.testStoreStaticHead
            @live.prim IR1
            add.64 IR11, IR11, IR1
            @dead IR1

            call.direct IR1, #default.testLoadStaticHead
            @live.prim IR1
            add.64 IR11, IR11, IR1
            @dead IR1

            call.direct IR1, #default.testStoreLoadTypedHead
            @live.prim IR1
            add.64 IR11, IR11, IR1
            @dead IR1

            ret.64 IR11
        @end
    @end
@end
