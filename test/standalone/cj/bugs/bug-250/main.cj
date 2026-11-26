let var_1701381094782_18 : Bool = (Int64(-18) is Int16)
public let var_1701381094782_86 : UInt32 = UInt32(136)
public var var_1701381094782_205 : UInt32 = UInt32(170)
var var_1701381094782_222 : UInt8 = UInt8(248)
let var_1701381094782_235 : Class_1701381094782_31 = Class_1701381094782_31()
public open class Class_1701381094782_31 {
    let var_1701381094782_1043 : Int = Int64(-28)
}
sealed public interface Interface_1701381094782_621<T0> {
}
public interface Interface_1701381094782_623 <: Interface_1701381094782_621<Range<Int8>> {
}
public interface Interface_1701381094782_622 <: Interface_1701381094782_621<Range<Int8>> & Interface_1701381094782_623 {
}
sealed public interface Interface_1701381094782_620<T0, T1> <: Interface_1701381094782_621<Range<Int8>> & Interface_1701381094782_622 {
}
public interface Interface_1701381094782_619<T0, T1, T2> <: Interface_1701381094782_620<Range<UInt16>, Range<Int64>> & Interface_1701381094782_623 {
}
sealed public interface Interface_1701381094782_618 <: Interface_1701381094782_619<VArray<UInt32, $6>, Option<UInt8>, Array<UInt64>> {
}
public interface Interface_1701381094782_617 <: Interface_1701381094782_618 {
}
sealed public interface Interface_1701381094782_616 <: Interface_1701381094782_617 & Interface_1701381094782_623 {
}
public open class Class_1701381094782_613 <: Class_1701381094782_31 & Interface_1701381094782_616 {
}
sealed public interface Interface_1701381094782_634<T0> <: Interface_1701381094782_620<Range<UInt16>, Range<Int64>> {
}
public open class Class_1701381094782_631 <: Class_1701381094782_613 & Interface_1701381094782_634<Array<UInt64>> & Interface_1701381094782_618 {
}
public func func_1701381094782_132(param_1701381094782_133 : Rune) : (UInt8, UInt32, UInt32, Class_1701381094782_31, Int16, String) { // block id: 134
    func func_1701381094782_374(namedParam_1701381094782_375 ! : Float32) : UInt8 { // block id: 376
        try { // block id: 381
            (((UInt16(UInt16(17))&127)..=(UInt16(UInt16(248))&127):(Int64(-82)|1)), ((UInt64(UInt64(196))&127)..=(UInt64(UInt64(69))&127):(Int64(-104)|1)), ((Int64(Int64(124))&127)..(Int64(Int64(-116))&127):(Int64(67)|1)), var_1701381094782_222, var_1701381094782_222)
        } finally { // block id: 393
        }[3] << ((UInt8(try { // block id: 425
            Int8(124) >> ((Int8(-21)) & 7)
        } finally { // block id: 431
        })) & 7)
    }
    var var_1701381094782_762 : Float64 = Float64(-63.0);
    let var_1701381094782_816 : (UInt8, UInt32, UInt32, Class_1701381094782_31, Int16, String) = (UInt8(98), UInt32(185), UInt32(224), Class_1701381094782_631(), Int16(7), "0o(F;H}V,y");
    let var_1701381094782_869 : UInt16 = UInt16(51);
    match (try { // block id: 803
        var_1701381094782_762 = Float64(30.0);
    } catch (_) { // block id: 809
    }) {
        case _ => 
            VArray<UInt16, $1>(repeat: var_1701381094782_869)
    }[0];
    (((Int16(-47) + Int16(27)) as (UInt8, UInt32, UInt32, Class_1701381094782_31, Int16, String)) ?? var_1701381094782_816)
}
main(): Int64 {
    try {
        var loopTimeVar_1701381094782_40 : Int = 0;
        do {
        } while ((try { // block id: 6
            let var_1701381094782_13 : Bool = false;
            var_1701381094782_13
        } catch (_) { // block id: 15
            var_1701381094782_18
        }) && loopTimeVar_1701381094782_40 < 100 ) ;
        func func_1701381094782_46(optionalParam_1701381094782_47 ! : UInt32 = UInt32(112), optionalParam_1701381094782_49 ! : Class_1701381094782_31 = Class_1701381094782_31(), namedParam_1701381094782_51 ! : UInt32) : Class_1701381094782_31 { // block id: 52
            try { // block id: 54
                let var_1701381094782_883 : (Rune) -> (UInt8, UInt32, UInt32, Class_1701381094782_31, Int16, String) = func_1701381094782_132;
            } finally { // block id: 963
            };
            var_1701381094782_235
        }
        func func_1701381094782_42<T0, T1>() : Int64 where T1 <: Class_1701381094782_631 & Class_1701381094782_613 { // block id: 43
            func_1701381094782_46(namedParam_1701381094782_51 : if (true) { // block id: 1022
                UInt32(108) ^ UInt32(35)
            } else { // block id: 1038
                var_1701381094782_86 >> ((var_1701381094782_205) & 7)
            }).var_1701381094782_1043
        }
    } catch (e: NoneValueException) { println("Exception: NoneValueException") }
    0;
}
