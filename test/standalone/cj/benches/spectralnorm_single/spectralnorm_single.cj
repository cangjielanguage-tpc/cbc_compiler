// EXEC: %compiler %cmp_opt %optimization_level_2 %overflow_wrapping %f -o %output
import std.math.*
import std.convert.*
import std.process.*
import std.convert.*

func A(i: Int64, j: Int64): Int64 {
    return ((i + j) * (i + j + 1) / 2 + i + 1)
}

func Au(u:Array<Float64>,v:Array<Float64>){
    for (i in 0..u.size){
        var t : Float64 = 0.0
        for (j in 0..u.size){
            t += u[j] / Float64(A(i, j))
        }
        v[i] = t
    }
}

func Atu(u:Array<Float64>,v:Array<Float64>){
    for (i in 0..u.size){
        var t : Float64 = 0.0
        for (j in 0..u.size){
            t += u[j] / Float64(A(j, i))
        }
        v[i] = t
    }
}


func AtAu(u:Array<Float64>,v:Array<Float64>,w:Array<Float64>){
    Au(u,w)
    Atu(w,v)
}

func spectralnorm(n:Int64){
    var u = Array<Float64>(n, repeat:1.0)
    var v = Array<Float64>(n, repeat:0.0)
    var w = Array<Float64>(n, repeat:0.0)
    var vv = 0.0
    var vBv = 0.0
    for (i in 0..10){
        AtAu(u,v,w)
        AtAu(v,u,w)
    }
    for (i in 0..n){
        vBv += u[i] * v[i]
        vv += v[i] * v[i]
    }
    return sqrt(vBv/vv)
}

main(){
    // Original workload: 5500
    var n = 50
    //var args = Process.current.arguments
    //if (args.size > 0) {
    //    n = Int64.tryParse(args[args.size-1]).getOrThrow()
    //}
    print(spectralnorm(n).format("9.9").toString())
    println()
    0
}
