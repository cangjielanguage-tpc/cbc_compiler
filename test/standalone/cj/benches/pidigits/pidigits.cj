//package pidigits

import std.math.numeric.*
import std.process.*
import std.convert.*
import std.collection.*

var n = 0
var silent = true

var tmp1 =  BigInt(0)
var tmp2 =  BigInt(0)
var y2 =    BigInt(1)
var bigk =  BigInt(0)
var accum = BigInt(0)
var denom = BigInt(1)
var numer = BigInt(1)
var ten =   BigInt(10)
var three = BigInt(3)
var four =  BigInt(4)



func next_term(k_: Int64): Int64{
    var k = k_
    while (true){
        k++
        y2 = BigInt(k * 2 + 1)
        bigk = BigInt(k)
        tmp1 = numer << 1
        accum = accum + tmp1
        accum = accum * y2
        denom = denom * y2
        numer = numer * bigk

        if (accum > numer){
            return k
        }
    }
    return 0 // only for type check.
}

func extract_digit(nth:BigInt): Int64{
    tmp1 = nth * numer
    tmp2 = tmp1 + accum
    tmp1 = tmp2 / denom
    return tmp1.toInt64()
}

func next_digit(k_: Int64): (Int64,Int64) {
    var k = k_
    while (true){
        k = next_term(k)
        var d3 = extract_digit(three)
        var d4 = extract_digit(four)
        if (d3 == d4){
            return (d3, k)
        }
    }
    return (0,0) // only for type check
}

func eliminate_digit(d: Int64){
    tmp1 = BigInt(d)
    accum = accum - denom * tmp1
    accum = accum * ten
    numer = numer * ten
}


main(){
    // Original workload: 10000
    n = 5000
    var line = StringBuilder()
    var d : Int64 = 0
    var k : Int64 = 0
    for (i in 1..=n) {
        var (d_,k_) = next_digit(k)
        d = d_; k = k_
        line.append(Rune(UInt32(d)+ UInt32(48)))  // 48 = UInt32('0')
        if (line.size == 10) {
            if (silent != true) {
                 print(line)
                 print("\t")
                 println(i)

            }
            line.reset()
        }
        eliminate_digit(d)
    }
    if ((line.size > 0) && (silent != true)) {
        print(line)
        print("\t")
        println(n)
    }
    0
}
