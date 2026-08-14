// EXEC: %compiler %cmp_opt %optimization_level_2 %overflow_wrapping %f -o %output

import std.io.*
import std.fs.*
import std.convert.*

var last : UInt32 = 42
let A : UInt32 = 3877
let C : UInt32 = 29573
let M : UInt32 = 139968

//let out = File("/dev/stdout", Write)
let out = File("/dev/null", Write)
let bos = BufferedOutputStream(out)

func rand(): Float64 {
    last = (last * A + C) % M
    return Float64(last) / Float64(M)
}

func getALU(): String {
    let sb = StringBuilder()
    sb.append("GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG")
    sb.append("GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA")
    sb.append("CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT")
    sb.append("ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA")
    sb.append("GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG")
    sb.append("AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC")
    sb.append("AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA")
    return sb.toString()
}

var ALU = getALU()

var IUB_keys : Array<UInt8> = "acgtBDHKMNRSVWY".toArray()

var IUB_values : Array<Float64> = [
    0.27,0.12,0.12,0.27,
    0.02,0.02,0.02,0.02,
    0.02,0.02,0.02,0.02,
    0.02,0.02,0.02
]

var HomoSap_keys : Array<UInt8> = "acgt".toArray()

var HomoSap_values : Array<Float64> = [
    0.3029549426680,
    0.1979883004921,
    0.1975473066391,
    0.3015094502008
]

func makeCumulative(keysSize : Int64, values : Array<Float64>): Array<Float64>{
    var temp: Array<Float64> = values.clone()
    var last : Float64 = 0.0
    var c = 0
    while (c < keysSize){
        var cur = temp[c]
        temp[c] = cur+last
        last = cur+last
        c++
    }
    return temp
}

func fastaRepeat(n_:Int64, seq:String){
    var seqi = 0
    var lenOut = 60
    var seqOut = seq + seq[0..lenOut]
    var n = n_
    while (n > 0) {
        if (n < lenOut) {lenOut = n}
        bos.write(unsafe {(seqOut[seqi..seqi+lenOut]).rawData()})
        bos.write("\n".toArray())
        seqi += lenOut
        if (seqi >= seq.size) {
            seqi -= seq.size
        }
        n -= lenOut
    }
}

func fastaRandom(n_:Int64, keys : Array<UInt8>, values : Array<Float64>){
    var keysSize = keys.size
    var temp = makeCumulative(keysSize, values)
    var width = 60
    var line = Array<UInt8>(width + 1, repeat:0)
    var n = n_
    var lineSize = 60
    while(n > 0){
        if (n < lineSize){ lineSize = n}
        var x = 0
        while (x < lineSize){
            var r = rand()
            var y = 0
            while (y < keysSize){
                if (r < temp[y]){
                    line[x] = keys[y]
                    break
                }
                y++
            }
            x++
        }
        line[lineSize] = UInt8(UInt32(r'\n'))
        bos.write(line.slice(0, lineSize + 1))
        n -= lineSize
    }
}

main(){
    // Original workload: 50000000
    var n = 5000
    print(">ONE Homo sapiens alu\n")
    fastaRepeat(2*n, ALU)

    print(">TWO IUB ambiguity codes\n")
    fastaRandom(3*n, IUB_keys, IUB_values)

    print(">THREE Homo sapiens frequency\n")
    fastaRandom(5*n, HomoSap_keys, HomoSap_values)

    bos.flush()
    out.close()
    0
}

