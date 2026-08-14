// EXEC: %compiler %cmp_opt %optimization_level_2 %overflow_wrapping %f -o %output

import std.io.*
import std.process.*
import std.fs.*
import std.convert.*

let LIMIT = 4.0
let ITER = 50
var yAt: Int64 = -1
// Original workload: 10000
var size: Int64 = 100
var bytesPerRow: Int64 = 0

//var f: File = File("/dev/stdout", Write)
var f: File = File("/dev/null", Write)
var bos: BufferedOutputStream<File> = BufferedOutputStream(f)

@OverflowWrapping
func computeRow(y: Int64,field: Array<UInt8>, initial_r: Array<Float64>, initial_i: Array<Float64>) {
    var (i, j, xb): (Int64,Int64,Int64)
    var (res, b): (UInt8,UInt8)
    var (Zr1, Zr2, Zi1, Zi2, Tr1, Tr2, Ti1, Ti2, Cr1, Cr2): (Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64,Float64)

    var x = 0
    while (x < bytesPerRow) {
        res = 0
        var Ci = initial_i[y]
        i = 0
        while (i < 8) {
            xb = x<<3
            Cr1 = initial_r[xb+i]
            Cr2 = initial_r[xb+i+1]
            Zr1=Cr1
            Zi1=Ci
            Zr2=Cr2
            Zi2=Ci
            b = 0
            j = 0
            while (j < ITER) {
                Tr1 = Zr1 * Zr1
                Ti1 = Zi1 * Zi1
                Zi1 = 2.0 * Zr1 * Zi1 + Ci
                Zr1 = Tr1 - Ti1 + Cr1

                if (Tr1 + Ti1 > LIMIT) {
                    b |= 2
                    if (b == 3) {
                        break
                    }
                }

                Tr2 = Zr2 * Zr2
                Ti2 = Zi2 * Zi2
                Zi2 = 2.0 * Zr2 * Zi2 + Ci
                Zr2 = Tr2 - Ti2 + Cr2

                if (Tr2 + Ti2 > LIMIT) {
                    b |= 1
                    if (b == 3) {
                        break
                    }
                }
                j++
            }
            res = (res << 2) | b
            i+=2
        }
        field[x] = !res
        x++
    }
    bos.write(field.slice(0, bytesPerRow))
}

main(){
    bytesPerRow = size >> 3
    var field = Array<UInt8>(bytesPerRow, repeat:0)
    var initial_r = Array<Float64>(size, repeat:0.0)
    var initial_i = Array<Float64>(size, repeat:0.0)
    var inv = 2.0 / Float64(size)
    for (xy in 0..size) {
        var i = inv * Float64(xy)
        initial_r[xy] = i - 1.5
        initial_i[xy] = i - 1.0
    }
    bos.write(unsafe {"P4\n${size} ${size}\n".rawData()})
    var y = 0
    while (y < size) {
        computeRow(y, field, initial_r, initial_i)
        y++
    }
    bos.flush()
    f.close()
    return 0
}
