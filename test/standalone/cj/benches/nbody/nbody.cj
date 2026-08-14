import std.math.*
import std.convert.*
import std.process.*
import std.convert.*


let PI = 3.14159265358979323
let SOLAR_MASS = 4.0 * PI * PI
let DAYS_PER_YEAR = 365.24

class Body{
    public var x : Float64
    public var y : Float64
    public var z : Float64
    public var vx : Float64
    public var vy : Float64
    public var vz : Float64
    public var mass : Float64
    public init(X:Float64,Y:Float64,Z:Float64,VX:Float64,VY:Float64,VZ:Float64,MASS:Float64){
        x=X;y=Y;z=Z;vx=VX;vy=VY;vz=VZ;mass=MASS
    }
}



var bodies : Array<Body> = [
Body( // Sun
        0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SOLAR_MASS
),
Body( // Jupiter
        4.84143144246472090,
        -1.16032004402742839,
        -1.03622044471123109e-01,
        1.66007664274403694e-03 * DAYS_PER_YEAR,
        7.69901118419740425e-03 * DAYS_PER_YEAR,
        -6.90460016972063023e-05 * DAYS_PER_YEAR,
        9.54791938424326609e-04 * SOLAR_MASS
),
Body( // Saturn
        8.34336671824457987,
        4.12479856412430479,
        -4.03523417114321381e-01,
        -2.76742510726862411e-03 * DAYS_PER_YEAR,
        4.99852801234917238e-03 * DAYS_PER_YEAR,
        2.30417297573763929e-05 * DAYS_PER_YEAR,
        2.85885980666130812e-04 * SOLAR_MASS
),
Body( // Uranus
      12.8943695621391310,
      -15.1111514016986312,
      -2.23307578892655734e-01,
      2.96460137564761618e-03 * DAYS_PER_YEAR,
      2.37847173959480950e-03 * DAYS_PER_YEAR,
      -2.96589568540237556e-05 * DAYS_PER_YEAR,
      4.36624404335156298e-05 * SOLAR_MASS
),
Body( // Neptune
      15.3796971148509165,
      -25.9193146099879641,
      1.79258772950371181e-01,
      2.68067772490389322e-03 * DAYS_PER_YEAR,
      1.62824170038242295e-03 * DAYS_PER_YEAR,
      -9.51592254519715870e-05 * DAYS_PER_YEAR,
      5.15138902046611451e-05 * SOLAR_MASS
)
]



func offsetMomentum() {
    var px = 0.0
    var py = 0.0
    var pz = 0.0
    var i = 0
    while (i < bodies.size){
        var t = bodies[i]
        px = t.vx * t.mass + px
        py = t.vy * t.mass + py
        pz = t.vz * t.mass + pz
        i++
    }
	//print("${bodies[0].vx} ${bodies[0].vy} ${bodies[0].vz}\n")
	bodies[0].vx = -px / SOLAR_MASS
	bodies[0].vy = -py / SOLAR_MASS
	bodies[0].vz = -pz / SOLAR_MASS
	//print("${bodies[0].vx} ${bodies[0].vy} ${bodies[0].vz}\n")
}

func energy(): Float64 {
    var (dx, dy, dz, distance) = (0.0, 0.0, 0.0, 0.0)
    var e: Float64 = 0.0
    var i: Int64 = 0
    var bodySize = bodies.size
    while (i < bodySize){
        var bodyi = bodies[i]
        e = 0.5 * bodyi.mass *
            (bodyi.vx * bodyi.vx +
                bodyi.vy * bodyi.vy +
                bodyi.vz * bodyi.vz ) + e
        var j = i+1
        while (j < bodySize) {
            var bodyj = bodies[j]
            dx = bodyi.x - bodyj.x
            dy = bodyi.y - bodyj.y
            dz = bodyi.z - bodyj.z
            distance = sqrt(dx*dx + dy*dy + dz*dz)

            e = e - (bodyi.mass * bodyj.mass) / distance
            j++
        }
        i++
    }
    return e;
}

func advance(dt: Float64) {
    var i = 0
    var j = 0
    while (i < bodies.size) {
        var body = bodies[i]
        j = i + 1
        while (j < bodies.size) {
            var body2 = bodies[j]
            let dx = body.x - body2.x
            let dy = body.y - body2.y
            let dz = body.z - body2.z

            let distance = sqrt(dx*dx + dy*dy + dz*dz)
            let mag = dt / (distance * distance * distance)
            body.vx -= dx * body2.mass * mag
            body.vy -= dy * body2.mass * mag
            body.vz -= dz * body2.mass * mag
            body2.vx += dx * body.mass * mag
            body2.vy += dy * body.mass * mag
            body2.vz += dz * body.mass * mag
            j++
        }
        i++
    }
    i = 0
    while (i < bodies.size) {
        var body = bodies[i]
        body.x += dt * body.vx
        body.y += dt * body.vy
        body.z += dt * body.vz
        i++
    }
}

main(){
    // Original workload: 5000000
    var n = 5000
    offsetMomentum()
    print(energy().format("9.9")+"\n")
    var i = 0
    while (i < n){
        advance(0.01)
        i++
    }
    print(energy().format("9.9")+"\n")
    return 0
}


