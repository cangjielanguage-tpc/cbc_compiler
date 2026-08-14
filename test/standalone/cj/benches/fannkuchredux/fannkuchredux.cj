import std.math.*
import std.sync.*
import std.convert.*
import std.collection.*

let MAX_N = 16
let CHUNKS_NUM = 720

// Original workload: 12
var n = 8
var taskNum = 0
var chunkSize = 0
var factorial = Array<Int64>()

// accumulated results.
var result = AtomicInt64(0)
let check = AtomicInt64(0)

main() {
	//n = Int64.tryParse(args[args.size-1]) ?? Int64(12)
	factorial = Array<Int64>(n + 1, repeat: 0)
	factorial[0] = 1
	for (i in 1..factorial.size) {
		factorial[i] = factorial[i - 1] * i
	}
	chunkSize = (factorial[n] + CHUNKS_NUM - 1) / CHUNKS_NUM
	chunkSize += chunkSize % 2
	taskNum = (factorial[n] + chunkSize - 1) / chunkSize

  let latch = SyncCounter(factorial[n] / chunkSize)
	//let threads = ArrayList<Future<Unit>>()
	var idxMin = 0
	while (idxMin < factorial[n]) {
		let idx = idxMin
		spawn {
       fannkuch(idx)
       latch.dec()
    }
		//threads.add(future)
		idxMin += chunkSize
	}
  latch.waitUntilZero()

	//let threads = ArrayList<Future<Unit>>()
	//var idxMin = 0
	//while (idxMin < factorial[n]) {
	//	let idx = idxMin
	//	let future = spawn { fannkuch(idx) }
	//	threads.add(future)
	//	idxMin += chunkSize
	//}

	//for (thread in threads) {
	//	thread.get()
	//}

	printResult(n, result.load(), check.load())
  0
}

func printResult(n: Int64, result: Int64, check: Int64) {
	println("${check}\nPfannkuchen(${n}) = ${result}")
}

@OverflowWrapping
func fannkuch(idxMin: Int64) {
	let idxMax = min(idxMin + chunkSize, factorial[n])

	let p = Array<Int64>(MAX_N, repeat: 0)
	let pp = Array<Int64>(MAX_N, repeat: 0)
	let count = Array<Int64>(MAX_N, repeat: 0)

	for (i in 0..n) {
		p[i] = i
	}

	var i = n - 1
	var idx = idxMin
	while (i > 0) {
		var d = idx / factorial[i]
		count[i] = d
		idx = idx % factorial[i]

		for (x in 0..n) { pp[x] = p[x] }

		var j = 0
		while (j <= i) {
			if (j + d <= i) {
				p[j] = pp[j + d]
			} else {
				p[j] = pp[j + d - i - 1]
			}
			j++
		}
		i--
	}

	var maxFlips: Int64 = 1
	var checkSum: Int64 = 0

	idx = idxMin
	var sign = true

	while (true) {
		// Count flips.
		var first = p[0]
		if (first != 0) {
			var flips = 1
			if (p[first] != 0) {
				for (x in 0..n) { pp[x] = p[x] }

				var p0 = first
				do {
					flips++
					var i = 1
					var j = p0 - 1
					while (i < j) {
						var t = pp[i]
						pp[i] = pp[j]
						pp[j] = t

						i++
						j--
					}
					var t = pp[p0]
					pp[p0] = p0
					p0 = t
				} while (pp[p0] != 0)
			}

			if (maxFlips < flips) {
				maxFlips = flips
			}

			if (sign) {
				checkSum += flips
			} else {
				checkSum -= flips
			}
		}

		idx++
		if (idx == idxMax) {
			break
		}

		// Next permutation.
		if (sign) {
			p[0] = p[1]
			p[1] = first
		} else {
			var t = p[1]
			p[1] = p[2]
			p[2] = t

			var k = 2
			while (true) {
				count[k] += 1
				if (count[k] <= k) {
					break
				}
				count[k] = 0
				var j = 0
				while (j <= k) {
				  p[j] = p[j + 1]
				  j++
				}
				p[k + 1] = first
				first = p[0]
				k++
			}
		}

		sign = !sign
	}

	check.fetchAdd(checkSum)
	var newFlips = maxFlips
	while (newFlips > result.load()) {
		newFlips = result.swap(newFlips)
	}
}


