import aot.*

func test_uint8() {
  logTest(8, false)
  let x: UInt8 = 0x10
  check(x >> 1, 0x8)
  check(x << 1, 0x20)

  let one: UInt8 = 1
  check(one << 7, 0x80)

  let MAX_UINT8: UInt8 = 0xFFu8
  check(MAX_UINT8 >> 1, 0x7F)
  check(MAX_UINT8 >> 4, 0x0F)
  check(MAX_UINT8 >> 7, 0x1)

  check(MAX_UINT8 << 1, 0xFE)
  check(MAX_UINT8 << 4, 0xF0)
  check(MAX_UINT8 << 7, 0x80)
}

func test_uint16() {
  logTest(16, false)
  let x: UInt16 = 0x10
  check(x >> 1, 0x8)
  check(x << 1, 0x20)

  let one: UInt16 = 1
  check(one << 15, 0x8000)

  let MAX_UINT16: UInt16 = 0xFFFFu16
  check(MAX_UINT16 >> 1, 0x7FFF)
  check(MAX_UINT16 >> 8, 0x00FF)
  check(MAX_UINT16 >> 15, 0x1)

  check(MAX_UINT16 << 1, 0xFFFE)
  check(MAX_UINT16 << 8, 0xFF00)
  check(MAX_UINT16 << 15, 0x8000)
}

func test_uint32() {
  logTest(32, false)
  let x: UInt32 = 0x10
  check(x >> 1, 0x8)
  check(x << 1, 0x20)

  let one: UInt32 = 1
  check(one << 31, 0x80000000)

  let MAX_UINT32: UInt32 = 0xFFFFFFFFu32
  check(MAX_UINT32 >> 1, 0x7FFFFFFF)
  check(MAX_UINT32 >> 16, 0xFFFF)
  check(MAX_UINT32 >> 31, 0x1)

  check(MAX_UINT32 << 1, 0xFFFFFFFE)
  check(MAX_UINT32 << 16, 0xFFFF0000)
  check(MAX_UINT32 << 31, 0x80000000)
}

func shiftLeftU64(x: UInt64, y: UInt64): UInt64 {
  return x << y
}

func shiftRightU64(x: UInt64, y: UInt64): UInt64 {
  return x >> y
}

var STATIC_UINT64_X: UInt64 = 0x10 // to avoid constant folding

func test_uint64() {
  logTest(64, false)
  let x: UInt64 = 0x10
  check(x >> 1, 0x8)
  check(x << 1, 0x20)

  let one: UInt64 = 1
  check(one << 63, 0x8000000000000000)

  let MAX_UINT64: UInt64 = 0xFFFFFFFFFFFFFFFFu64
  check(MAX_UINT64 >> 1, 0x7FFFFFFFFFFFFFFF)
  check(MAX_UINT64 >> 32, 0xFFFFFFFF)
  check(MAX_UINT64 >> 63, 0x1)

  check(MAX_UINT64 << 1, 0xFFFFFFFFFFFFFFFE)
  check(MAX_UINT64 << 32, 0xFFFFFFFF00000000)
  check(MAX_UINT64 << 63, 0x8000000000000000)
  
  check(shiftLeftU64(STATIC_UINT64_X, 4u64), 0x100u64)
  check(shiftRightU64(STATIC_UINT64_X, 4u64), 0x1u64)
}

func test_int8() {
  logTest(8, true)
  let x: Int8 = 0x10
  check(x >> 1, 0x8)
  check(x << 1, 0x20)

  let MAX_INT8: Int8 = 127
  check(MAX_INT8 >> 1, 63)
  check(MAX_INT8 >> 4, 7)
  check(MAX_INT8 >> 7, 0)

  check(MAX_INT8 << 1, -2)
  check(MAX_INT8 << 4, -16)
  check(MAX_INT8 << 7, -128)

  let MIN_INT8: Int8 = -128
  check(MIN_INT8 >> 1, -64)
  check(MIN_INT8 >> 4, -8)
  check(MIN_INT8 >> 7, -1)

  check(MIN_INT8 << 1, 0)
  check(MIN_INT8 << 4, 0)
  check(MIN_INT8 << 7, 0)

  let negOne: Int8 = -1
  check(negOne >> 1, -1)
  check(negOne >> 7, -1)
  check(negOne << 1, -2)
  check(negOne << 7, -128)
}

func test_int16() {
  logTest(16, true)
  let x: Int16 = 0x10
  check(x >> 1, 0x8)
  check(x << 1, 0x20)

  let MAX_INT16: Int16 = 32767
  check(MAX_INT16 >> 1, 16383)
  check(MAX_INT16 >> 8, 127)
  check(MAX_INT16 >> 15, 0)

  check(MAX_INT16 << 1, -2)
  check(MAX_INT16 << 8, -256)
  check(MAX_INT16 << 15, -32768)

  let MIN_INT16: Int16 = -32768
  check(MIN_INT16 >> 1, -16384)
  check(MIN_INT16 >> 8, -128)
  check(MIN_INT16 >> 15, -1)

  check(MIN_INT16 << 1, 0)
  check(MIN_INT16 << 8, 0)
  check(MIN_INT16 << 15, 0)

  let negOne: Int16 = -1
  check(negOne >> 1, -1)
  check(negOne >> 15, -1)
  check(negOne << 1, -2)
  check(negOne << 15, -32768)
}

func test_int32() {
  logTest(32, true)
  let x: Int32 = 0x10
  check(x >> 1, 0x8)
  check(x << 1, 0x20)

  let MAX_INT32: Int32 = 2147483647
  check(MAX_INT32 >> 1, 1073741823)
  check(MAX_INT32 >> 16, 32767)
  check(MAX_INT32 >> 31, 0)

  check(MAX_INT32 << 1, -2)
  check(MAX_INT32 << 16, -65536)
  check(MAX_INT32 << 31, -2147483648)

  let MIN_INT32: Int32 = -2147483648
  check(MIN_INT32 >> 1, -1073741824)
  check(MIN_INT32 >> 16, -32768)
  check(MIN_INT32 >> 31, -1)

  check(MIN_INT32 << 1, 0)
  check(MIN_INT32 << 16, 0)
  check(MIN_INT32 << 31, 0)

  let negOne: Int32 = -1
  check(negOne >> 1, -1)
  check(negOne >> 31, -1)
  check(negOne << 1, -2)
  check(negOne << 31, -2147483648)
}

func shiftLeftI64(x: Int64, y: Int64): Int64 {
  return x << y
}

func shiftRightI64(x: Int64, y: Int64): Int64 {
  return x >> y
}

var STATIC_INT64_X: Int64 = 0x10 // to avoid constant folding

func test_int64() {
  logTest(64, true)
  let x: Int64 = 0x10
  check(x >> 1, 0x8)
  check(x << 1, 0x20)

  let MAX_INT64: Int64 = 9223372036854775807
  check(MAX_INT64 >> 1, 4611686018427387903)
  check(MAX_INT64 >> 32, 2147483647)
  check(MAX_INT64 >> 63, 0)

  check(MAX_INT64 << 1, -2)
  check(MAX_INT64 << 32, -4294967296)
  check(MAX_INT64 << 63, -9223372036854775808)

  let MIN_INT64: Int64 = -9223372036854775808
  check(MIN_INT64 >> 1, -4611686018427387904)
  check(MIN_INT64 >> 32, -2147483648)
  check(MIN_INT64 >> 63, -1)

  check(MIN_INT64 << 1, 0)
  check(MIN_INT64 << 32, 0)
  check(MIN_INT64 << 63, 0)

  let negOne: Int64 = -1
  check(negOne >> 1, -1)
  check(negOne >> 63, -1)
  check(negOne << 1, -2)
  check(negOne << 63, -9223372036854775808)

  check(shiftLeftI64(STATIC_INT64_X, 4i64), 0x100i64)
  check(shiftRightI64(STATIC_INT64_X, 4i64), 0x1i64)
}

main(): Int64 {
  test_uint8()
  test_uint16()
  test_uint32()
  test_uint64()
  test_int8()
  test_int16()
  test_int32()
  test_int64()
  0
}