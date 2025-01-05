package bitvecs

class MultiplicationTests extends munit.FunSuite {

  // Unsigned 8-bit tests
  test("Unsigned 8-bit multiplication without overflow") {
    val x: BV[8, false] = 12
    val y: BV[8, false] = 10
    val z = x * y
    val expected: BV[8, false] = 120
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit multiplication with overflow") {
    val x: BV[8, false] = 16
    val y: BV[8, false] = 16
    val z = x * y
    val expected: BV[8, false] = 0 // 256 % 256 = 0
    assertEquals(z, expected)
  }

  // Signed 8-bit tests
  test("Signed 8-bit multiplication without overflow") {
    val x: BV[8, true] = -12
    val y: BV[8, true] = 10
    val z = x * y
    val expected: BV[8, true] = -120
    assertEquals(z, expected)
  }

  test("Signed 8-bit multiplication with overflow") {
    val x: BV[8, true] = 64
    val y: BV[8, true] = 2
    val z = x * y
    val expected: BV[8, true] = -128 // Overflow wraps to the negative range
    assertEquals(z, expected)
  }

  // Unsigned 32-bit tests
  test("Unsigned 32-bit multiplication without overflow") {
    val x: BV[32, false] = 10_000
    val y: BV[32, false] = 1_000
    val z = x * y
    val expected: BV[32, false] = 10_000_000
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit multiplication with overflow") {
    val x: BV[32, false] = 65_536
    val y: BV[32, false] = 65_536
    val z = x * y
    val expected: BV[32, false] = 0 // 2^32 % 2^32 = 0
    assertEquals(z, expected)
  }

  // Signed 32-bit tests
  test("Signed 32-bit multiplication with overflow") {
    val x: BV[32, true] = 46_340
    val y: BV[32, true] = -46_340
    val z = x * y
    val expected: BV[32, true] = -2147395600 // Result fits within signed range
    assertEquals(z, expected)
  }

  // Unsigned 64-bit tests
  test("Unsigned 64-bit multiplication without overflow") {
    val x: BV[64, false] = 4_294_967_296L // 2^32
    val y: BV[64, false] = 2
    val z = x * y
    val expected: BV[64, false] = 8_589_934_592L // 2^33
    assertEquals(z, expected)
  }

  test("Unsigned 64-bit multiplication with overflow") {
    val x: BV[64, false] = 9_223_372_036_854_775_807L // Max unsigned value
    val y: BV[64, false] = 2
    val z = x * y
    val expected: BV[64, false] = BigInt("18446744073709551614") % BigInt("18446744073709551616") // Overflow wraps
    assertEquals(z, expected)
  }

  // Signed 64-bit tests
  test("Signed 64-bit multiplication with overflow") {
    val x: BV[64, true] = -9_223_372_036_854_775_808L // Min signed value
    val y: BV[64, true] = -1
    val z = x * y
    val expected: BV[64, true] = -9_223_372_036_854_775_808L // Wraps back to min value
    assertEquals(z, expected)
  }

  // Unsigned 72-bit tests
  test("Unsigned 72-bit multiplication without overflow") {
    val x: BV[72, false] = BigInt("2361183241434822606848")
    val y: BV[72, false] = BigInt("2")
    val z = x * y
    val expected: BV[72, false] = BigInt("4722366482869645213696")
    assertEquals(z, expected)
  }

  test("Unsigned 72-bit multiplication with overflow") {
    val x: BV[72, false] = BigInt("2361183241434822606848")
    val y: BV[72, false] = BigInt("3")
    val z = x * y
    val expected: BV[72, false] = BigInt("2361183241434822606848")
    assertEquals(z, expected)
  }

  // Intermediate lengths
  test("Unsigned 20-bit multiplication with overflow") {
    val x: BV[20, false] = 1_000
    val y: BV[20, false] = 2_048
    val z = x * y
    val expected: BV[20, false] = 999424
    assertEquals(z, expected)
  }

  test("Signed 37-bit multiplication with wrap-around") {
    val x: BV[37, true] = BigInt("-68719476736") // Min value
    val y: BV[37, true] = BigInt("-1")
    val z = x * y
    val expected: BV[37, true] = BigInt("68719476736") // Wraps to positive max
    assertEquals(z, expected)
  }
}
