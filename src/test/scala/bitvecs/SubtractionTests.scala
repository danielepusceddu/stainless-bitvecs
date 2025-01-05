package bitvecs

class SubtractionTests extends munit.FunSuite {

  // Unsigned 8-bit tests
  test("Unsigned 8-bit subtraction without underflow") {
    val x: BV[8, false] = 10
    val y: BV[8, false] = 5
    val z = x - y
    val expected: BV[8, false] = 5
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit subtraction with underflow") {
    val x: BV[8, false] = 5
    val y: BV[8, false] = 10
    val z = x - y
    val expected: BV[8, false] = 251 // 256 - 10 + 5 = 251
    assertEquals(z, expected)
  }

  // Unsigned 32-bit tests
  test("Unsigned 32-bit subtraction without underflow") {
    val x: BV[32, false] = 1_000_000
    val y: BV[32, false] = 500_000
    val z = x - y
    val expected: BV[32, false] = 500_000
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit subtraction with underflow") {
    val x: BV[32, false] = 0
    val y: BV[32, false] = 1
    val z = x - y
    val expected: BV[32, false] = 4294967295L // 2^32 - 1
    assertEquals(z, expected)
  }

  // Signed 32-bit tests
  test("Signed 32-bit subtraction with overflow") {
    val x: BV[32, true] = -2_147_483_648 // Min value
    val y: BV[32, true] = 1
    val z = x - y
    val expected: BV[32, true] = 2_147_483_647 // Max value
    assertEquals(z, expected)
  }

  test("Signed 32-bit subtraction without overflow") {
    val x: BV[32, true] = -1_000_000
    val y: BV[32, true] = 500_000
    val z = x - y
    val expected: BV[32, true] = -1_500_000
    assertEquals(z, expected)
  }

  // Unsigned 64-bit tests
  test("Unsigned 64-bit subtraction without underflow") {
    val x: BV[64, false] = 9_223_372_036_854_775_807L // Max value
    val y: BV[64, false] = 1
    val z = x - y
    val expected: BV[64, false] = 9_223_372_036_854_775_806L
    assertEquals(z, expected)
  }

  test("Unsigned 64-bit subtraction with underflow") {
    val x: BV[64, false] = 0
    val y: BV[64, false] = 1
    val z = x - y
    val expected: BV[64, false] = BigInt("18446744073709551615") // 2^64 - 1
    assertEquals(z, expected)
  }

  // Signed 64-bit tests
  test("Signed 64-bit subtraction with underflow") {
    val x: BV[64, true] = -9_223_372_036_854_775_808L // Min value
    val y: BV[64, true] = 1
    val z = x - y
    val expected: BV[64, true] = 9_223_372_036_854_775_807L // Max value
    assertEquals(z, expected)
  }

  // Unsigned 72-bit tests
  test("Unsigned 72-bit subtraction without underflow") {
    val x: BV[72, false] = BigInt("4722366482869645213696")
    val y: BV[72, false] = BigInt("2361183241434822606848")
    val z = x - y
    val expected: BV[72, false] = BigInt("2361183241434822606848")
    assertEquals(z, expected)
  }

  test("Unsigned 72-bit subtraction with underflow") {
    val x: BV[72, false] = BigInt("2361183241434822606848")
    val y: BV[72, false] = BigInt("2361183241434822606849")
    val z = x - y
    val expected: BV[72, false] = BigInt("4722366482869645213695") // 2^72 - 1
    assertEquals(z, expected)
  }

  // Intermediate lengths
  test("Unsigned 20-bit subtraction with underflow") {
    val x: BV[20, false] = 0
    val y: BV[20, false] = 1
    val z = x - y
    val expected: BV[20, false] = BigInt("1048575") // 2^20 - 1
    assertEquals(z, expected)
  }

  test("Signed 37-bit subtraction with wrap-around") {
    val x: BV[37, true] = BigInt("-68719476736") // Min value
    val y: BV[37, true] = BigInt("1")
    val z = x - y
    val expected: BV[37, true] = BigInt("68719476735") // Max value
    assertEquals(z, expected)
  }
}
