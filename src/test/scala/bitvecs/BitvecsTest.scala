package bitvecs

class BitvecsSuite extends munit.FunSuite {

  // Unsigned 8-bit tests
  test("Unsigned 8-bit addition without overflow") {
    val x: BV[8, false] = 10
    val y: BV[8, false] = 20
    val z = x + y
    val expected: BV[8, false] = 30
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit addition with overflow") {
    val x: BV[8, false] = 254
    val y: BV[8, false] = 3
    val z = x + y
    val expected: BV[8, false] = 1
    assertEquals(z, expected)
  }

  // Signed 8-bit tests
  test("Signed 8-bit addition with overflow") {
    val x: BV[8, true] = 127
    val y: BV[8, true] = 1
    val z = x + y
    val expected: BV[8, true] = -128
    assertEquals(z, expected)
  }

  test("Signed 8-bit subtraction with underflow") {
    val x: BV[8, true] = -128
    val y: BV[8, true] = 1
    val z = x - y
    val expected: BV[8, true] = 127
    assertEquals(z, expected)
  }

  // Unsigned 32-bit tests
  test("Unsigned 32-bit addition without overflow") {
    val x: BV[32, false] = 100000
    val y: BV[32, false] = 200000
    val z = x + y
    val expected: BV[32, false] = 300000
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit addition with overflow") {
    val x: BV[32, false] = 0xFFFFFFFE
    val y: BV[32, false] = 3
    val z = x + y
    val expected: BV[32, false] = 1
    assertEquals(z, expected)
  }

  // Signed 32-bit tests
  test("Signed 32-bit addition with overflow") {
    val x: BV[32, true] = 2147483647 // Max positive value
    val y: BV[32, true] = 1
    val z = x + y
    val expected: BV[32, true] = -2147483648 // Min negative value
    assertEquals(z, expected)
  }

  // Unsigned 64-bit tests
  test("Unsigned 64-bit addition without overflow") {
    val x: BV[64, false] = 9223372036854775807L // Large value
    val y: BV[64, false] = 1
    val z = x + y
    val expected: BV[64, false] = BigInt("9223372036854775808")
    assertEquals(z, expected)
  }

  // Unsigned 72-bit tests
  test("Unsigned 72-bit addition without overflow") {
    val x: BV[72, false] = BigInt("4722366482869645213695")
    val y: BV[72, false] = 1
    val z = x + y
    val expected: BV[72, false] = BigInt("4722366482869645213696")
    assertEquals(z, expected)
  }

  // Signed 72-bit tests
  test("Signed 72-bit subtraction with underflow") {
    val x: BV[72, true] = BigInt("-2361183241434822606848")
    val y: BV[72, true] = BigInt("1")
    val z = x - y
    val expected: BV[72, true] = BigInt("2361183241434822606847")
    assertEquals(z, expected)
  }

  // Unsigned 96-bit tests
  test("Unsigned 96-bit addition with overflow") {
    val x: BV[96, false] = BigInt("79228162514264337593543950335")
    val y: BV[96, false] = BigInt("1")
    val z = x + y
    val expected: BV[96, false] = BigInt("0")
    assertEquals(z, expected)
  }

  // Intermediate lengths
  test("Unsigned 20-bit addition without overflow") {
    val x: BV[20, false] = 500000
    val y: BV[20, false] = 200000
    val z = x + y
    val expected: BV[20, false] = 700000
    assertEquals(z, expected)
  }

  test("Unsigned 37-bit addition without overflow") {
    val x: BV[37, false] = BigInt("68719476735")
    val y: BV[37, false] = 1
    val z = x + y
    val expected: BV[37, false] = BigInt("68719476736")
    assertEquals(z, expected)
  }

  test("Signed 37-bit subtraction with underflow") {
    val x: BV[37, true] = BigInt("-68719476736")
    val y: BV[37, true] = 1
    val z = x - y
    val expected: BV[37, true] = BigInt("68719476735")
    assertEquals(z, expected)
  }
}
