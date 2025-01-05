package bitvecs

class SubtractionTests extends munit.FunSuite {
  test("Signed 8-bit subtraction with underflow") {
    val x: BV[8, true] = -128
    val y: BV[8, true] = 1
    val z = x - y
    val expected: BV[8, true] = 127
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

  test("Signed 37-bit subtraction with underflow") {
    val x: BV[37, true] = BigInt("-68719476736")
    val y: BV[37, true] = 1
    val z = x - y
    val expected: BV[37, true] = BigInt("68719476735")
    assertEquals(z, expected)
  }
}
