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


  // Unsigned 8-bit tests
  test("Unsigned 8-bit AND operation") {
    val x: BV[8, false] = 0b10101010
    val y: BV[8, false] = 0b11001100
    val z = x & y
    val expected: BV[8, false] = 0b10001000
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit OR operation") {
    val x: BV[8, false] = 0b10101010
    val y: BV[8, false] = 0b11001100
    val z = x | y
    val expected: BV[8, false] = 0b11101110
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit XOR operation") {
    val x: BV[8, false] = 0b10101010
    val y: BV[8, false] = 0b11001100
    val z = x ^ y
    val expected: BV[8, false] = 0b01100110
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit left shift") {
    val x: BV[8, false] = 0b00001111
    val z = x << 2
    val expected: BV[8, false] = 0b00111100
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit right shift") {
    val x: BV[8, false] = 0b00001111
    val z = x >> 2
    val expected: BV[8, false] = 0b00000011
    assertEquals(z, expected)
  }

  // Signed 8-bit tests
  test("Signed 8-bit AND operation") {
    val x: BV[8, true] = 0b10101010
    val y: BV[8, true] = 0b11001100
    val z = x & y
    val expected: BV[8, true] = 0b10001000
    assertEquals(z, expected)
  }

  test("Signed 8-bit arithmetic right shift") {
    val x: BV[8, true] = -8 // 0b11111000 in 8-bit two's complement
    val z = x >> 2
    val expected: BV[8, true] = -2 // 0b11111110 in 8-bit two's complement
    assertEquals(z, expected)
  }

  // Unsigned 32-bit tests
  test("Unsigned 32-bit AND operation") {
    val x: BV[32, false] = 0xFFFF0000
    val y: BV[32, false] = 0x00FFFF00
    val z = x & y
    val expected: BV[32, false] = 0x00FF0000
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit OR operation") {
    val x: BV[32, false] = 0xFFFF0000
    val y: BV[32, false] = 0x00FFFF00
    val z = x | y
    val expected: BV[32, false] = 0xFFFFFF00
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit XOR operation") {
    val x: BV[32, false] = 0xFFFF0000
    val y: BV[32, false] = 0x00FFFF00
    val z = x ^ y
    val expected: BV[32, false] = 0xFF00FF00
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit left shift") {
    val x: BV[32, false] = 0x00000001
    val z = x << 16
    val expected: BV[32, false] = 0x00010000
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit right shift") {
    val x: BV[32, false] = 0x00010000
    val z = x >> 8
    val expected: BV[32, false] = 0x00000100
    assertEquals(z, expected)
  }

  // Signed 32-bit tests
  test("Signed 32-bit arithmetic right shift") {
    val x: BV[32, true] = -2147483648 // 0x80000000
    val z = x >> 1
    val expected: BV[32, true] = -1073741824 // 0xC0000000
    assertEquals(z, expected)
  }

  // Mixed-length tests
  test("Unsigned 16-bit AND operation") {
    val x: BV[16, false] = 0b1111000011110000
    val y: BV[16, false] = 0b1100110011001100
    val z = x & y
    val expected: BV[16, false] = 0b1100000011000000
    assertEquals(z, expected)
  }

  test("Unsigned 16-bit OR operation") {
    val x: BV[16, false] = 0b1111000011110000
    val y: BV[16, false] = 0b1100110011001100
    val z = x | y
    val expected: BV[16, false] = 0b1111110011111100
    assertEquals(z, expected)
  }

  test("Unsigned 16-bit XOR operation") {
    val x: BV[16, false] = 0b1111000011110000
    val y: BV[16, false] = 0b1100110011001100
    val z = x ^ y
    val expected: BV[16, false] = 0b0011110000111100
    assertEquals(z, expected)
  }

  // Unsigned 8-bit tests
  test("Unsigned 8-bit multiplication without overflow") {
    val x: BV[8, false] = 10
    val y: BV[8, false] = 20
    val z = x * y
    val expected: BV[8, false] = 200
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit multiplication with overflow") {
    val x: BV[8, false] = 16
    val y: BV[8, false] = 16
    val z = x * y
    val expected: BV[8, false] = 0 // 256 wraps around to 0 in 8-bit arithmetic
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit division") {
    val x: BV[8, false] = 200
    val y: BV[8, false] = 10
    val z = x / y
    val expected: BV[8, false] = 20
    assertEquals(z, expected)
  }

  test("Unsigned 8-bit modulo") {
    val x: BV[8, false] = 200
    val y: BV[8, false] = 11
    val z = x % y
    val expected: BV[8, false] = 2
    assertEquals(z, expected)
  }

  // Signed 8-bit tests
  test("Signed 8-bit multiplication without overflow") {
    val x: BV[8, true] = -10
    val y: BV[8, true] = 20
    val z = x * y
    val expected: BV[8, true] = -200
    assertEquals(z, expected)
  }

  test("Signed 8-bit multiplication with overflow") {
    val x: BV[8, true] = 64
    val y: BV[8, true] = 2
    val z = x * y
    val expected: BV[8, true] = -128 // Overflow wraps around
    assertEquals(z, expected)
  }

  test("Signed 8-bit division") {
    val x: BV[8, true] = -128
    val y: BV[8, true] = 2
    val z = x / y
    val expected: BV[8, true] = -64
    assertEquals(z, expected)
  }

  test("Signed 8-bit modulo") {
    val x: BV[8, true] = -128
    val y: BV[8, true] = 10
    val z = x % y
    val expected: BV[8, true] = -8
    assertEquals(z, expected)
  }

  // Unsigned 32-bit tests
  test("Unsigned 32-bit multiplication without overflow") {
    val x: BV[32, false] = 10000
    val y: BV[32, false] = 3000
    val z = x * y
    val expected: BV[32, false] = 30000000
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit multiplication with overflow") {
    val x: BV[32, false] = 0xFFFFFFFF
    val y: BV[32, false] = 2
    val z = x * y
    val expected: BV[32, false] = 0xFFFFFFFE
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit division") {
    val x: BV[32, false] = 30000000
    val y: BV[32, false] = 10000
    val z = x / y
    val expected: BV[32, false] = 3000
    assertEquals(z, expected)
  }

  test("Unsigned 32-bit modulo") {
    val x: BV[32, false] = 30000001
    val y: BV[32, false] = 10000
    val z = x % y
    val expected: BV[32, false] = 1
    assertEquals(z, expected)
  }

  // Signed 32-bit tests
  test("Signed 32-bit multiplication without overflow") {
    val x: BV[32, true] = -100000
    val y: BV[32, true] = 300
    val z = x * y
    val expected: BV[32, true] = -30000000
    assertEquals(z, expected)
  }

  test("Signed 32-bit division") {
    val x: BV[32, true] = -30000000
    val y: BV[32, true] = 100000
    val z = x / y
    val expected: BV[32, true] = -300
    assertEquals(z, expected)
  }

  test("Signed 32-bit modulo") {
    val x: BV[32, true] = -30000001
    val y: BV[32, true] = 100000
    val z = x % y
    val expected: BV[32, true] = -1
    assertEquals(z, expected)
  }

  // Unsigned 64-bit tests
  test("Unsigned 64-bit multiplication") {
    val x: BV[64, false] = BigInt("9223372036854775807")
    val y: BV[64, false] = 2
    val z = x * y
    val expected: BV[64, false] = BigInt("18446744073709551614") % BigInt("2").pow(64)
    assertEquals(z, expected)
  }

  test("Unsigned 64-bit division") {
    val x: BV[64, false] = BigInt("18446744073709551614")
    val y: BV[64, false] = 2
    val z = x / y
    val expected: BV[64, false] = BigInt("9223372036854775807")
    assertEquals(z, expected)
  }

  test("Unsigned 64-bit modulo") {
    val x: BV[64, false] = BigInt("18446744073709551615")
    val y: BV[64, false] = 3
    val z = x % y
    val expected: BV[64, false] = 0
    assertEquals(z, expected)
  }

    // Unsigned 37-bit tests
  test("Unsigned 37-bit multiplication without overflow") {
    val x: BV[37, false] = BigInt("238941")
    val y: BV[37, false] = 5
    val z = x * y
    val expected: BV[37, false] = BigInt("1194705") // Wraps around modulo 2^37
    assertEquals(z, expected)
  }

  test("Unsigned 37-bit division") {
    val x: BV[37, false] = BigInt("68719476736")
    val y: BV[37, false] = 2
    val z = x / y
    val expected: BV[37, false] = BigInt("34359738368")
    assertEquals(z, expected)
  }

  test("Unsigned 37-bit modulo") {
    val x: BV[37, false] = BigInt("68719476737")
    val y: BV[37, false] = 10
    val z = x % y
    val expected: BV[37, false] = BigInt("7")
    assertEquals(z, expected)
  }

  test("Unsigned 37-bit AND operation") {
    val x: BV[37, false] = BigInt("68719476735")
    val y: BV[37, false] = BigInt("34359738368")
    val z = x & y
    val expected: BV[37, false] = BigInt("34359738368")
    assertEquals(z, expected)
  }

  test("Unsigned 37-bit OR operation") {
    val x: BV[37, false] = BigInt("68719476734")
    val y: BV[37, false] = BigInt("1")
    val z = x | y
    val expected: BV[37, false] = BigInt("68719476735")
    assertEquals(z, expected)
  }

  test("Unsigned 37-bit XOR operation") {
    val x: BV[37, false] = BigInt("68719476735")
    val y: BV[37, false] = BigInt("34359738368")
    val z = x ^ y
    val expected: BV[37, false] = BigInt("34359738367")
    assertEquals(z, expected)
  }

  test("Unsigned 37-bit left shift") {
    val x: BV[37, false] = BigInt("1")
    val z = x << 36
    val expected: BV[37, false] = BigInt("1") << 36
    assertEquals(z, expected)
  }

  test("Unsigned 37-bit right shift") {
    val x: BV[37, false] = BigInt("68719476735")
    val z = x >> 1
    val expected: BV[37, false] = BigInt("34359738367")
    assertEquals(z, expected)
  }

  test("Signed 37-bit division") {
    val x: BV[37, true] = 34359738368L
    val y: BV[37, true] = 3
    val z = x / y
    val expected: BV[37, true] = 11453246122L
    assertEquals(z, expected)
  }

  test("Signed 37-bit modulo") {
    val x: BV[37, true] = BigInt("34359738369")
    val y: BV[37, true] = 10
    val z = x % y
    val expected: BV[37, true] = BigInt("9")
    assertEquals(z, expected)
  }

  test("Signed 37-bit AND operation") {
    val x: BV[37, true] = BigInt("17179869183") // Max positive
    val y: BV[37, true] = BigInt("-34359738368") // Min negative
    val z = x & y
    val expected: BV[37, true] = BigInt("0") // No bits overlap
    assertEquals(z, expected)
  }

  test("Signed 37-bit arithmetic right shift") {
    val x: BV[37, true] = BigInt("-34359738368")
    val z = x >> 1
    val expected: BV[37, true] = BigInt("-17179869184")
    assertEquals(z, expected)
  }

  // Unsigned 72-bit tests
  test("Unsigned 72-bit multiplication") {
    val x: BV[72, false] = BigInt("4722366482869645213695")
    val y: BV[72, false] = 3
    val z = x * y
    val expected: BV[72, false] = BigInt("4722366482869645213693") // Wraps around modulo 2^72
    assertEquals(z, expected)
  }

  test("Unsigned 72-bit division") {
    val x: BV[72, false] = BigInt("4722366482869645213695")
    val y: BV[72, false] = 2
    val z = x / y
    val expected: BV[72, false] = BigInt("2361183241434822606847")
    assertEquals(z, expected)
  }

  test("Unsigned 72-bit modulo") {
    val x: BV[72, false] = BigInt("4722366482869645213695")
    val y: BV[72, false] = 14
    val z = x % y
    val expected: BV[72, false] = BigInt("7")
    assertEquals(z, expected)
  }

}
