package bitvecs
import scala.compiletime._
import scala.compiletime.ops.int.S
import scala.math.Numeric
import scala.math.max

sealed trait Nat
type Natural[T <: Int] = T match
  case 0     => Nat
  case S[n1] => Natural[n1]

type Storage[L <: Int] = L match
  case 0     => Byte
  case 9     => Short
  case 17    => Int
  case 33    => Long
  case 65    => BigInt
  case S[n1] => Storage[n1]

transparent inline def defaultStorageInit[L <: Int & Singleton](using
    ev: ValueOf[L]
) =
  inline if ev.value <= 8 then 0: Byte
  else if ev.value <= 16 then 0: Short
  else if ev.value <= 32 then 0: Int
  else if ev.value <= 64 then 0: Long
  else BigInt(0)

trait BVStorageOps[L <: Int, T]:
  def add(x: T, y: T): T
  def sub(x: T, y: T): T
  def mul(x: T, y: T): T
  def div(x: T, y: T): T
  def and(x: T, y: T): T
  def or(x: T, y: T): T
  def xor(x: T, y: T): T
  def neg(x: T): T
  def lshift(x: T, y: Int): T
  def rshift(x: T, y: Int): T
  def mod(x: T, y: T): T
  def eq(x: T, y: T): Boolean

  def toInt(x: T): Int
  def toBigInt(x: T): BigInt

given storageOpsByte[L <: Int]: BVStorageOps[L, Byte] with
  def add(x: Byte, y: Byte): Byte = (x + y).toByte
  def sub(x: Byte, y: Byte): Byte = (x - y).toByte
  def mul(x: Byte, y: Byte): Byte = (x * y).toByte
  def div(x: Byte, y: Byte): Byte = (x / y).toByte
  def and(x: Byte, y: Byte): Byte = (x & y).toByte
  def or(x: Byte, y: Byte): Byte = (x | y).toByte
  def xor(x: Byte, y: Byte): Byte = (x ^ y).toByte
  def neg(x: Byte) = (~x).toByte
  def lshift(x: Byte, y: Int): Byte = (x << y).toByte
  def rshift(x: Byte, y: Int): Byte = (x >> y).toByte
  def mod(x: Byte, y: Byte): Byte = (x % y).toByte
  def eq(x: Byte, y: Byte): Boolean = x == y

  def toInt(x: Byte): Int = x & 0xff
  def toBigInt(x: Byte): BigInt = BigInt(x & 0xff)

given storageOpsShort[L <: Int]: BVStorageOps[L, Short] with
  def add(x: Short, y: Short): Short = (x + y).toShort
  def sub(x: Short, y: Short): Short = (x - y).toShort
  def mul(x: Short, y: Short): Short = (x * y).toShort
  def div(x: Short, y: Short): Short = (x / y).toShort
  def and(x: Short, y: Short): Short = (x & y).toShort
  def or(x: Short, y: Short): Short = (x | y).toShort
  def xor(x: Short, y: Short): Short = (x ^ y).toShort
  def neg(x: Short) = (~x).toShort
  def lshift(x: Short, y: Int): Short = (x << y).toShort
  def rshift(x: Short, y: Int): Short = (x >> y).toShort
  def mod(x: Short, y: Short): Short = (x % y).toShort
  def eq(x: Short, y: Short): Boolean = x == y

  def toInt(x: Short): Int = x & 0xffff
  def toBigInt(x: Short): BigInt = BigInt(x & 0xffff)

given storageOpsInt[L <: Int]: BVStorageOps[L, Int] with
  def add(x: Int, y: Int): Int = x + y
  def sub(x: Int, y: Int): Int = x - y
  def mul(x: Int, y: Int): Int = x * y
  def div(x: Int, y: Int): Int = x / y
  def and(x: Int, y: Int): Int = x & y
  def or(x: Int, y: Int): Int = x | y
  def xor(x: Int, y: Int): Int = x ^ y
  def neg(x: Int) = ~x
  def lshift(x: Int, y: Int): Int = x << y
  def rshift(x: Int, y: Int): Int = x >> y
  def mod(x: Int, y: Int): Int = x % y
  def eq(x: Int, y: Int): Boolean = x == y

  def toInt(x: Int): Int = x
  def toBigInt(x: Int): BigInt = BigInt(x)

given storageOpsLong[L <: Int]: BVStorageOps[L, Long] with
  def add(x: Long, y: Long): Long = x + y
  def sub(x: Long, y: Long): Long = x - y
  def mul(x: Long, y: Long): Long = x * y
  def div(x: Long, y: Long): Long = x / y
  def and(x: Long, y: Long): Long = x & y
  def or(x: Long, y: Long): Long = x | y
  def xor(x: Long, y: Long): Long = x ^ y
  def neg(x: Long) = ~x
  def lshift(x: Long, y: Int): Long = x << y
  def rshift(x: Long, y: Int): Long = x >>> y
  def mod(x: Long, y: Long): Long = x % y
  def eq(x: Long, y: Long): Boolean = x == y

  def toInt(x: Long): Int = (x & 0xffffffff).toInt
  def toBigInt(x: Long): BigInt = BigInt(x)

given storageOpsBigInt[L <: Int]: BVStorageOps[L, BigInt] with
  def add(x: BigInt, y: BigInt): BigInt = x + y
  def sub(x: BigInt, y: BigInt): BigInt = x - y
  def mul(x: BigInt, y: BigInt): BigInt = x * y
  def div(x: BigInt, y: BigInt): BigInt = x / y
  def and(x: BigInt, y: BigInt): BigInt = x & y
  def or(x: BigInt, y: BigInt): BigInt = x | y
  def xor(x: BigInt, y: BigInt): BigInt = x ^ y
  def neg(x: BigInt) = ~x
  def lshift(x: BigInt, y: Int): BigInt = x << y
  def rshift(x: BigInt, y: Int): BigInt = x >> y
  def mod(x: BigInt, y: BigInt): BigInt = x % y
  def eq(x: BigInt, y: BigInt): Boolean = x == y

  def toInt(x: BigInt): Int = x.toInt
  def toBigInt(x: BigInt): BigInt = x

extension [L <: Int, T](x: T)(using ops: BVStorageOps[L, T])
  def +(y: T): T = ops.add(x, y)
  def -(y: T): T = ops.sub(x, y)
  def *(y: T): T = ops.mul(x, y)
  def /(y: T): T = ops.div(x, y)
  def ^(y: T): T = ops.xor(x, y)
  def &(y: T): T = ops.and(x, y)
  def |(y: T): T = ops.or(x, y)
  def unary_~ = ops.neg(x)
  def <<(y: Int): T = ops.lshift(x, y)
  def >>(y: Int): T = ops.rshift(x, y)
  def %(y: T): T = ops.mod(x, y)
  // override def == (y: T): Boolean = ops.eq(x, y)
  // override def equals(y : Any): Boolean = if x.isInstanceOf[T] then ops.eq(x, y.asInstanceOf[T]) else false

trait ToStorage[L <: Int, T]:
  def apply(value: Byte): T
  def apply(value: Short): T
  def apply(value: Int): T
  def apply(value: Long): T
  def apply(value: BigInt): T

inline given toByteStorage[L <: Int]: ToStorage[L, Byte] with
  def apply(value: Byte) = value
  def apply(value: Short) = value.toByte
  def apply(value: Int): Byte = value.toByte
  def apply(value: Long): Byte = value.toByte
  def apply(value: BigInt): Byte = value.toByte

inline given toShortStorage[L <: Int]: ToStorage[L, Short] with
  def apply(value: Byte): Short = value.toShort
  def apply(value: Short): Short = value
  def apply(value: Int): Short = value.toShort
  def apply(value: Long): Short = value.toShort
  def apply(value: BigInt): Short = value.toShort

inline given toIntStorage[L <: Int]: ToStorage[L, Int] with
  def apply(value: Byte): Int = value.toInt
  def apply(value: Short): Int = value.toInt
  def apply(value: Int): Int = value
  def apply(value: Long): Int = value.toInt
  def apply(value: BigInt): Int = value.toInt

inline given toLongStorage[L <: Int]: ToStorage[L, Long] with
  def apply(value: Byte): Long = value.toLong
  def apply(value: Short): Long = value.toLong
  def apply(value: Int): Long = value.toLong
  def apply(value: Long): Long = value
  def apply(value: BigInt): Long = value.toLong

inline given toBigIntStorage[L <: Int]: ToStorage[L, BigInt] with
  def apply(value: Byte): BigInt = BigInt(value)
  def apply(value: Short): BigInt = BigInt(value)
  def apply(value: Int): BigInt = BigInt(value)
  def apply(value: Long): BigInt = BigInt(value)
  def apply(value: BigInt): BigInt = value

implicit inline def intToBV[
    L <: Int & Singleton,
    S <: (true | false) & Singleton
](i: Int)(using
    nat: Natural[L] =:= Nat,
    len: ValueOf[L],
    signed: ValueOf[S],
    storageOps: BVStorageOps[L, Storage[L]],
    toStorage: ToStorage[L, Storage[L]]
): BV[L, S] =
  inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
  then BV[L, S](toStorage(i))
  else BV[L, S](toStorage(i) & ((toStorage(1) << len.value) - toStorage(1)))

implicit inline def bigIntToBV[
    L <: Int & Singleton,
    S <: (true | false) & Singleton
](i: BigInt)(using
    nat: Natural[L] =:= Nat,
    len: ValueOf[L],
    signed: ValueOf[S],
    storageOps: BVStorageOps[L, Storage[L]],
    toStorage: ToStorage[L, Storage[L]]
): BV[L, S] = {
  val bv =
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then BV[L, S](toStorage(i))
    else BV[L, S](toStorage(i) & ((toStorage(1) << len.value) - toStorage(1)))

  inline if signed.value && len.value > 64 then
    // this unfortunately has to happen as we don't have a way to "tell" the
    // bv that its negative...
    if (bv.underlying & bv.signBit) != toStorage(0) then
      BV[L, S](bv.underlying - bv.modulus)
    else bv
  else bv
}

inline implicit def longIntToBV[
    L <: Int & Singleton,
    S <: (true | false) & Singleton
](
    i: Long
)(using
    nat: Natural[L] =:= Nat,
    len: ValueOf[L],
    signed: ValueOf[S],
    storageOps: BVStorageOps[L, Storage[L]],
    toStorage: ToStorage[L, Storage[L]]
): BV[L, S] =
  inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
  then BV[L, S](toStorage(i))
  else BV[L, S](toStorage(i) & ((toStorage(1) << len.value) - toStorage(1)))

implicit inline def BVToInt[
    L <: Int & Singleton,
    S <: (true | false) & Singleton
](bv: BV[L, S])(using
    nat: Natural[L] =:= Nat,
    len: ValueOf[L],
    signed: ValueOf[S],
    storageOps: BVStorageOps[L, Storage[L]],
    toStorage: ToStorage[L, Storage[L]]
): Int = {
  inline if len.value > 64 then
    storageOps.toInt(
      bv.underlying
    ) // we always store the "correct" value in bigint
  else inline if !signed.value || len.value == 32
  then
    storageOps.toInt(
      bv.underlying
    ) // we don't care in this case. This could lead to the value being negative if we are of len 32 but there is no other way
  else if (bv.underlying & bv.signBit) == toStorage(0) then
    storageOps.toInt(bv.underlying)
  else
    val mask = ~((1 << len.value) - 1)
    -(~(storageOps.toInt(bv.underlying) | mask) + 1)
}

implicit inline def BVToBigInt[
    L <: Int & Singleton,
    S <: (true | false) & Singleton
](bv: BV[L, S])(using
    nat: Natural[L] =:= Nat,
    len: ValueOf[L],
    signed: ValueOf[S],
    storageOps: BVStorageOps[L, Storage[L]],
    toStorage: ToStorage[L, Storage[L]]
): BigInt = {
  inline if len.value > 64 then
    storageOps.toInt(
      bv.underlying
    ) // we always store the "correct" value in bigint
  else inline if signed.value then
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then
      storageOps.toBigInt(
        bv.underlying
      ) // underlying correctly encodes signdness
    else if (bv.underlying & bv.signBit) == toStorage(0) then // positive
      storageOps.toBigInt(bv.underlying)
    else
      val mask = ~((1 << len.value) - 1)
      -(~(storageOps.toBigInt(bv.underlying) | mask) + 1)
  else inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
  then
    if (bv.underlying & bv.signBit) == toStorage(0) then // positive anyways
      storageOps.toBigInt(bv.underlying)
    else // value is negative in storage but we want positive
      (BigInt(1) << (len.value - 1)) | storageOps.toBigInt(bv.underlying)
  else // we don't have to "correct" the sign bit...
    storageOps.toBigInt(bv.underlying)
}

// implicit def byteIntToBV[L <: Int & Singleton, S <: (true | false) & Singleton](i: byte)
// (using
//   nat: Natural[L] =:= Nat,
//   len: ValueOf[L],
//   signed: ValueOf[S],
//   storageOps: BVStorageOps[L, Storage[L]],
//   toStorage: ToStorage[L, Storage[L]]): BV[L, S] = BV[L, S](toStorage(i))
//
class BV[L <: Int & Singleton, S <: (true | false) & Singleton](using
    nat: Natural[L] =:= Nat,
    signed: ValueOf[S],
    len: ValueOf[L],
    storageOps: BVStorageOps[L, Storage[L]],
    toStorage: ToStorage[L, Storage[L]]
)(private[bitvecs] val underlying: Storage[L]) {

  private[bitvecs] inline def modulus = toStorage(1) << len.value
  private[bitvecs] inline def mask = (toStorage(1) << len.value) - toStorage(1)
  private[bitvecs] inline def signBit = toStorage(1) << (len.value - 1)

  println(s"Value of storage is $underlying")

  // For bigint overflow and underflow have to be considered.
  //
  // In the unsigned case, everything is fine as we can never get negative but
  // in the signed case special care has to be taken. Underflow (so negative to
  // positive) is fine, BigInt can handle this (`BigInt(-129) & 0xff == 127`, as
  // one would expect), but overflow to negative has to be handled. The bigint
  // then stores

  inline def +(other: BV[L, S]): BV[L, S] = {
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then BV[L, S](underlying + other.underlying)
    else
      val storage = (underlying + other.underlying) & mask
      // only have to take care here if BigInt storage is used & we are signed

      inline if len.value > 64 && signed.value then
        if (storage & signBit) != toStorage(0) then BV[L, S](storage - modulus)
        else BV[L, S](storage)
      else BV[L, S](storage)
  }

  inline def -(other: BV[L, S]): BV[L, S] = {
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then BV[L, S](underlying - other.underlying)
    else
      val storage = (underlying - other.underlying) & mask

      inline if len.value > 64 && signed.value then
        if (storage & signBit) != toStorage(0) then BV[L, S](storage - modulus)
        else BV[L, S](storage)
      else BV[L, S](storage)
  }

  inline def *(other: BV[L, S]): BV[L, S] = {
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then BV[L, S](underlying * other.underlying)
    else
      val storage = (underlying * other.underlying) & mask

      inline if len.value > 64 && signed.value then
        if (storage & signBit) != toStorage(0) then
          BV[L, S](((storage ^ mask) + toStorage(1)) * toStorage(-1))
        else BV[L, S](storage)
      else BV[L, S](storage)
  }

  inline def /(other: BV[L, S]): BV[L, S] = {
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then BV[L, S](underlying / other.underlying)
    else inline if signed.value then
      inline if len.value > 64 then
        val storage = (underlying / other.underlying) & mask

        if (storage & signBit) != toStorage(0) then
          BV[L, S](((storage ^ mask) + toStorage(1)) * toStorage(-1))
        else BV[L, S](storage)
      else
      // this is an especially shitty case. Here we need to "extend" the other
      // to fill the underlying s.t. the division works
      if (other.underlying & signBit) != toStorage(0) then
        val upperBits = (~toStorage(0)) ^ mask
        BV[L, S]((underlying / (other.underlying | upperBits)) & mask)
      else BV[L, S]((underlying / other.underlying) & mask)
    else
      val storage = (underlying / other.underlying) & mask
      BV[L, S](storage)
  }

  inline def %(other: BV[L, S]): BV[L, S] = {
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then BV[L, S](underlying % other.underlying)
    else inline if signed.value then
      inline if len.value > 64 then
        val storage = (underlying % other.underlying) & mask

        if (storage & signBit) != toStorage(0) then
          BV[L, S](((storage ^ mask) + toStorage(1)) * toStorage(-1))
        else BV[L, S](storage)
      else
      // this is an especially shitty case. Here we need to "extend" the other
      // to fill the underlying s.t. the division works
      if (other.underlying & signBit) != toStorage(0) then
        val upperBits = (~toStorage(0)) ^ mask

        if (underlying & signBit) != toStorage(0) then
          BV[L, S](
            ((underlying | upperBits) % (other.underlying | upperBits)) & mask
          )
        else BV[L, S]((underlying % (other.underlying | upperBits)) & mask)
      else BV[L, S]((underlying % other.underlying) & mask)
    else
      val storage = (underlying % other.underlying) & mask
      BV[L, S](storage)
  }

  inline def <<(by: Int): BV[L, S] = {
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then BV[L, S](underlying << by)
    else BV[L, S]((underlying << by) & mask)
  }

  inline def >>(by: Int): BV[L, S] = {
    // unfortunately, we have to do some differentiation here. The logical shift
    // does, unfortunately, not work for non Int integers, so we have to emulate
    // it.
    //
    // we have to differentiate: If we are signed, we have to check if the
    // former MSB is set and if yes, we have to "fill" with 1's (if the
    // underlying storage is larger the actual MSB could be unset leading to the
    // wrong shift... )
    inline if signed.value || len.value > 64
    then // BigInt just takes care of shifts..
      inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value >= 64
      then
        // here our storage luckily matches with the len of the bv, so we can
        // just ignore everything
        BV[L, S](underlying >> by)
      else
        // in this case the MSB of our BV does not line up with the MSB of the
        // underlying storage.. So we have to "fill" the MSBs
        val negShiftedMask =
          ~((toStorage(1) << max(len.value - by, 0)) - toStorage(1)) & mask
        BV[L, S]((underlying >> by) | negShiftedMask)
    else
      val shiftedMask = (toStorage(1) << max(len.value - by, 0)) - toStorage(1)
      println(s"value of shiftedMask is $shiftedMask")
      BV[L, S]((underlying >> by) & shiftedMask)
  }

  inline def &(other: BV[L, S]): BV[L, S] = {
    // no need to mask, cannot grow
    BV[L, S](underlying & other.underlying)
  }

  inline def |(other: BV[L, S]): BV[L, S] = {
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then BV[L, S](underlying | other.underlying)
    else BV[L, S]((underlying | other.underlying) & mask)
  }

  inline def ^(other: BV[L, S]): BV[L, S] = {
    inline if len.value == 8 || len.value == 16 || len.value == 32 || len.value == 64
    then BV[L, S](underlying ^ other.underlying)
    else BV[L, S]((underlying ^ other.underlying) & mask)
  }

  inline def ==(other: BV[L, S]): Boolean = {
    underlying == other.underlying
  }

  inline def apply(index: Int): Boolean = {
    if index < len.value then
      (underlying & toStorage(1) << index) != toStorage(0)
    else throw new IllegalArgumentException("Index out of range")
  }

  override def toString(): String = (0 to len.value - 1)
    .map { idx =>
      if this(idx) then "1" else "0"
    }
    .mkString
    .reverse
}

@main def run(): Unit = {
  // val a: BV[8, false] = 5
  //
  // val b: BV[8, false] = -6
  //
  // println((a + b): BigInt)
  //
  val expected: BV[64, false] = BigInt("9223372036854775808")
  println(expected)
}
