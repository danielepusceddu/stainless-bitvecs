/* Copyright 2009-2021 EPFL, Lausanne */

import scala.collection.immutable.BitSet
import scala.language.implicitConversions
import scala.compiletime.constValue
import scala.compiletime.summonInline

object BitVectorsClasses {

  trait BVKind {
    def len: Int
    def signed: Boolean
  }

  sealed abstract class BVKindSigned extends BVKind {
    final def signed = true
  }

  sealed abstract class BVKindUnsigned extends BVKind {
    final def signed = false
  }

  object i8 extends BVKindSigned {
    def len = 8
  }

  object i16 extends BVKindSigned {
    def len = 16
  }
  object i32 extends BVKindSigned {
    def len = 32
  }

  object i64 extends BVKindSigned {
    def len = 64
  }

  object u8 extends BVKindUnsigned {
    def len = 8
  }

  object u16 extends BVKindUnsigned {
    def len = 16
  }
  object u32 extends BVKindUnsigned {
    def len = 32
  }

  object u64 extends BVKindUnsigned {
    def len = 64
  }

  type Int8 = BV[i8.type]
  type Int16 = BV[i16.type]
  type Int32 = BV[i32.type]
  type Int64 = BV[i64.type]

  type UInt8 = BV[u8.type]
  type UInt16 = BV[u16.type]
  type UInt32 = BV[u32.type]
  type UInt64 = BV[u64.type]

  // implicit def intToBV[T <: BVKind & Singleton](
  //     i: Int
  // )(using ev: ValueOf[T]): BV[T] = {
  //   if !ev.value.signed then {
  //     intToBvRaw(i)
  //   } else {
  //     if i < 0 then {
  //       val bv = intToBvRaw((-i).min((1 << ev.value.len)))
  //       val bi: BigInt = (~bv).rawToBigInt
  //       bigIntToBVRaw(bi + 1)
  //     } else {
  //       bigIntToBVRaw(i.min(1 << ev.value.len) - 1))
  //     }
  //   }
  // }
  //
  // def intToBvRaw[T <: BVKind & Singleton](i: Int): BV[T] = {
  //   var current = i
  //   var builder = BitSet.newBuilder
  //   var bitIndex = 0
  //
  //   while (current > 0 && bitIndex < ev.value.len) {
  //     if ((current & 1) == 1) {
  //       builder += bitIndex
  //     }
  //
  //     current >>= 1
  //     bitIndex += 1
  //   }
  //   return BV(builder.result)
  // }

  implicit def bigIntToBV[T <: BVKind & Singleton](
      i: BigInt
  )(using ev: ValueOf[T]): BV[T] = {
    if !ev.value.signed then {
      bigIntToBVRaw(i)
    } else {
      if i < 0 then {
        val bv = bigIntToBVRaw((-i).min((BigInt(1) << ev.value.len)))
        val bi: BigInt = (~bv).rawToBigInt
        bigIntToBVRaw(bi + 1)
      } else {
        bigIntToBVRaw(i.min((BigInt(1) << ev.value.len) - 1))
      }
    }
  }

  implicit def BVTobigInt[T <: BVKind & Singleton](
      v: BV[T]
  )(using ev: ValueOf[T]): BigInt = {
    if ev.value.signed && v(ev.value.len - 1) then {
      ((~v).rawToBigInt + 1) * -1
    } else {
      v.rawToBigInt
    }
  }

  def bigIntToBVRaw[T <: BVKind & Singleton](
      i: BigInt
  )(using ev: ValueOf[T]): BV[T] = {
    var current = i
    var builder = BitSet.newBuilder
    var bitIndex = 0

    while (current > 0 && bitIndex < ev.value.len) {
      if ((current & 1) == 1) {
        builder += bitIndex
      }

      current >>= 1
      bitIndex += 1
    }
    return BV(builder.result)
  }

  implicit class ArrayIndexing[T](underlying: Array[T]) {
    def apply[X <: BVKind & Singleton](bv: BV[X]): T = underlying(bv.toInt)
  }

  def min[T <: BVKind & Singleton](using ev: ValueOf[T]): BV[T] =
    if ev.value.signed then { BigInt(2 << (ev.value.len - 1) - 1) }
    else { BV[T]() }

  def max[T <: BVKind & Singleton](using ev: ValueOf[T]): BV[T] =
    if ev.value.signed then {
      BigInt(-(2 << (ev.value.len - 1)))
    } else { BV[T]() }

  def fromByte(n: Byte): Int8 = ???
  def fromShort(n: Short): Int16 = ???
  def fromInt(n: Int): Int32 = ???
  def fromLong(n: Long): Int64 = ???

  case class BV[T <: BVKind & Singleton](underlying: BitSet = BitSet.empty)(
      using val ev: ValueOf[T]
  ) {
    def filled: BV[T] = {
      BV[T](BitSet((0 to ev.value.len - 1)*))
    }

    def update(index: Int): BV[T] = {
      if index < ev.value.len then BV[T](underlying + index)
      else throw new IllegalArgumentException("Index out of range")
    }

    def apply(index: Int): Boolean = {
      if index < ev.value.len then underlying.apply(index)
      else throw new IllegalArgumentException("Index out of range")
    }

    def unary_~ : BV[T] = {
      BV[T](this.filled.underlying ^ underlying)
    }
    def unary_- : BV[T] = { ??? } // should only exist on signed?

    def +(other: BV[T]): BV[T] = { // this sum works for 2's complement too
      val a: BigInt = this
      val b: BigInt = other
      a + b
    }

    def -(other: BV[T]): BV[T] = {
      val a: BigInt = this
      val b: BigInt = other
      a - b
    }

    def *(other: BV[T]): BV[T] = {
      val bi1: BigInt = this
      return bi1 * other
    }

    def %(other: BV[T]): BV[T] = {
      return mod(other)
    }

    def mod(other: BV[T]): BV[T] = {
      val bi1: BigInt = this
      return bi1.mod(other)
    }
    def /(other: BV[T]): BV[T] = {
      val bi1: BigInt = this
      return bi1 * other
    }

    def >(other: BV[T]): Boolean = {
      val bi1: BigInt = this
      return bi1 > other
    }

    def >=(other: BV[T]): Boolean = {
      val bi1: BigInt = this
      return bi1 >= other
    }

    def <(other: BV[T]): Boolean = {
      val bi1: BigInt = this
      return bi1 < other
    }

    def <=(other: BV[T]): Boolean = {
      val bi1: BigInt = this
      return bi1 <= other
    }

    def |(other: BV[T]): BV[T] = {
      BV[T](this.underlying | other.underlying)
    }

    def &(other: BV[T]): BV[T] = {
      BV[T](this.underlying & other.underlying)
    }

    def ^(other: BV[T]): BV[T] = {
      BV[T](this.underlying ^ other.underlying)
    }

    def <<(i: Int): BV[T] = {
      BV(
        BitSet(
          underlying.toSeq.map(_ + i).filter(i => i >= 0 && i < ev.value.len)*
        )
      )
    }

    def >>(i: Int): BV[T] = {
      BV(
        BitSet(
          underlying.toSeq.map(_ - i).filter(i => i >= 0 && i < ev.value.len)*
        )
      )
    }

    def <<(other: BV[T]): BV[T] = {
      var bi: BigInt = other

      var res = this
      while (bi > Int.MaxValue) {
        res <<= Int.MaxValue
        bi -= Int.MaxValue
      }

      return res << bi.toInt
    }

    def >>(other: BV[T]): BV[T] = {
      var bi: BigInt = other

      var res = this
      while (bi > Int.MaxValue) {
        res >>= Int.MaxValue
        bi -= Int.MaxValue
      }

      return res >> bi.toInt
    }

    def >>>(other: BV[T]): BV[T] = { ??? }

    def widen[T <: BVKind & Singleton](using ev2: ValueOf[T]): BV[T] = {
      BV(underlying)
    }

    def narrow[T <: BVKind & Singleton](using ev2: ValueOf[T]): BV[T] = {
      val full: BV[T] = BV()
      BV(underlying & full.filled.underlying)
    }

    def toSigned[X <: BV[? <: BVKind & Singleton]]: X = { ??? }
    def toUnsigned[X <: BV[? <: BVKind & Singleton]]: X = { ??? }

    inline def rawToBigInt: BigInt = {
      var acc = BigInt(0)
      (0 to ev.value.len - 1)
        .map { idx =>
          if (underlying(idx)) {
            acc += BigInt(1) << idx
          }
        }
      acc
    }

    def toByte: Byte = { ??? }
    def toShort: Short = { ??? }
    def toInt: Int = { ??? }
    def toLong: Long = { ??? }

    override def toString(): String = (0 to ev.value.len - 1)
      .map { idx =>
        if underlying(idx) then "1" else "0"
      }
      .mkString
      .reverse
  }
}

// just so you know this is the stupid implementation. I would say we just brute the implementations for the basic operations on this today and then benchmark it
