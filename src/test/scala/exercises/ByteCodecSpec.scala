package exercises

import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import org.scalacheck.Prop._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.nio.ByteBuffer

object Tests {
  trait ByteDecoder[A]{
    def decode(bytes:Array[Byte]): Option[A]
  }

  trait ByteEncoder[A] {
    def encode(o:A): Array[Byte]
  }

  trait ByteCodec[A] extends ByteDecoder[A] with ByteEncoder[A]

  trait ByteCodecLaws[A]{
    def codec: ByteCodec[A]

    def isomorphism(o:A): Boolean =
      codec.decode(codec.encode(o)) == Some(o)
  }

  trait ByteCodecTests[A] extends Laws {
    def laws: ByteCodecLaws[A]

    def byteCodec(implicit arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
      "byteCodec",
      parent = None,
      props = "isomorphism" -> forAll(laws.isomorphism _)
    )
  }

  implicit object IntByteCodec extends ByteCodec[Int] {
    override def decode(bytes: Array[Byte]): Option[Int] = {
      if(bytes.length != 4) None
      else {
        val bb = ByteBuffer.allocate(4)
        bb.put(bytes)
        bb.flip()
        Some(bb.getInt)
      }
    }

    override def encode(o: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(o)
      bb.array()
    }
  }

  object IntByteCodecLaws extends ByteCodecLaws[Int] {
    override def codec: ByteCodec[Int] = IntByteCodec
  }

  object IntByteCodecTests extends ByteCodecTests[Int] {
    override def laws: ByteCodecLaws[Int] = IntByteCodecLaws
  }
}

import Tests._

class ByteCodecSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {
  checkAll("ByteCodec[Int]", IntByteCodecTests.byteCodec)
}