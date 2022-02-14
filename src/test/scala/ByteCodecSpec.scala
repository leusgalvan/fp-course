import org.scalacheck.Arbitrary
import org.typelevel.discipline.Laws
import org.scalacheck.Prop._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.nio.ByteBuffer
import scala.util.Try

object Tests {
  trait ByteEncoder[A] {
    def encode(o:A): Array[Byte];
  }

  trait ByteDecoder[A]{
    def decode(bytes:Array[Byte]):Option[A]
  }

  trait ByteCodec[A] extends ByteDecoder[A] with ByteEncoder[A]

  trait ByteCodecLaws[A]{
    def codec: ByteCodec[A]
    def isomorphism(o:A): Boolean = codec.decode(codec.encode(o)) == Some(o)
  }

  trait ByteCodecTests[A] extends Laws{
    def laws: ByteCodecLaws[A]
    def byteCodecRules(implicit arb: Arbitrary[A]):RuleSet = new DefaultRuleSet(
      "byteCodec",
      parent = None,
      props = "isomorphism" -> forAll(laws.isomorphism _)
    )
  }

  object ByteCodecTests{
    def apply[A](implicit bc: ByteCodec[A]): ByteCodecTests[A] = new ByteCodecTests[A] {
      override def laws: ByteCodecLaws[A] = new ByteCodecLaws[A] {
        override def codec: ByteCodec[A] = bc
      }
    }
  }

  implicit object IntByteCodec extends ByteCodec[Int]{
    override def encode(o: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(o)
      bb.array()
    }

    override def decode(bytes: Array[Byte]): Option[Int] = {
      if(bytes.length != 4) None
      else {
        val bb = ByteBuffer.allocate(4)
        bb.put(bytes)
        bb.flip()
        Some(bb.getInt)
      }
    }
  }

  object IntByteCodecLaws extends ByteCodecLaws[Int]{
    override def codec: ByteCodec[Int] = IntByteCodec
  }

  object IntByteCodecTests extends ByteCodecTests[Int]{
    override def laws: ByteCodecLaws[Int] = IntByteCodecLaws
  }

  //String stuff
  implicit object StringByteCodec extends ByteCodec[String]{
    override def encode(o: String): Array[Byte] = if(o!=null) o.getBytes else "".getBytes

    override def decode(bytes: Array[Byte]): Option[String] = Try{
      new String(bytes)
    }.toOption
  }

  object StringByteCodecLaws extends ByteCodecLaws[String]{
    override def codec: ByteCodec[String] = StringByteCodec
  }

  object StringByteCodecTests extends ByteCodecTests[String]{
    override def laws: ByteCodecLaws[String] = StringByteCodecLaws
  }

}

import Tests._

class ByteCodecSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {
  checkAll("ByteCodec[Int]", ByteCodecTests[Int].byteCodecRules)
  checkAll("ByteCodec[String]", ByteCodecTests[String].byteCodecRules)
}