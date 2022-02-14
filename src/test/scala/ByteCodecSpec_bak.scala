//import org.scalacheck.Arbitrary
//import org.scalacheck.Prop.forAll
//import org.scalatest.funsuite.AnyFunSuite
//import org.scalatest.prop.Configuration
//import org.typelevel.discipline.Laws
//import org.typelevel.discipline.scalatest.FunSuiteDiscipline
//
//import java.nio.ByteBuffer
//import scala.util.Try
//
//object Tests {
//  trait ByteDecoder[A] {
//    def decode(bytes: Array[Byte]): Option[A]
//  }
//
//  trait ByteEncoder[A] {
//    def encode(a: A): Array[Byte]
//  }
//
//  trait ByteCodec[A] extends ByteDecoder[A] with ByteEncoder[A]
//
//  trait ByteCodecLaws[A] {
//    def codec: ByteCodec[A]
//
//    def isomorphism(a: A): Boolean =
//      codec.decode(codec.encode(a)) == Some(a)
//  }
//
//  trait ByteCodecTests[A] extends Laws {
//    def laws: ByteCodecLaws[A]
//
//    def byteCodec(implicit arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
//      name = "byteCodec",
//      parent = None,
//      "isomorphism" -> forAll(laws.isomorphism _)
//    )
//  }
//
//  object ByteCodecTests {
//    def apply[A](implicit bc: ByteCodec[A]): ByteCodecTests[A] = new ByteCodecTests[A] {
//      override def laws: ByteCodecLaws[A] = new ByteCodecLaws[A] {
//        override def codec: ByteCodec[A] = bc
//      }
//    }
//  }
//
//  implicit object IntByteCodec extends ByteCodec[Int] {
//    override def decode(bytes: Array[Byte]): Option[Int] = {
//      if(bytes.length != 4) None
//      else {
//        val bb = ByteBuffer.allocate(4)
//        bb.put(bytes)
//        bb.flip()
//        Some(bb.getInt)
//      }
//    }
//
//    override def encode(a: Int): Array[Byte] = {
//      val bb = ByteBuffer.allocate(4)
//      bb.putInt(a)
//      bb.array()
//    }
//  }
//
//  implicit object StringByteCodec extends ByteCodec[String] {
//    override def decode(bytes: Array[Byte]): Option[String] = Try(new String(bytes)).toOption
//
//    override def encode(a: String): Array[Byte] = a.getBytes
//  }
//}
//
//import Tests._
//
//class ByteCodecSpec_bak extends AnyFunSuite with Configuration with FunSuiteDiscipline {
//  checkAll("ByteCodec[Int]", ByteCodecTests[Int].byteCodec)
//  checkAll("ByteCodec[String]", ByteCodecTests[String].byteCodec)
//}
