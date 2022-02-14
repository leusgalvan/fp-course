import java.nio.ByteBuffer

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
}

implicit object StringByteEncoder extends ByteEncoder[String] {
  override def encode(a: String): Array[Byte] = {
    a.getBytes()
  }
}

implicit object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(n)
    bb.array()
  }
}

//implicit object OptionString extends ByteEncoder[Option[String]] {
//  override def encode(a: Option[String]): Array[Byte] = {
//    a match {
//      case Some(value) => StringByteEncoder.encode(value)
//      case None => Array[Byte]()
//    }
//  }
//}
//
//implicit object OptionInt extends ByteEncoder[Option[Int]] {
//  override def encode(a: Option[Int]): Array[Byte] = {
//    a match {
//      case Some(value) => IntByteEncoder.encode(value)
//      case None => Array[Byte]()
//    }
//  }
//}

implicit def optionEncoder[A](implicit encA: ByteEncoder[A]): ByteEncoder[Option[A]] = new ByteEncoder[Option[A]] {
  override def encode(a: Option[A]): Array[Byte] = {
    a match {
      case Some(value) => encA.encode(value)
      case None => Array[Byte]()
    }
  }
}

ByteEncoder[String].encode("hello")
ByteEncoder[Int].encode(1000)
ByteEncoder[Option[String]].encode(Option("world"))
ByteEncoder[Option[String]].encode(None)
ByteEncoder[Option[Int]].encode(Option(1000))
ByteEncoder[Option[Int]].encode(None)