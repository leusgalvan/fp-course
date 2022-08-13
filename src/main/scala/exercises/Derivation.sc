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

implicit object OptionString extends ByteEncoder[Option[String]] {
  override def encode(a: Option[String]): Array[Byte] = ???
}

ByteEncoder[String].encode("hello")
ByteEncoder[Int].encode(1000)
ByteEncoder[Option[String]].encode(Option("world"))