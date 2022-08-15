trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit object StringByteEncoder extends ByteEncoder[String] {
    override def encode(s: String): Array[Byte] =
      s.getBytes
  }

  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
}

implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] =
    s.getBytes.map(b => (b + 3).toByte)
}

ByteEncoder[String].encode("hello")