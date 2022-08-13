trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit val stringByteEncoder: ByteEncoder[String] = new ByteEncoder[String] {
    override def encode(s: String): Array[Byte] =
      s.getBytes
  }

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = ???
}

implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] =
    s.getBytes.map(b => (b+3).toByte)
}