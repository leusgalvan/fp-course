trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit val stringByteEncoder: ByteEncoder[String] = instance[String](_.getBytes)

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = new ByteEncoder[A] {
    override def encode(a: A): Array[Byte] =
      f(a)
  }
}

implicit val rot3StringByteEncoder: ByteEncoder[String] =
  ByteEncoder.instance[String](_.getBytes.map(b => (b+3).toByte))