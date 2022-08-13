import java.nio.ByteBuffer

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

implicit object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(4)
    bb.putInt(n)
    bb.array()
  }
}

implicit object StringByteEncoder extends ByteEncoder[String] {
  override def encode(a: String): Array[Byte] = {
    a.getBytes
  }
}

implicit class ByteEncoderOps[A](val a: A) extends AnyVal {
  def encode(implicit enc: ByteEncoder[A]): Array[Byte] =
    enc.encode(a)
}

5.encode
"hello world".encode

trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

implicit object IntByteDecoder extends ByteDecoder[Int] {
  override def decode(bytes: Array[Byte]): Option[Int] = {
    if(bytes.length != 4) None
    else {
      val bb = ByteBuffer.allocate(4)
      bb.put(bytes)
      bb.flip()
      Some(bb.getInt())
    }
  }
}
