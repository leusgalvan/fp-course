import java.io.FileOutputStream
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit object StringByteEncoder extends ByteEncoder[String] {
    override def encode(s: String): Array[Byte] =
      s.getBytes
  }
}


trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
}

object FileChannel extends Channel {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(new FileOutputStream("fp-course/test")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

implicit object Rot3StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] =
    s.getBytes.map(b => (b + 3).toByte)
}

FileChannel.write("hello")

// Common situation:
// For each of type A:
// - 1 main instance of ByteEncoder
// - couple more instances for specific use cases

// Goal:
// - Use the main instance by default
// - Provide a different instance for specific use cases