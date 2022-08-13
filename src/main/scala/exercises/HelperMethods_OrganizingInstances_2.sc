import java.io.FileOutputStream
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
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

case class Switch(isOn: Boolean)
object SwitchByteEncoder extends ByteEncoder[Switch] {
  // Should return an array of 1 byte:
  // - '1' if isOn is true
  // - '0' otherwise
  override def encode(a: Switch): Array[Byte] = ???
}