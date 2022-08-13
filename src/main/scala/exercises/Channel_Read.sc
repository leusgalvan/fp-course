/*
1. Add apply and instance to ByteDecoder

2. Write an instance of ByteDecoder[String] (declare it as implicit val or implicit object)

3. Use the instance to decode the following array: Array(98, 105, 101, 110, 32, 58, 41)
   Extra points: use apply (point 1)

*/
trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}
