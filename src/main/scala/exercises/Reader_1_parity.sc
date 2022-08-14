import cats._
import cats.implicits._
import cats.data._

val signReader: Reader[Int, String] = Reader(n => if(n > 0) "positive" else if(n < 0) "negative" else "zero")
signReader.run(0)

val parityReader: Reader[Int, String] = ???