import cats._
import cats.data._
import cats.implicits._

5.valid[NonEmptyList[String]]
"error".invalid[Int]

5.validNel[String]
"error".invalidNel[Int]

def concat[A](as: List[A], as2: List[A]): List[A] = ???
