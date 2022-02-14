import cats._
import cats.implicits._
import cats.data._

5.valid[NonEmptyList[String]]
"error".invalid[Int]

5.validNel[String]
"error".invalidNel[Int]

def concat[A](as: List[A], as2: List[A]): List[A] =
  as.foldRight(as2)((a, b) => a :: b)

concat(List(1, 2, 3), List(4))

5.validNec[String]
"error".invalidNec[Int]

6.validNec[String].ensure(NonEmptyChain("number is not even"))(_ % 2 == 0)

Validated.condNec(false, 5, "error")

5.validNec[String].getOrElse(10)
"error".invalidNec[Int].getOrElse(10)

5.validNec[String].orElse(10.validNec[String])
"error".invalidNec[Int].orElse(10.validNec[String])

5.validNec[String].toEither
"error".invalidNec[Int].toEither

Validated.fromEither[NonEmptyChain[String], Int](Right(5))
Validated.fromEither[NonEmptyChain[String], Int](Left(NonEmptyChain("error")))