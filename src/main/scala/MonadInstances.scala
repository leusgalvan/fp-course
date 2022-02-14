import cats._
import cats.implicits._
import cats.data._

import scala.util.{Failure, Success, Try}

object MonadInstances {
  def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def pure[A](x: A): Either[E, A] = Right(x)

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = {
      fa match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
      }
    }

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = {
      f(a) match {
        case Right(Right(b)) => Right(b)
        case Right(Left(a)) => tailRecM(a)(f)
        case Left(e) => Left(e)
      }
    }
  }
}
