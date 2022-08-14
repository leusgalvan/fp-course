package exercises

import cats._
import cats.implicits._

import scala.util.Try

// Either[E, *] :  * -> *
object MonadErrorInstances_3_try {
  def eitherME[E]: MonadError[Either[E, *], E] = new MonadError[Either[E, *], E] {
    override def raiseError[A](e: E): Either[E, A] =
      Left(e)

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
      fa match {
        case Right(a) => Right(a)
        case Left(e) => f(e)
      }

    override def pure[A](x: A): Either[E, A] = ???

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???
  }

  val tryME: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {
    override def raiseError[A](e: Throwable): Try[A] = ???

    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = ???

    override def pure[A](x: A): Try[A] = ???

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = ???

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???
  }
}
