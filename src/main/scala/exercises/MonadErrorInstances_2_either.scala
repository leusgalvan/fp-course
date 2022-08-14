package exercises

import cats._
import cats.implicits._

// Either[E, *] :  * -> *
object MonadErrorInstances_2_either {
  def eitherME[E]: MonadError[Either[E, *], E] = new MonadError[Either[E, *], E] {
    override def raiseError[A](e: E): Either[E, A] = ???

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] = ???

    override def pure[A](x: A): Either[E, A] = ???

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???
  }
}
