import cats._
import cats.implicits._
import scala.util._

implicit val tryMonad: Monad[Try] = new Monad[Try] {
  override def pure[A](x: A): Try[A] = ???

  override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = ???

  override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???
}
