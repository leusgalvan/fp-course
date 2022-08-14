import cats._
import cats.implicits._

val listMonad: Monad[List] = new Monad[List] {
  override def pure[A](x: A): List[A] = ???

  override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = ???

  override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???
}