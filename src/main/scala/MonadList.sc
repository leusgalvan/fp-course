import cats._
import cats.implicits._

val listMonad: Monad[List] = new Monad[List] {
  override def pure[A](x: A): List[A] =
    List(x)

  override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
    fa match {
      case Nil => Nil
      case (h :: t) => f(h) ::: flatMap(t)(f)
    }

  override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???
}

listMonad.flatMap(List(1, 2, 3))(a => List(a + 1, a + 2))