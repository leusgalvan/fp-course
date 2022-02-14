import cats._
import cats.implicits._

val optionMonad: Monad[Option] = new Monad[Option] {
  override def pure[A](x: A): Option[A] = Some(x)

  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
    fa match {
      case Some(a) => f(a)
      case None => None
    }

  override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = {
    f(a) match {
      case Some(Right(b)) => Some(b)
      case Some(Left(a)) => tailRecM(a)(f)
      case None => None
    }
  }
}

def iterateWhileM[A](initial: A)(f: A => Option[A])(p: A => Boolean): Option[A] =
  optionMonad.tailRecM(initial) { a =>
    if(p(a)) f(a).map(x => Left(x))
    else Some(Right(a))
  }

iterateWhileM(1)(n => Some(n+1))(_ < 100000)