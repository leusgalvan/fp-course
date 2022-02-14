import cats._
import cats.implicits._

sealed trait MOption[+A]

object MOption {
  case class MSome[+A](a: A) extends MOption[A]
  case object MNone extends MOption[Nothing]

  implicit val monadMOption: Monad[MOption] = new Monad[MOption] {
    override def pure[A](x: A): MOption[A] =
      MSome(x)

    override def flatMap[A, B](fa: MOption[A])(f: A => MOption[B]): MOption[B] =
      fa match {
        case MSome(a) => f(a)
        case MNone => MNone
      }

    override def tailRecM[A, B](a: A)(f: A => MOption[Either[A, B]]): MOption[B] = ???

    override def map[A, B](fa: MOption[A])(f: A => B): MOption[B] =
      flatMap(fa)(a => pure(f(a)))

    override def flatten[A](ffa: MOption[MOption[A]]): MOption[A] =
      flatMap(ffa)(identity)
  }
}

import MOption._

val x: MOption[Int] = Monad[MOption].pure(5)
val y: MOption[Int] = Monad[MOption].pure(6).flatMap(i => MNone)

val z: MOption[Int] =
  for {
    a <- Monad[MOption].pure(5)
    b <- (MNone: MOption[Int])
  } yield a + b

val ffa: MOption[MOption[Int]] = Monad[MOption].pure(Monad[MOption].pure(5))
ffa.flatten
Monad[MOption].flatten(ffa)