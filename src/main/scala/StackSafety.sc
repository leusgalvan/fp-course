import scala.annotation.tailrec



trait Trampoline[+A]
object Trampoline {
  case class Done[A](a: A) extends Trampoline[A]
  case class More[A](f: () => Trampoline[A]) extends Trampoline[A]
  case class FlatMap[A, B](ta: Trampoline[A], f: A => Trampoline[B]) extends Trampoline[B]

  def resume[A](ta: Trampoline[A]): Either[() => Trampoline[A], A] = ta match {
    case Done(a) => Right(a)
    case More(thunk) => resume(thunk())
    case FlatMap(t, f) => t match {
      case Done(a2) => resume(f(a2))
      case More(thunk2) => Left(() => FlatMap(thunk2(), f))
      case FlatMap(t2, f2) => resume(FlatMap(t2, (x: Any) => FlatMap(f2(x), f)))
    }
  }

  // FlatMap(FlatMap(t2, f2), f) ---> FlatMap(t2, x => FlatMap(f2(x), f))

  @tailrec
  def run[A](ta: Trampoline[A]): A = resume(ta) match {
    case Right(a) => a
    case Left(thunk) => run(thunk())
  }
}

object X {
  import Trampoline._

  def isEven(n: Int): Trampoline[Boolean] =
    if(n == 0) Done(true)
    else More(() => isOdd(n - 1))

  def isOdd(n: Int): Trampoline[Boolean] =
    if(n == 0) Done(false)
    else More(() => isEven(n - 1))
}


import X._
import Trampoline._

def flatMap[A, B](as: List[A])(f: A => List[B]): Trampoline[List[B]] =
  as match {
    case Nil => Done(Nil)
    case (h :: t) => More { () =>
      FlatMap(flatMap(t)(f), (lb: List[B]) => Done(f(h) ::: lb))
    }
  }

run(flatMap((1 to 100000).toList)(i => List(i, i+1)))