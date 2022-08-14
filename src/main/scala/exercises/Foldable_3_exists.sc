import cats._
import cats.implicits._

trait MList[+A]

object MList {
  def apply[A](elems: A*): MList[A] = {
    elems.foldRight(mnil[A])((a, b) => mcons(a, b))
  }

  case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]
  case object MNil extends MList[Nothing]

  def mnil[A]: MList[A] = MNil
  def mcons[A](hd: A, tl: MList[A]): MList[A] = MCons(hd, tl)

  implicit val listFoldable: Foldable[MList] = new Foldable[MList] {
    override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = fa match {
      case MNil => b
      case MCons(h, t) => foldLeft(t, f(b, h))(f)
    }

    override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: MList[A]): Eval[B] =
        as match {
          case MNil => lb
          case MCons(h, t) => f(h, Eval.defer(loop(t)))
        }
      Eval.defer(loop(fa))
    }
  }
}

import MList._

MList(1, 2, 3)

def sum(ints: MList[Int]): Int =
  ints.foldLeft(0)((b, a) => b + a)

def length[A](list: MList[A]): Int =
  list.foldLeft(0)((b, a) => 1 + b)

def filterPositive(ints: MList[Int]): MList[Int] =
  ints.foldRight(Eval.now(mnil[Int]))((i, eis) => if(i > 0) Eval.now(mcons(i, eis.value)) else eis).value

sum(MList(1, 2, 3))
length(MList(1,2,3,4,5))
filterPositive(MList(-1, 0, 1, 2))

MList(1, 2, 3).foldMap(_.show)
MList(1, 2, 3).foldMap(i => i * 2)
MList(1, 2, 3).fold
MList("hello", "world").fold

def find[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Option[A] =
  fa.foldLeft[Option[A]](None)((b, a) => if(p(a)) Some(a) else b)

find[MList, Int](MList(1, 3))(i => i % 2 == 0)

def exists[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean = ???