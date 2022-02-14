import cats._
import cats.data._
import cats.implicits._


trait MList[+A]
object MList {
  case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]
  case object MNil extends MList[Nothing]

  def apply[A](elems: A*) = {
    elems.foldRight(mnil[A])((a, b) => mcons(a, b))
  }

  def mnil[A]: MList[A] = MNil
  def mcons[A](hd: A, tl: MList[A]): MList[A] = MCons(hd, tl)

  implicit val traverseMList: Traverse[MList] = new Traverse[MList] {
    override def traverse[G[_], A, B](fa: MList[A])(f: A => G[B])(implicit G: Applicative[G]): G[MList[B]] = {
//      fa match {
//        case MNil => G.pure(MNil)
//        case MCons(h, t) => (f(h), traverse(t)(f)).mapN(MCons.apply)
//      }
    }

    override def foldLeft[A, B](fa: MList[A], b: B)(f: (B, A) => B): B = fa match {
      case MNil => b
      case MCons(h, t) => foldLeft(t, f(b, h))(f)
    }

    override def foldRight[A, B](fa: MList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {
      case MNil => lb
      case MCons(h, t) => foldRight(t, f(h, lb))(f)
    }

    override def sequence[G[_], A](fga: MList[G[A]])(implicit G: Applicative[G]): G[MList[A]] =
      traverse(fga)(identity)
  }
}
import MList._

Traverse[MList].traverse(MList(1, 2, 3)) { i =>
  if(i % 2 == 0) Option("even") else Option("odd")
}

type MapString[A] = Map[String, A]
implicit val traverseMap: Traverse[MapString] = new Traverse[MapString] {
  override def traverse[G[_], A, B](fa: MapString[A])(f: A => G[B])(implicit G: Applicative[G]): G[MapString[B]] =


  override def foldLeft[A, B](fa: MapString[A], b: B)(f: (B, A) => B): B = ???

  override def foldRight[A, B](fa: MapString[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
}