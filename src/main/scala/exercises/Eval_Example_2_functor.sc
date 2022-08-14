import cats._
import cats.implicits._
import cats.data._

case class Stream[+A](head: A, tail: Eval[Stream[A]]) {
  def take(n: Int): Eval[List[A]] =
    if(n == 0) Eval.now(Nil)
    else tail.flatMap(sa => sa.take(n-1)).map(rest => head :: rest)
}

object Stream {
  def iterate[A](initial: A)(f: A => A): Stream[A] =
    Stream(initial, Eval.later(iterate(f(initial))(f)))

  implicit val streamFunctor: Functor[Stream] = new Functor[Stream] {
    override def map[A, B](fa: Stream[A])(f: A => B): Stream[B] = ???
  }
}
import Stream._
val nats = iterate(1)(_ + 1)
nats.take(100000).value