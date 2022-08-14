import cats._
import cats.implicits._
import cats.data._

case class Stream[+A](head: A, tail: Eval[Stream[A]]) {
  def take(n: Int): Eval[List[A]] = ???
}

object Stream {
  def iterate[A](initial: A)(f: A => A): Stream[A] =
    Stream(initial, Eval.later(iterate(f(initial))(f)))
}

import Stream._
val nats = iterate(1)(_ + 1)