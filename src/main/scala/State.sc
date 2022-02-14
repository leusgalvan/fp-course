import cats._
import cats.implicits._
import cats.data._

// State ==~ S => (S, A)

State[Int, Double](s => (s + 1, s.toDouble * 2))

type St[A] = State[Int, A]

8.pure[St]

val x = State[Int, Double](s => (s + 1, s.toDouble * 3))
x.run(4).value
x.runA(4).value
x.runS(4).value

State.get[Int].run(5).value
State.set[Int](10).run(1).value
State.modify[Int](s => s * 5).run(3).value
State.inspect[Int, String](s => s.toString + "!").run(2).value

def get[S]: State[S, S] = State(s => (s, s))
def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
def inspect[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))

val y = State[Int, Double](s => (s * 2, s.toDouble / 2))
x.map(d => d * 2.5).run(2).value
x.flatMap(d => State[Int, Double](s => (s * 2, d + 3))).run(1).value
(x, y).mapN((d1, d2) => d1 + d2).run(3).value

x.flatMap(_ => y)

val t1 = State.modify[Int](s => s * 2)
val t2 = State.modify[Int](s => s * 3)
(t1 >> t2).run(10).value
(t1 >> t2 >> 8.pure[St]).run(10).value