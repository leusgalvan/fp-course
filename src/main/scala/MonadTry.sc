import cats._
import cats.implicits._
import scala.util._

// pure(x).flatMap(f) === f(x)
val f: Int => Try[Int] = i => throw new Exception("boom")
// Success(10).flatMap(f)
f(10)