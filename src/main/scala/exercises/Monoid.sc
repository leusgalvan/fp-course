import cats._
import cats.implicits._

case class Speed(metersPerSecond: Double) {
  def kilometersPerSec: Double = metersPerSecond / 1000.0
  def milesPerSec: Double = metersPerSecond / 1609.34
}

object Speed {
  def addSpeeds(s1: Speed, s2: Speed): Speed =
    Speed(s1.metersPerSecond + s2.metersPerSecond)

  implicit val eqSpeed: Eq[Speed] = Eq.fromUniversalEquals
  implicit val monoidSpeed: Monoid[Speed] = Monoid.instance(Speed(0), addSpeeds)
}

Monoid[Speed].combine(Speed(1000), Speed(2000))
Monoid[Speed].empty
Monoid[Speed].combine(Speed(1000), Monoid[Speed].empty)
Speed(1000) |+| Speed(2000)
Monoid[Speed].combineAll(List(Speed(100), Speed(200), Speed(300)))
List(Speed(100), Speed(200), Speed(300)).combineAll
Monoid[Speed].isEmpty(Speed(100))
Monoid[Speed].isEmpty(Speed(0))

val sumMonoid: Monoid[Int] = ???
val minMonoid: Monoid[Int] = ???
def listMonoid[A]: Monoid[List[A]] = ???
val stringMonoid: Monoid[String] = ???