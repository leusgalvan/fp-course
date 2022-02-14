import cats._
import cats.implicits._
import cats.laws.discipline.FunctorTests
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

class Secret[A](val value: A) {
  private def hashed: String = {
    val s = value.toString
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    val d = MessageDigest.getInstance("SHA-1")
    val hashBytes = d.digest(bytes)
    new String(hashBytes, StandardCharsets.UTF_8)
  }

  override def toString: String = hashed
}

object Secret {
  implicit def secretEq[A: Eq]: Eq[Secret[A]] = Eq.instance((s1, s2) => Eq[A].eqv(s1.value, s2.value))

  implicit val secretFunctor = new Functor[Secret] {
    override def map[A, B](fa: Secret[A])(f: A => B): Secret[B] =
      new Secret(f(fa.value))
  }
}

class SecretSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline{
  implicit val arbSecret: Arbitrary[Secret[String]] = Arbitrary {
    for {
      name <- Gen.alphaNumStr
    } yield new Secret(name)
  }

  checkAll("FunctorTests[Secret]", FunctorTests[Secret].functor[String, String, String])
}
