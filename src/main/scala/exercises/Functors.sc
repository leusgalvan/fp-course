import cats._

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
  implicit val secretFunctor = new Functor[Secret] {
    override def map[A, B](fa: Secret[A])(f: A => B): Secret[B] =
      new Secret(f(fa.value))
  }
}

val leandroSecret: Secret[String] = new Secret("leandro")
leandroSecret.value

val upperLeandroSecret = Functor[Secret].map(leandroSecret)(_.toUpperCase)
upperLeandroSecret.value

val optionFunctor: Functor[Option] = ???
val listFunctor: Functor[List] = ???