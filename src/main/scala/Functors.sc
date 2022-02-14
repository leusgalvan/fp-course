import cats._
import cats.implicits._
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

val optionFunctor: Functor[Option] = new Functor[Option] {
  override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
    fa match {
      case None => None
      case Some(a) => Some(f(a))
    }
}

val listFunctor: Functor[List] = new Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B): List[B] =
    fa match {
      case Nil => Nil
      case hd :: tl => f(hd) :: map(tl)(f)
    }
}

optionFunctor.map(Some(3))(_ + 1)
listFunctor.map(List(1, 2, 3))(_ * 2)

listFunctor.as(List(1, 3, 5), 10)
optionFunctor.as(Some(4), 11)