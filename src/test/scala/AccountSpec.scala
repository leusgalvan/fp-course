import cats._
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  implicit def byIdOrder(implicit longOrder: Order[Long]): Order[Account] =
    Order.from((acc1, acc2) => longOrder.compare(acc1.id, acc2.id))

  object Instances {
    implicit val byNumberOrder: Order[Account] = Order.by(acc => acc.number)
    implicit val byBalanceOrder: Order[Account] = Order.by(acc => acc.balance)
  }
}

class AccountSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {
  implicit val arbAccount: Arbitrary[Account] = Arbitrary {
    for {
      id <- Gen.long
      number <- Gen.alphaNumStr
      balance <- Gen.double
      owner <- Gen.alphaNumStr
    } yield Account(id, number, balance, owner)
  }

  val f: Account => Account = account => account.copy(balance = account.balance + 1000.0)
  val f2: Account => Account = account => account.copy(owner = account.owner.toLowerCase)
  implicit val arbAccToAcc: Arbitrary[Account => Account] = Arbitrary {
    Gen.oneOf(f, f2)
  }

  //checkAll("accountEqTests", AccountEqTests.eqv)
}
