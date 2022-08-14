import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  implicit def orderById(implicit orderLong: Order[Long]): Order[Account] = Order.from((a1, a2) => orderLong.compare(a1.id, a2.id))

  object Instances {
    implicit val orderByNumber: Order[Account] = Order.by(account => account.number)
    // provide an instance of Order[Account] that orders by balance
  }
}