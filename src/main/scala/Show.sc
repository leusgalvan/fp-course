import cats._
import cats.implicits._

case class Account(id: Long, number: String, balance: Double, owner: String)

object Account {
  implicit val toStringShow: Show[Account] = Show.fromToString

  object Instances {
    implicit val byOwnerAndBalance: Show[Account] = Show.show { account =>
      s"${account.owner} - $$${account.balance}"
    }

    // Write an instance of show which will output something like 'This account belongs to Leandro'
    implicit val prettyByOwner: Show[Account] = Show.show { account =>
      s"This account belongs to ${account.owner}"
    }
  }
}

val leandro = Account(1, "123-45", 2000, "Leandro")
Account.toStringShow.show(leandro)
Account.Instances.byOwnerAndBalance.show(leandro)
Account.Instances.prettyByOwner.show(leandro)

import Account.Instances.prettyByOwner
leandro.show