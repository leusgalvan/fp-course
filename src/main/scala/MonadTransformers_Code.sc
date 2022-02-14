import cats._
import cats.implicits._
import cats.data._

case class Account(id: Long, balance: Double) {
  def updateBalance(f: Double => Double) = copy(balance = f(balance))
}

trait AccountRepo

type ErrorOr[A] = Either[String, A]
type AccountOp[A] = ReaderT[ErrorOr, AccountRepo, A]
def findAccountById(id: Long): AccountOp[Account] = Account(id, 500).pure[AccountOp]
def saveAccount(account: Account): AccountOp[Account] = ReaderT((_: AccountRepo) => "oops".asLeft[Account])

def depositMoney(accountId: Long, amount: Double): AccountOp[Account] =
  for {
    account <- findAccountById(accountId)
    savedAccount <- saveAccount(account.updateBalance(_ + amount))
  } yield savedAccount

depositMoney(1, 1000 )
  .run(new AccountRepo{})