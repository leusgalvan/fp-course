import cats._
import cats.implicits._
import cats.data.{Reader, _}

trait DbResultSet

trait SqlDecoder[A] {
  def fromResultSet(result: DbResultSet): A
}

object SqlDecoder {
  def apply[A](implicit ev: SqlDecoder[A]): SqlDecoder[A] = ev
  def instance[A](f: DbResultSet => A): SqlDecoder[A] = new SqlDecoder[A] {
    override def fromResultSet(result: DbResultSet): A = f(result)
  }
}

case class Query(query: String) {
  def withLongParam(p: Long): Query = this
}

case class Account(id: Long, balance: Double) {
  def deposit(amount: Double): Account =
    copy(balance = balance + amount)

  def withdraw(amount: Double): Account =
    copy(balance = balance - amount)
}

object Account {
  implicit val sqlDecoder: SqlDecoder[Account] = SqlDecoder.instance(rs => Account(1, 2000))
}

case class Statement(statement: String) {
  def withLongParam(l: Long): Statement = this
  def withDoubleParam(d: Double): Statement = this
}

trait DbClient {
  def executeStatement(sql: Statement): Boolean
  def executeQuery[A: SqlDecoder](sql: Query): A
}

object LiveDbClient extends DbClient {
  def executeStatement(sql: Statement): Boolean = true
  def executeQuery[A: SqlDecoder](sql: Query): A = SqlDecoder[A].fromResultSet(new DbResultSet{})
}

trait AccountService {
  def save(account: Account): Reader[DbClient, Boolean]
  def delete(id: Long): Reader[DbClient, Boolean]
  def findById(id: Long): Reader[DbClient, Account]
  def transferFunds(sourceId: Long, destId: Long, amount: Double): Reader[DbClient, Boolean]
  def update(id: Long, upd: Account => Account): Reader[DbClient, Boolean]
}

object LiveAccountService extends AccountService{
  val getDbClient: Reader[DbClient, DbClient] = Reader(identity)

  def save(account: Account): Reader[DbClient, Boolean] =
    for {
      dbClient <- getDbClient
      result = dbClient.executeStatement(
        Statement(s"insert into account(id, balance) values(?, ?)")
          .withLongParam(account.id)
          .withDoubleParam(account.balance)
      )
    } yield result

  def delete(id: Long): Reader[DbClient, Boolean] =
    for {
      dbClient <- getDbClient
      result = dbClient.executeStatement(
        Statement(s"delete from account where id = ?")
          .withLongParam(id)
      )
    } yield result

  def findById(id: Long): Reader[DbClient, Account] =
    for {
      dbClient <- getDbClient
      account = dbClient.executeQuery[Account](Query("select id, balance"))
    } yield account

  def update(id: Long, upd: Account => Account): Reader[DbClient, Boolean] =
    for {
      account <- findById(id)
      success <- save(upd(account))
    } yield success

  def transferFunds(sourceId: Long, destId: Long, amount: Double): Reader[DbClient, Boolean] =
    for {
      sourceAccount <- findById(sourceId)
      destAccount <- findById(destId)
      sourceSuccessful <- save(sourceAccount.withdraw(amount))
      destSuccessful <- save(destAccount.deposit(amount))
    } yield sourceSuccessful && destSuccessful
}

//case class Client(id: Long, email: String, account: Account)
//
//trait EmailService {
//  def sendEmail(to: String, text: String): Boolean
//}
//
//trait ClientService {
//  def findById(id: Long): Reader[DbClient, Client] =
//    for {
//      dbClient <- Reader[DbClient, DbClient](identity)
//      client = dbClient.executeQuery[Client]("select * from clients where id = ?", id.show)
//    } yield client
//
//  private def delete(id: Long): Reader[DbClient, Boolean] = ???
//
//  def unsuscribe(id: Long): Reader[DbClient with AccountService with EmailService, Boolean] = for {
//    client <- findById(id)
//    accountService <- Reader[AccountService, AccountService](identity)
//    accDeleted <- accountService.delete(client.account.id)
//    dbClient <- Reader[DbClient, DbClient](identity)
//    clientDeleted = dbClient.executeSQL("delete from clients where id = ?", client.id.show)
//    emailService <- Reader[EmailService, EmailService](identity)
//    emailSent = emailService.sendEmail(client.email, "Your account was deleted")
//  } yield accDeleted && clientDeleted && emailSent
//}
//
//object LiveClientService extends ClientService
//
//trait Env {
//
//}