import cats._
import cats.implicits._
import cats.data._

val signReader: Reader[Int, String] = Reader(n => if(n > 0) "positive" else if(n < 0) "negative" else "zero")
signReader.run(0)

val parityReader: Reader[Int, String] = Reader(n => if(n % 2 == 0) "even" else "odd")
parityReader.run(1)

val descriptionReader: Reader[Int, String] =
  for {
    sign <- signReader
    parity <- parityReader
  } yield s"$sign and $parity"
descriptionReader.run(-2)

val addOneReader: Reader[Int, Int] =
  for {
    env <- Reader(identity[Int])
  } yield env + 1

case class Person(id: Long, name: String, emailAddress: String)
case class Account(id: Long, ownerId: Long)

trait AccountRepository {
  val accountRepository: Service

  trait Service {
    def findAccountById(id: Long): Account
    def saveAccount(account: Account): Unit
  }
}

trait LiveAccountRepository extends AccountRepository {
  override val accountRepository: Service = new Service {
    override def findAccountById(id: Long): Account = Account(id, 2)

    override def saveAccount(account: Account): Unit = ()
  }
}

trait PersonRepository {
  val personRepository: Service

  trait Service {
    def findPersonById(id: Long): Person
  }
}

trait LivePersonRepository extends PersonRepository {
  override val personRepository: Service = new Service {
    override def findPersonById(id: Long): Person = Person(2, "leandro", "leandro@mail.com")
  }
}

def findNextAccount(id: Long): Reader[AccountRepository, Account] =
  for {
    accountRepository <- Reader(identity[AccountRepository])
    account = accountRepository.accountRepository.findAccountById(id + 1)
  } yield account

def findOwnerNameByAccountId(id: Long): Reader[PersonRepository with AccountRepository, String] =
  for {
    accountModule <- Reader(identity[AccountRepository])
    personModule <- Reader(identity[PersonRepository])
    account = accountModule.accountRepository.findAccountById(id)
    owner = personModule.personRepository.findPersonById(account.ownerId)
  } yield owner.name

trait EmailService {
  val emailService: Service

  trait Service {
    def sendEmail(address: String, text: String): Unit
  }
}

trait LiveEmailService extends EmailService {
  override val emailService: Service = new Service {
    override def sendEmail(address: String, text: String): Unit = ()
  }
}

type Env = PersonRepository with AccountRepository with EmailService
val liveEnv: Env = new LivePersonRepository with LiveAccountRepository with LiveEmailService

findOwnerNameByAccountId(1).run(liveEnv)

def openAccount(accountId: Long, owner: Long): Reader[PersonRepository with AccountRepository with EmailService, Account] =
  for {
    accountRepository <- Reader(identity[AccountRepository])
    emailService <- Reader(identity[EmailService])
    personRepository <- Reader(identity[PersonRepository])
    account = Account(accountId, owner)
    _ = accountRepository.accountRepository.saveAccount(account)
    person = personRepository.personRepository.findPersonById(account.ownerId)
    _ = emailService.emailService.sendEmail(person.emailAddress, "You have a new account!")
  } yield account

openAccount(1, 1).run(liveEnv)

// 1. Define an EmailService module with a method sendEmail(address: String, text: String): Unit
// 2. Define a LiveEmailService trait with a dummy implementation for the module
// 3. Update Env and liveEnv to include this new module
// 4. Add an emailAddress field to the Person class and change the PersonRepository implementation accordingly
// 5. Add a saveAccount(account: Account): Unit method to the AccountRepository module
// 6. Add a dummy implementation for saveAccount in LiveAccountRepository
// 7. Implement an openAccount(accountId, ownerId) method that will create and save a new account,
// 8. and will notify the user via email. Use the reader monad, and the dependencies that you see fit.
// 9. Run the function using the live environment