case class User(username: String, password: String)

trait UserRepo {
  def findUser(userId: Long): Option[User]
}

trait UserService { this: UserRepo =>
  def findUser(userId: Long): Option[User]
}

case class Account(accountNumber: String, user: User)

trait AccountService { this: UserService =>
  def openAccount(accountNumber: String, userId: Long): Option[Account]
}



