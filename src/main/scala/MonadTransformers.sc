import cats._
import cats.implicits._
import cats.data._

trait AccountRepo
type ErrorOr[A] = Either[String, A]
type AccountOp[A] = ReaderT[ErrorOr, AccountRepo, A]
val dummyRepo: AccountRepo = new AccountRepo {}

// ReaderT == Kleisli

// ErrorOr[B] -> ReaderT[ErrorOr, A, B]
ReaderT.liftF[ErrorOr, AccountRepo, Int](5.asRight[String]).run(dummyRepo)
ReaderT.liftF[ErrorOr, AccountRepo, Int]("hello".asLeft[Int]).run(dummyRepo)

// B -> ReaderT[ErrorOr, A, B]
5.pure[AccountOp].run(dummyRepo)

// ReaderT[ErrorOr, A, B] -> ReaderT[Option, A, B]
5.pure[AccountOp].mapF {
  case Right(x) => Some(x)
  case Left(s) => None
}.run(dummyRepo)

ReaderT((_: AccountRepo) => 5.asRight[String]).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int]).run(dummyRepo)

5.pure[AccountOp].map(_ + 1).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int]).map(_ + 1).run(dummyRepo)

5.pure[AccountOp].flatMap(n => (n+1).pure[AccountOp]).run(dummyRepo)
ReaderT((_: AccountRepo) => "hello".asLeft[Int]).flatMap(n => (n+1).pure[AccountOp]).run(dummyRepo)

type ErrorOrOpt[A] = OptionT[ErrorOr, A] // ErrorOr[Option[A]] -> Either[String, Option[A]]
type AccountOp2[A] = ReaderT[ErrorOrOpt, AccountRepo, A]

5.pure[AccountOp2].flatMap(n => (n + 1).pure[AccountOp2]).run(dummyRepo).value

5.pure[ErrorOrOpt].flatMap(n => (n + 1).pure[ErrorOrOpt])
OptionT(Option(5).asRight[String]).flatMap(n => (n + 1).pure[ErrorOrOpt])
OptionT(Option.empty[Int].asRight[String]).flatMap(n => (n + 1).pure[ErrorOrOpt])
OptionT("boom".asLeft[Option[Int]]).flatMap(n => (n + 1).pure[ErrorOrOpt])

OptionT(Option(5).asRight[String]).subflatMap(n => Option(n+1))
OptionT("boom".asLeft[Option[Int]]).subflatMap(n => Option(n+1))
OptionT(Option.empty[Int].asRight[String]).subflatMap(n => Option(n+1))

OptionT(Option(5).asRight[String]).semiflatMap(n => Right(n+1))

OptionT(Option(5).asRight[String]).flatMapF(n => Right(Some(n+1)))

