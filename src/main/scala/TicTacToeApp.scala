import cats._
import cats.implicits._
import cats.data._

import scala.io.{Source, StdIn}

object TicTacToeApp {
  sealed trait Player
  object Player {
    case object P1 extends Player
    case object P2 extends Player

    implicit val showPlayer: Show[Player] = Show.show {
      case P1 => "X"
      case P2 => "O"
    }

    implicit val showOptPlayer: Show[Option[Player]] = Show.show {
      case None => "-"
      case Some(p) => p.show
    }

    implicit val eqPlayer: Eq[Player] = Eq.fromUniversalEquals[Player]
  }


  class Board private (sz: Int, board: Map[Board.Cell, Player]) {
    val size: Int = sz

    def updated(row: Int, col: Int, player: Player): Board =
      new Board(sz, board + ((row, col) -> player))

    def readCell(row: Int, col: Int): Option[Player] = board.get((row, col))

    def readRow(row: Int): List[Option[Player]] =
      List.range(0, sz).map(col => readCell(row, col))

    def readCol(col: Int): List[Option[Player]] =
      List.range(0, sz).map(row => readCell(row, col))

    def readDiags: (List[Option[Player]], List[Option[Player]]) = (
      List.range(0, sz).map(i => readCell(i, i)),
      List.range(0, sz).map(j => readCell(size - j - 1, size - j - 1))
    )
  }

  object Board {
    type Cell = (Int, Int)

    def empty(size: Int): Board = new Board(size, Map.empty[Cell, Player])

    implicit val showBoard: Show[Board] = Show.show { board =>
      (0 until board.size).map { row =>
        board.readRow(row).map(Player.showOptPlayer.show).mkString(" ")
      }.mkString("\n")
    }
  }

  case class GameState(
    turn: Player,
    board: Board,
    plays: List[Play]
  )

  object GameState {
    implicit val gameStateShow: Show[GameState] = Show.show { gs =>
      s"""Turn: ${gs.turn}
         |
         |Board:
         |${gs.board.show}
         |
         |Plays:
         |${gs.plays.reverse}
         |""".stripMargin
    }
  }
  case class Play(row: Int, col: Int)

  class TicTacToe(boardSize: Int) {
    type Game[A] = State[GameState, A]

    val initialState: GameState = GameState(Player.P1, Board.empty(boardSize), Nil)

    def switchTurns: Game[Unit] =
      State.modify[GameState] { gameState =>
        val newTurn = if(gameState.turn === Player.P1) Player.P2 else Player.P1
        gameState.copy(turn = newTurn)
      }

    def currentPlayerWon: Game[Boolean] =
      State.get[GameState].map { gameState =>
        val board = gameState.board
        val currentPlayer = gameState.turn
        List.range(0, boardSize).exists { i =>
          val (diag1, diag2) = board.readDiags
          board.readRow(i).forall(_ === Some(currentPlayer)) ||
          board.readCol(i).forall(_ === Some(currentPlayer)) ||
          diag1.forall(_ === Some(currentPlayer)) ||
          diag2.forall(_ === Some(currentPlayer))
        }
      }

    def doPlay(play: Play): Game[Unit] = {
      State.modify[GameState] { gameState =>
        val newBoard = gameState.board.updated(play.row, play.col, gameState.turn)
        gameState.copy(board = newBoard, plays = play :: gameState.plays)
      }
    }

    def readPlayFromConsole(turn: Player): Play = {
      println(s"Enter play $turn:")
      val n = StdIn.readInt()
      Play(n / 10, n % 10)
    }

    def game: State[GameState, Player] = {
      for {
        gameState     <- State.get[GameState]
        _              = println(gameState.board.show)
        play           = readPlayFromConsole(gameState.turn)
        _             <- doPlay(play)
        weHaveAWinner <- currentPlayerWon
        winner        <- if(weHaveAWinner) gameState.turn.pure[Game]
                         else switchTurns >> game
      } yield winner
    }

    def playGame: (GameState, Player) = game.run(
      initialState
    ).value

    def replayGame(plays: List[Play]): GameState = {
      plays
        .foldRight(State.get[GameState])((p, g) => doPlay(p) >> switchTurns >> g)
        .runS(initialState)
        .value
    }
  }

  def main(args: Array[String]): Unit = {
//    val (finalState, winner) = new TicTacToe(3).playGame
//    println(
//      s"""Winner: $winner
//        |Final board:
//        |${finalState.board.show}
//        |""".stripMargin)
    val plays = List(Play(0, 0), Play(1, 1), Play(0, 2))
    val result = new TicTacToe(3).replayGame(plays).show
    println(result)
  }
}

/*
Exercises:
- Keep track of the plays and modify playGame to return both the winner and the
  list of plays
- Write a show instance for GameState in the companion object
- Write a def replay(plays: List[Play]): GameState function inside TicTacToe that
  will execute the plays and return the final GameState
*/


