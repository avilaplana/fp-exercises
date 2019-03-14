package com.games.tictactoe

import cats.effect.IO
import cats.syntax.all._
import cats.{Monad, Show}
import com.games.tictactoe.domain._

object domain {

  sealed trait Color

  case object Black extends Color

  case object White extends Color

  case class BoardGame(board: List[List[Option[Color]]])

  object BoardGame {
    def emptyBpard = BoardGame(
      List(
        List.fill(3)(None),
        List.fill(3)(None),
        List.fill(3)(None)
      )
    )

    implicit val showBoard: Show[BoardGame] = (boardGame: BoardGame) => {
      val NewLine = "\n"
      boardGame.board.map {
        _.map {
          case None => "-"
          case Some(White) => "w"
          case Some(Black) => "b"
        }.mkString("|", "|", "|")
      }.mkString(NewLine, NewLine, NewLine)
    }
  }

  sealed trait State

  case object InProgress extends State

  case object Draw extends State

  case class Winner(color: Color) extends State

  case class Position(row: Int, column: Int)

}

class Console[F[_] : Monad] {
  def printLine(line: String): F[Unit] = Monad[F].pure(println(line))

  def readLine: F[String] = Monad[F].pure(scala.io.StdIn.readLine)
}

object Console {
  def apply[F[_]](implicit console: Console[F]): Console[F] = console

  implicit val consoleIO: Console[IO] = new Console[IO]
}

abstract class TicTacToe[F[_] : Monad : Console] {

  val F = Monad[F]

  def initBoard: F[BoardGame] = F.pure(BoardGame.emptyBpard)

  def chooseStartColor(color: String): F[Color] = F.pure {
    val colorRegex = "^(black|white)$".r
    color match {
      case colorRegex("black") => Black
      case colorRegex("white") => White
      case _ => ??? // TODO resolve it
    }
  }

  def changeColor(color: Color): F[Color] = if (color == Black) F.pure(White) else F.pure(Black)

  def selectPosition(positionStr: String): F[Position] = F.pure { // TODO regular expression
    val positionRegex = "^([0-2])([0-2])$".r
    positionStr match {
      case positionRegex(row, column) => Position(row.toInt, column.toInt)
      case _ => ??? // TODO resolve it
    }
  }

  def placeColor(boardGame: BoardGame)(color: Color, position: Position): F[BoardGame] =
    F.pure {
      val board: List[List[Option[Color]]] = boardGame.board
      board(position.row)(position.column) match {
        case Some(cc) => ??? // TODO resolve it
        case None =>
          val row = board(position.row)
          val rowUpdated: List[Option[Color]] = row.updated(position.column, Some(color))
          BoardGame(board.updated(position.row, rowUpdated))
      }
    }


  def state(boardGame: BoardGame): F[State] = F.pure {
    boardGame.board match {
      case List(List(Some(t1), Some(t2), Some(t3)), _, _) if t1 == t2 && t2 == t3 => Winner(t1)
      case List(_, List(Some(t1), Some(t2), Some(t3)), _) if t1 == t2 && t2 == t3 => Winner(t1)
      case List(_, _, List(Some(t1), Some(t2), Some(t3))) if t1 == t2 && t2 == t3 => Winner(t1)
      case List(List(Some(t1), _, _), List(Some(t2), _, _), List(Some(t3), _, _)) if t1 == t2 && t2 == t3 => Winner(t1)
      case List(List(_, Some(t1), _), List(_, Some(t2), _), List(_, Some(t3), _)) if t1 == t2 && t2 == t3 => Winner(t1)
      case List(List(_, _, Some(t1)), List(_, _, Some(t2)), List(_, _, Some(t3))) if t1 == t2 && t2 == t3 => Winner(t1)
      case List(List(Some(t1), _, _), List(_, Some(t2), _), List(_, _, Some(t3))) if t1 == t2 && t2 == t3 => Winner(t1)
      case List(List(_, _, Some(t1)), List(_, Some(t2), _), List(Some(t3), _, _)) if t1 == t2 && t2 == t3 => Winner(t1)
      case List(List(Some(_), Some(_), Some(_)), List(Some(_), Some(_), Some(_)), List(Some(_), Some(_), Some(_))) => Draw
      case _ => InProgress
    }
  }



  def startGame: F[(BoardGame, Color)] = for {
    initialBoard <- initBoard
    _ <- Console[F].printLine("Choose a color:")
    colorStr <- Console[F].readLine
    color <- chooseStartColor(colorStr)
  } yield (initialBoard, color)

  def playGame(boardGame: BoardGame, color: Color): F[State] =
    for {
      _ <- Console[F].printLine("Choose a position:")
      positionStr <- Console[F].readLine
      position <- selectPosition(positionStr)
      boardGame <- placeColor(boardGame)(color, position)
      gameState <- state(boardGame)
      _ <- Console[F].printLine(s"the state of the board is $gameState: ${boardGame.show}")
      newColor <- changeColor(color)
      st <- if (gameState == InProgress) playGame(boardGame, newColor) else F.pure(gameState)
    } yield st

  def game: F[State] = for {
    r <- startGame
    state <- playGame(r._1, r._2)
  } yield state
}


object Game extends App {

  import cats.effect.IO

  object TicTacToeIO extends TicTacToe[IO]

  TicTacToeIO
    .game
    .unsafeRunSync() match {
    case Draw => println("There was a Draw")
    case Winner(Black) => println("Black won!!!!")
    case Winner(White) => println("White won!!!!")
    case _ => println("There is an error")
  }

}


