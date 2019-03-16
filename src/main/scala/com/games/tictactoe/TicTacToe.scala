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


sealed trait BoardError

case object PoistionBusyError extends BoardError

sealed trait ParserError

case object PoistionParseError extends ParserError

case object ColorParseError extends ParserError

trait Parser[T] {
  def parse(s: String): Either[ParserError, T]
}


object Parser {

  def apply[T](implicit pr: Parser[T]) = pr

  implicit val positionParser: Parser[Position] = new Parser[Position] {
    override def parse(s: String): Either[ParserError, Position] = {
      val positionRegex = "^([0-2])([0-2])$".r
      s match {
        case positionRegex(row, column) => Right(Position(row.toInt, column.toInt))
        case _ => Left(PoistionParseError)
      }
    }
  }

  implicit val colorParser: Parser[Color] = new Parser[Color] {
    override def parse(s: String): Either[ParserError, Color] = {
      val colorRegex = "^(black|white)$".r
      s match {
        case colorRegex("black") => Right(Black)
        case colorRegex("white") => Right(White)
        case _ => Left(ColorParseError)
      }
    }
  }
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

  def parseStartColor(color: String)(implicit pr: Parser[Color]): F[Either[ParserError, Color]] =
    F.pure(pr.parse(color))

  def parsePosition(positionStr: String)(implicit pr: Parser[Position]): F[Either[ParserError, Position]] =
    F.pure(pr.parse(positionStr))

  def changeColor(color: Color): F[Color] = if (color == Black) F.pure(White) else F.pure(Black)

  def placeColor(boardGame: BoardGame)(color: Color, position: Position): F[Either[BoardError, BoardGame]] =
    F.pure {
      val board: List[List[Option[Color]]] = boardGame.board
      board(position.row)(position.column) match {
        case Some(cc) => Left(PoistionBusyError)
        case None =>
          val row = board(position.row)
          val rowUpdated: List[Option[Color]] = row.updated(position.column, Some(color))
          Right(BoardGame(board.updated(position.row, rowUpdated)))
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
    color <- parseStartColor(colorStr)
  } yield (initialBoard, color.getOrElse(???)) // TODO fix this

  def playGame(boardGame: BoardGame, color: Color): F[State] =
    for {
      _ <- Console[F].printLine(s"$color chose a position:")
      positionStr <- Console[F].readLine
      position <- parsePosition(positionStr)
      boardGame <- placeColor(boardGame)(color, position.getOrElse(???)) //TODO  fix this
      gameState <- state(boardGame.getOrElse(???)) //TODO  fix this
      _ <- Console[F].printLine(s"the state of the board is $gameState: ${boardGame.getOrElse(???).show}") //TODO  fix this
      newColor <- changeColor(color)
      st <- if (gameState == InProgress) playGame(boardGame.getOrElse(???), newColor) else F.pure(gameState) //TODO  fix this
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


