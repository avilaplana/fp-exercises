package com.games.tictactoe

import cats.Monad
import cats.syntax.all._
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
  }

  sealed trait State

  case object InProgress extends State

  case object Draw extends State

  case class Winner(color: Color) extends State

  case class Position(row: Int, column: Int)

}

abstract class TicTacToe[F[_] : Monad] {

  val F = Monad[F]

  def initBoard(implicit m: Monad[F]): F[BoardGame] = m.pure(BoardGame.emptyBpard)

  def chooseStartColor: F[Color] = F.pure {
    scala.io.StdIn.readLine("Choose color:").toLowerCase match {
      case "black" => Black
      case "white" => White
      case _ => ??? // TODO resolve it
    }
  }

  def changeColor(color: Color): F[Color] = if (color == Black) F.pure(White) else F.pure(Black)

  def selectPosition: F[Position] = F.pure { // TODO regular expression
    scala.io.StdIn.readLine("Choose position:") match {
      case "00" => Position(0, 0)
      case "01" => Position(0, 1)
      case "02" => Position(0, 2)
      case "10" => Position(1, 0)
      case "11" => Position(1, 1)
      case "12" => Position(1, 2)
      case "20" => Position(2, 0)
      case "21" => Position(2, 1)
      case "22" => Position(2, 2)
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
    color <- chooseStartColor
  } yield (initialBoard, color)

  def playGame(boardGame: BoardGame, color: Color): F[State] =
    for {
      position <- selectPosition
      boardGame <- placeColor(boardGame)(color, position)
      gameState <- state(boardGame)
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


