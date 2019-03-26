package com.games.hangman

import cats.data.EitherT
import cats.syntax.all._
import cats.{Monad, Show}
import com.games.hangman.domain.Attempt._
import com.games.hangman.domain.GameError.ParsingError
import com.games.hangman.domain.State.{Hang, InProgress, Winner}
import com.games.hangman.domain.{Console, GameError, Paper, State, Word}

import scala.concurrent.{Await, Future}

object domain {

  type Word = String

  sealed trait Attempt

  object Attempt {

    case object Zero extends Attempt

    case object One extends Attempt

    case object Two extends Attempt

    case object Three extends Attempt

    case object Four extends Attempt

    case object Five extends Attempt

    case object Six extends Attempt

    case object Seven extends Attempt

    case object Eight extends Attempt

  }

  case class Paper(word: Word, attempt: Attempt, guess: Seq[Option[Char]])

  object Paper {
    def init(word: Word) = Paper(word, Zero, List.fill(word.size)(None))

    implicit val paperShow = new Show[Paper] {
      override def show(t: Paper): String =
        s"""
            | Attempts: ${t.attempt}
            | ${t.guess.map { l =>
             l.getOrElse(" _ ")
           }.mkString}
         """.stripMargin
    }
  }

  sealed trait State

  object State {

    case object InProgress extends State

    case object Winner extends State

    case object Hang extends State

  }

  sealed trait GameError

  object GameError {
    case object ParsingError extends GameError
  }

  implicit class ShowOps[T](t: T) {
    def show(implicit T: Show[T]) = T.show(t)
  }

  class Console[F[_]: Monad] {
    def printLine(line: String): F[Unit] = Monad[F].pure(println(line))

    def readLine: F[String] = Monad[F].pure(scala.io.StdIn.readLine)
  }

  object Console {
    def apply[F[_]](implicit console: Console[F]): Console[F] = console
  }

}

abstract class Hangman[F[_]: Monad: Console] {

  val alphabetRegex = "^([a-zA-Z])$".r

  val F = Monad[F]

  def pickAWord: F[Word] = F.pure("hola") // reader monad

  def updatePaper(paper: Paper, letter: Char): F[Paper] = { // this should be unit test
    val newGuess =
      paper.word.zipWithIndex
        .collect { case (let, position) if let == letter => position }
        .foldRight(paper.guess) { (index, b) =>
          b.patch(index, Seq(Some(letter)), 1)
        }
    F.pure(paper.copy(guess = newGuess))
  }

  def validate(candidate: String): F[Either[GameError, Char]] =
    candidate match {
      case alphabetRegex(letter) => F.pure(Right(letter.head))
      case _                     => F.pure(Left(ParsingError))
    }

  def chooseLetter: F[Char] = {
    val nw = for {
      _         <- EitherT.right(Console[F].printLine("Please, choose a letter:"))
      letter    <- EitherT.right(Console[F].readLine)
      validated <- EitherT(validate(letter))
    } yield validated

    nw.value.flatMap { // TODO REFACTOR THIS
      case Left(e)        => chooseLetter
      case Right(n: Char) => F.pure(n)
    }
  }

  def guessWord: F[String] =
    for {
      _    <- Console[F].printLine("Guess the word:")
      word <- Console[F].readLine
    } yield word

  def gameState(paper: Paper, guess: String): F[(State, Paper)] =
    if (paper.word == guess)
      F.pure((Winner, paper))
    else {
      val (s, p) = paper.attempt match {
        case Zero  => (InProgress, paper.copy(attempt = One))
        case One   => (InProgress, paper.copy(attempt = Two))
        case Two   => (InProgress, paper.copy(attempt = Three))
        case Three => (InProgress, paper.copy(attempt = Four))
        case Four  => (InProgress, paper.copy(attempt = Five))
        case Five  => (InProgress, paper.copy(attempt = Six))
        case Six   => (InProgress, paper.copy(attempt = Seven))
        case Seven => (Hang, paper.copy(attempt = Eight))
      }
      F.pure((s, p))
    }

  def playGame(paper: Paper): F[State] =
    for {
      _                  <- Console[F].printLine(paper.show)
      letter             <- chooseLetter
      paperWithNewLetter <- updatePaper(paper, letter)
      _                  <- Console[F].printLine(paperWithNewLetter.show)
      guess              <- guessWord
      stateAndPaper      <- gameState(paperWithNewLetter, guess)
      state              <- if (stateAndPaper._1 == InProgress) playGame(stateAndPaper._2) else F.pure(stateAndPaper._1)
    } yield state

  def startGame: F[Unit] =
    for {
      word  <- pickAWord
      state <- playGame(Paper.init(word))
    } yield
      state match {
        case Winner => Console[F].printLine("Congratulations, you have won!!!!")
        case Hang   => Console[F].printLine("You have lost, next time will be better!!!!")
        case _      => Console[F].printLine("Definitively there is a bug")
      }
}

object IOHangmand extends App {
  import cats.effect.IO
  implicit val ioConsole = new Console[IO]

  val hangman = new Hangman[IO] {}

  hangman.startGame
    .unsafeRunSync()
}

object FutureHangmand extends App {
  import cats.instances.future._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  implicit val futureConsole = new Console[Future]

  val hangman = new Hangman[Future] {}

  Await.result(hangman.startGame, 60 seconds)
}
