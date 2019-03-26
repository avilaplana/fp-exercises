package com.games.hangman
import cats.Id
import com.games.hangman.domain.Attempt.{Eight, One, Seven}
import com.games.hangman.domain.GameError.ParsingError
import com.games.hangman.domain.State.{Hang, InProgress, Winner}
import com.games.hangman.domain.{Console, Paper}
import org.scalatest.{EitherValues, Matchers, WordSpec}

class HangmanSpec extends WordSpec with Matchers with EitherValues {

  implicit val consoleId = new Console[Id]
  val hangman = new Hangman[Id] {}

  "upatePaperWithLetter" should {
    "update paper with the new letter" in {
      val paper = Paper.init("holah")
      hangman.updatePaper(paper, 'h') shouldBe paper.copy(guess = Seq(Some('h'), None, None, None, Some('h')))
    }
  }

  "gameState" should {
    "return the state Winner when the guess is the right word" in {
      val paper = Paper.init("holah")
      hangman.gameState(paper, "holah") shouldBe (Winner, paper)
    }

    "return the state InProgress and the paper with another try failure when the guess is not the right" in {
      val paper = Paper.init("holah")
      hangman.gameState(paper, "whatever") shouldBe (InProgress, paper.copy(attempt = One))
    }

    "return the state Hung when the guess is not the right and is last attempt" in {
      val paper = Paper.init("holah").copy(attempt = Seven)
      hangman.gameState(paper, "whatever") shouldBe (Hang, paper.copy(attempt = Eight))
    }
  }

  "validate" should {
    "return a char if is a alphabet's letter" in  {
      hangman.validate("a").right.value shouldBe 'a'
    }

    "return a error if is not an alphabet's letter" in  {
      hangman.validate("1").left.value shouldBe ParsingError
    }
  }
}
