package days

import scala.io.BufferedSource

object DayTwo {

  enum Hand:
    case Paper, Rock, Scissors

    def beats(): Hand = this match {
      case Paper => Rock
      case Rock => Scissors
      case Scissors => Paper
    }

  class GameRound(opponent: Hand, player: Hand) {

    def score(): Int = gameScore() + handScore()

    private def gameScore(): Int =
      (opponent, player) match {
        case (o, p) if player.beats().equals(opponent) => 6
        case (o, p) if player.equals(opponent) => 3
        case _ => 0
      }

    private def handScore(): Int = player match {
      case Hand.Rock => 1
      case Hand.Paper => 2
      case Hand.Scissors => 3
    }
  }

  def partOne(source: BufferedSource): Int = {
    source.getLines()
      .flatMap(s => parseInput(s))
      .map(i => (inputToHand(i._1), inputToHand(i._2)))
      .map(h => GameRound(h._1, h._2))
      .map(g => g.score())
      .sum
  }

  def partTwo(source: BufferedSource): Int = {
    source.getLines()
      .flatMap(s => parseInput(s))
      .map(i => {
        val opponent = inputToHand(i._1)
        (opponent, handToPlay(opponent, i._2))
      })
      .map(h => GameRound(h._1, h._2))
      .map(g => g.score())
      .sum
  }

  private def parseInput(input: String): Option[(String, String)] =
    input.split(' ').toList match {
      case opponent :: player :: Nil => Some((opponent, player))
      case _ => None
    }

  private def inputToHand(input: String): Hand = input match {
    case "A" | "X" => Hand.Rock
    case "B" | "Y" => Hand.Paper
    case "C" | "Z" => Hand.Scissors
  }

  private def handToPlay(opponent: Hand, playerInput: String): Hand =
    playerInput match {
      case "X" => opponent.beats()
      case "Y" => opponent
      case "Z" => Hand.values.find(h => h != opponent.beats() && h != opponent).get
    }
}
