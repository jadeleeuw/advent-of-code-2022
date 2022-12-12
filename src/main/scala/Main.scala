import days.DayFour.{partOne, partTwo}

@main def main: Unit =
  val input = scala.io.Source.fromResource("inputs/day-4.txt")
  println(partTwo(input))
