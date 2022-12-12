package days

import scala.io.BufferedSource

object DayOne {
  def maxCaloriesBetweenElves(source: BufferedSource, numberOfElves: Int): Int = {
    sumCaloriesPerElf(source.getLines()).sorted.reverse.take(numberOfElves).sum
  }

  private def sumCaloriesPerElf(it: Iterator[String]): List[Int] = {
    val (oneElfInput, rest) = it.span(s => !s.isBlank)

    if (rest.hasNext) {
      // Remove the next blank line
      rest.next()
      List(oneElfInput.map(s => s.toInt).sum) ::: sumCaloriesPerElf(rest)
    } else {
      List(oneElfInput.map(s => s.toInt).sum)
    }
  }
}
