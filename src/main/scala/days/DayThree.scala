package days

import scala.io.BufferedSource

object DayThree {

  def partOne(source: BufferedSource): Int = {
    source.getLines()
      .map(s => findCommonItem(s))
      .map(c => getPriority(c))
      .sum
  }

  def partTwo(source: BufferedSource, elvesPerGroup: Int): Int = {
    source.getLines()
      .grouped(elvesPerGroup)
      .map(rs => findCommonItemsPerGroup(rs))
      .map(c => getPriority(c))
      .sum
  }

  def findCommonItem(rugSack: String): Char = {
    val (c1, c2) = rugSack.splitAt(rugSack.length / 2)
    c1.toCharArray.toSet.intersect(c2.toCharArray.toSet).head
  }

  def findCommonItemsPerGroup(rugSacks: Seq[String]): Char = {
    rugSacks.map(rs => rs.toCharArray.toSet)
      .reduce((s1, s2) => s1.intersect(s2))
      .head
  }

  def getPriority(char: Char): Int = {
    if (char.isLower) {
      ('a' to 'z').indexOf(char) + 1
    }
    else {
      ('A' to 'Z').indexOf(char) + 27
    }
  }
}

