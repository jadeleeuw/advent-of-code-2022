package days

import scala.io.BufferedSource

object DayFour {

  def partOne(source: BufferedSource): Int = {
    source.getLines()
      .flatMap(s => splitInput(s))
      .map(i => (inputToRange(i._1).get, inputToRange(i._2).get))
      .map(rs => overlappingRange(rs._1, rs._2))
      .count(b => b)
  }

  def partTwo(source: BufferedSource): Int = {
    source.getLines()
      .flatMap(s => splitInput(s))
      .map(i => (inputToRange(i._1).get, inputToRange(i._2).get))
      .map(rs => noRangeOverlap(rs._1, rs._2))
      .count(b => !b)
  }

  def splitInput(input: String): Option[(String, String)] =
    input.split(',') match {
      case Array(r1, r2) => Some((r1, r2))
      case _ => None
    }

  def inputToRange(input: String): Option[Range] =
    input.split('-') match {
      case Array(start, end) => Some(Range.inclusive(start.toInt, end.toInt))
      case _ => None
    }

  def overlappingRange(range1: Range, range2: Range): Boolean = {
    val (r1, r2) = range1 match {
      case _ if range1.start < range2.start => (range1, range2)
      case _ if range1.start == range2.start => if (range1.end > range2.end) (range1, range2) else (range2, range1)
      case _ => (range2, range1)
    }
    r1.start <= r2.start && r1.end >= r2.end
  }

  def noRangeOverlap(range1: Range, range2: Range): Boolean = {
    range2.end < range1.start || range2.start > range1.end
  }
}
