package day2

import scala.io.Source

object Day2B:

  def main(args: Array[String]): Unit =
    val data = Source
      .fromResource("day2/input.txt")
      .mkString
      .split(",")
      .toList
      .flatMap(_.split("-").toList match
        case first +: last +: Nil => List(first -> last)
        case _                    => Nil)

    val invalidIds = data
      .flatMap: (first, last) =>
        first.toLong.to(last.toLong).filter(isInvalid)

    println(invalidIds)
    println(invalidIds.sum)

  def isInvalid(id: Long): Boolean =
    val s = id.toString
    1.to(s.length / 2)
      .exists: l =>
        s.grouped(l).distinct.size == 1
