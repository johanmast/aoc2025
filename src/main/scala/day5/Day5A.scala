package day5

import scala.io.Source

object Day5A:

  def main(args: Array[String]): Unit =
    val (freshRanges, available) = Source
      .fromResource("day5/input.txt")
      .mkString
      .split("\n\n")
      .toVector match
      case freshRanges +: available +: Vector() =>
        (
          freshRanges
            .split("\n")
            .toVector
            .map:
              _.split("-").toVector match
                case start +: end +: Vector() =>
                  Range.Long.inclusive(start.toLong, end.toLong, 1)
                case _ => throw Error("fail")
          ,
          available
            .split("\n")
            .toVector
            .map(_.toLong)
        )
      case _ => throw Error("fail")

    println(freshRanges)
    println(available)

    println(
      available.count: ingr =>
        freshRanges.exists(range => ingr >= range.start && ingr <= range.end)
    )
