package day3

import scala.annotation.tailrec
import scala.io.Source

object Day3B:

  type Joltage = Int
  type Index = Int
  type BatteryWithIndex = (joltage: Joltage, index: Index)

  def main(args: Array[String]): Unit =
    val data: Vector[Vector[BatteryWithIndex]] = Source
      .fromResource("day3/input.txt")
      .getLines()
      .toVector
      .map(_.grouped(1).map(_.toInt).zipWithIndex.toVector)

    val joltages = data.map(findBatteries(_, Vector.empty, 0, 12))
    println(joltages.map(_.mkString.toLong).sum)

  @tailrec
  def findBatteries(
      bank: Vector[BatteryWithIndex],
      acc: Vector[Joltage],
      lastIndex: Index,
      remaining: Int
  ): Vector[Joltage] =
    if remaining == 0 then acc
    else
      val slice = bank.slice(lastIndex, bank.length - remaining + 1)
      val highest = slice.maxBy(_.joltage)
//      println(s"${slice.map(_.joltage).mkString}: ${highest.joltage}")
      findBatteries(
        bank = bank,
        acc = acc.appended(highest.joltage),
        lastIndex = highest.index + 1,
        remaining = remaining - 1
      )
