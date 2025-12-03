package day3

import scala.io.Source

object Day3A:

  type Joltage = Int
  type Index = Int
  type BatteryWithIndex = (joltage: Joltage, index: Index)

  def main(args: Array[String]): Unit =
    val data: Vector[Vector[BatteryWithIndex]] = Source
      .fromResource("day3/input.txt")
      .getLines()
      .toVector
      .map(_.grouped(1).map(_.toInt).zipWithIndex.toVector)

    val joltages = data.map: bank =>
      val firstBattery = bank.reduce: (last, current) =>
        if current.joltage > last.joltage && current.index < bank.length - 1
        then current
        else last
      val secondBattery = bank.drop(firstBattery.index + 1).maxBy(_.joltage)
      firstBattery.joltage * 10 + secondBattery.joltage

    println(joltages.sum)
