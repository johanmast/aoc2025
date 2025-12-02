package day1

import scala.io.Source

object Day1A:
  enum Direction:
    case Left, Right

  def main(args: Array[String]): Unit =
    val data = Source
      .fromResource("day1/input.txt")
      .getLines()
      .toVector
      .map: line =>
        val direction = line.head match
          case 'L' => Direction.Left
          case 'R' => Direction.Right
        direction -> line.tail.toInt

    val password = data.foldLeft((0, 50)):
      case ((password, pos), (direction, ticks)) =>
        val posAfterTicks = direction match
          case Direction.Left  => pos - ticks % 100
          case Direction.Right => pos + ticks % 100
        val newPos =
          if posAfterTicks < 0 then 100 + posAfterTicks
          else if posAfterTicks > 99 then posAfterTicks - 100
          else posAfterTicks

        val newPassword = if newPos == 0 then password + 1 else password
        println(newPos)
        (newPassword, newPos)

    println(password)
