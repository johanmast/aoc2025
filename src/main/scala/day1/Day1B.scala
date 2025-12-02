package day1

import scala.annotation.tailrec
import scala.io.Source

object Day1B:
  enum Direction:
    case Left, Right

  type TickOut = (zeroes: Int, pos: Int)

  @tailrec
  def tick(acc: Int, pos: Int, direction: Direction, ticks: Int): TickOut =
    if ticks == 0 then (acc, pos)
    else
      direction match
        case Direction.Left =>
          val posAfterTick = pos - 1
          val newAcc = if posAfterTick == 0 then acc + 1 else acc
          val newPos = if posAfterTick < 0 then 99 else posAfterTick
          tick(newAcc, newPos, direction, ticks - 1)
        case Direction.Right =>
          val posAfterTick = pos + 1
          val newAcc = if posAfterTick == 100 then acc + 1 else acc
          val newPos = if posAfterTick > 99 then 0 else posAfterTick
          tick(newAcc, newPos, direction, ticks - 1)

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
        val (newPassword, newPos) = tick(password, pos, direction, ticks)
        println(newPos)
        (newPassword, newPos)

    println(password)
