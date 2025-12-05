package day4

import scala.io.Source

object Day4A:

  final case class Grid(value: Vector[Vector[GridItem]]):
    override def toString: String = value.map(_.mkString).mkString("\n")

    def accessible: Int =
      value.zipWithIndex
        .flatMap: (row, rowI) =>
          row.zipWithIndex.map: (column, columnI) =>
            column == GridItem.Roll && paperNeighbors(rowI, columnI) < 4
        .count(_ == true)

    def paperNeighbors(row: Int, column: Int): Int =
      neighborsOf(row, column).count(_ == GridItem.Roll)

    def neighborsOf(row: Int, column: Int): Vector[GridItem] =
      val n = neighbors(row, column)
      Vector(
        n(-1, -1),
        n(-1, 0),
        n(-1, 1),
        n(0, -1),
        n(0, 1),
        n(1, -1),
        n(1, 0),
        n(1, 1)
      ).flatten

    def neighbors(row: Int, column: Int)(
        dRow: Int,
        dColumn: Int
    ): Option[GridItem] =
      value.lift(row + dRow).flatMap(_.lift(column + dColumn))

  enum GridItem:
    case Roll, Empty

    override def toString: String = this match
      case GridItem.Roll  => "@"
      case GridItem.Empty => "."

  def main(args: Array[String]): Unit =
    val data: Grid = Grid(
      Source
        .fromResource("day4/input.txt")
        .getLines()
        .toVector
        .map:
          _.map:
            case '@' => GridItem.Roll
            case _   => GridItem.Empty
          .toVector
    )

    println(data)

    println(data.accessible)
