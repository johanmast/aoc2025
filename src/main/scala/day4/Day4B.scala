package day4

import scala.annotation.tailrec
import scala.io.Source

object Day4B:

  final case class Grid(value: Vector[Vector[GridItem]]):
    override def toString: String = value.map(_.mkString).mkString("\n")

    def remove: Grid =
      Grid(
        value.zipWithIndex
          .map: (row, rowI) =>
            row.zipWithIndex.map: (column, columnI) =>
              if column == GridItem.Roll && paperNeighbors(rowI, columnI) < 4
              then GridItem.Empty
              else column
      )

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

    @tailrec
    def keepRemoving(grid: Grid): Grid =
      val newGrid = grid.remove
      if newGrid == grid then newGrid else keepRemoving(newGrid)

    val finalGrid = keepRemoving(data)
    val rollsBefore = data.value.flatMap(_.filter(_ == GridItem.Roll)).size
    val rollsAfter = finalGrid.value.flatMap(_.filter(_ == GridItem.Roll)).size
    println(rollsBefore - rollsAfter)
