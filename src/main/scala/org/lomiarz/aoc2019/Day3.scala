package org.lomiarz.aoc2019

object Day3 {

  def main(args: Array[String]): Unit = {
    val input = readLines("day3").map(_.split(","))
    val center = (0, 0)

    def generatePoints(direction: String, distance: Int, startingPoint: (Int, Int)): Seq[(Int, Int)] = direction match {
      case "R" =>
        for {
          x <- 1 to distance
        } yield (startingPoint._1 + x, startingPoint._2)
      case "L" =>
        for {
          x <- 1 to distance
        } yield (startingPoint._1 - x, startingPoint._2)
      case "U" =>
        for {
          y <- 1 to distance
        } yield (startingPoint._1, startingPoint._2 + y)
      case "D" =>
        for {
          y <- 1 to distance
        } yield (startingPoint._1, startingPoint._2 - y)
    }

    def wirePoints(wirePath: Array[String]): Seq[(Int, Int)] = {
      val regexp = """([RLDU])(\d+)""".r
      wirePath.foldLeft(Seq((0, 0))) {
        case (acc, shift) =>
          shift match {
            case regexp(direction, distance) => acc ++ generatePoints(direction, distance.toInt, acc.last)
          }
      }
    }

    val wire1 = wirePoints(input.head)
    val wire2 = wirePoints(input(1))
    val intersections = wire1.intersect(wire2).filterNot(_ == center)

    println(s"Part 1 result = ${intersections.map { case (x, y) => x.abs + y.abs }.min}")
    println(s"Part 2 result = ${intersections.map(point => wire1.indexOf(point) + wire2.indexOf(point)).min}")

    ()
  }
}
