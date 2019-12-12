package org.lomiarz.aoc2019

object Day1Part2 {

  def main(args: Array[String]): Unit = {
    def fuelConsumption(mass: Long): Long = {
      val fuel = (mass / 3 - 2)
      if (fuel > 0) fuel + fuelConsumption(fuel) else 0
    }

    val result = readLines("day1")
      .map(_.toLong)
      .map(fuelConsumption)
      .sum

    println(result)

    ()
  }
}
