package org.lomiarz.aoc2019

object Day1Part1 {

  def main(args: Array[String]): Unit = {
    def fuelConsumption(mass: Long): Long = mass / 3 - 2

    val result = readLines("day1")
      .map(_.toLong)
      .map(fuelConsumption)
      .sum

    println(result)

    ()
  }
}
