package org.lomiarz.aoc2019

object Day4 {

  def main(args: Array[String]): Unit = {
    val inputRange = 158126 to 624574
    def getNumberDigits(number: Int) = number.toString.map(_.asDigit)

    val part1 = inputRange.count { number =>
      val digits = getNumberDigits(number)
      val containsOnlyEqualOrIncreasingDigits = digits.zip(digits.tail).forall { case (a, b) => b >= a }
      val atLeastTwoAdjacentDigitsAreSame = digits.zip(digits.tail).exists { case (a, b) => a == b }

      containsOnlyEqualOrIncreasingDigits && atLeastTwoAdjacentDigitsAreSame
    }

    val part2 = inputRange.count { number =>
      val digits = getNumberDigits(number)
      val containsOnlyEqualOrIncreasingDigits = digits.zip(digits.tail).forall { case (a, b) => b >= a }
      val twoAdjacent = digits.zip(digits.tail).map { case (a, b) => Set(a, b) }.filter(_.size == 1).flatten.toSet
      val threeAdjacent = digits.zip(digits.tail).zip(digits.tail.tail).map { case ((a, b), c) => Set(a, b, c) }.filter(_.size == 1).flatten.toSet

      containsOnlyEqualOrIncreasingDigits && twoAdjacent.nonEmpty && twoAdjacent != threeAdjacent
    }

    println(s"Part 1 result = $part1")

    println(s"Part 2 result = $part2")

    ()
  }
}
