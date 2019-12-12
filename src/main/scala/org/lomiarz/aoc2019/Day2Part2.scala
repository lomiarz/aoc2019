package org.lomiarz.aoc2019

object Day2Part2 {

  def main(args: Array[String]): Unit = {
    val input = readLines("day2").head.split(",").map(_.toInt)

    def processIntcode(instructionNumber: Int, intcode: Array[Int]): Array[Int] = {
      val index = instructionNumber * 4
      val opcode = intcode(index)
      val inputIndex1 = intcode(index + 1)
      val inputIndex2 = intcode(index + 2)
      val outputIndex = intcode(index + 3)
      if (opcode == 99)
        intcode
      else if (opcode == 1)
        processIntcode(instructionNumber + 1, intcode.updated(outputIndex, intcode(inputIndex1) + intcode(inputIndex2)))
      else if (opcode == 2)
        processIntcode(instructionNumber + 1, intcode.updated(outputIndex, intcode(inputIndex1) * intcode(inputIndex2)))
      else throw new IllegalArgumentException("Wrong input")
    }

    val inputs = for {
      noun <- 0 to 99
      verb <- 0 to 99
    } yield (noun, verb)

    inputs
      .find {
        case (noun, verb) =>
          processIntcode(0, input.updated(1, noun).updated(2, verb))(0) == 19690720
      }
      .map {
        case (noun, verb) =>
          100 * noun + verb
      }
      .foreach(println)

    ()
  }
}
