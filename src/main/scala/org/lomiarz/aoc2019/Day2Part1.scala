package org.lomiarz.aoc2019

object Day2Part1 {

  def main(args: Array[String]): Unit = {
    val input = readLines("day2").head.split(",").map(_.toInt)
    val fix = Seq((1, 12), (2, 2))

    val intcode = fix.foldLeft(input) {
      case (acc, (position, value)) => acc.updated(position, value)
    }

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

    println(processIntcode(0, intcode)(0))

    ()
  }
}
