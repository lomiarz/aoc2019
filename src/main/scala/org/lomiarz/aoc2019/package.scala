package org.lomiarz

import scala.io.Source

package object aoc2019 {

  def readLines(fileName: String): Seq[String] =
    Source.fromResource(fileName).getLines().toSeq

}
