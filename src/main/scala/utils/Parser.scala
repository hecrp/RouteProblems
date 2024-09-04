package utils

import scala.io.Source

object Parser {

  def parse(filename: String): (Array[Array[Int]], Int) = {
    val lines = Source.fromFile(filename).getLines.toList
    val size = lines.size

    val distanceMatrix = lines.zipWithIndex.foldLeft(Array.ofDim[Int](size, size)) { case (matrix, (line, i)) =>
      line.trim.split("\\s+").map(_.toInt).zipWithIndex.foreach { case (distance, j) =>
        matrix(i)(j) = distance
      }
      matrix
    }

    (distanceMatrix, size)
  }

}