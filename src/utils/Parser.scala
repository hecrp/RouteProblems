package utils

import scala.io.Source

object Parser {

  def parse(filename: String): (Array[Array[Int]], Int) = {
    val lines = Source.fromFile(filename).getLines.toList
    val size: Int = lines.size

    val distanceMatrix = Array.ofDim[Int](size, size)

    for (i <- 0 until size)
      for (j <- 0 until size)
        distanceMatrix(i)(j) = 0

    var i: Int = 0
    var j: Int = 0

    for (line <- lines){
      val distances: List[Int] = line.trim.split("\\s+").toList.map(_.toInt)

      for(distance <- distances){
        distanceMatrix(i)(j) = distance
        j += 1
      }

      j = 0
      i += 1
    }

    (distanceMatrix, size)

  }

}