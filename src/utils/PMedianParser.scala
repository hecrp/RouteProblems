package utils

import scala.io.Source

/**
  * Created by hector on 1/18/17.
  */
object PMedianParser {

  def parse(filename: String): (Array[Array[Int]], Int) = {
    val lines = Source.fromFile(filename).getLines.toList
    val size: Int = lines(0).toInt

    val distanceMatrix = Array.ofDim[Int](size, size)

    for (i <- 0 until size)
      for (j <- 0 until size)
        distanceMatrix(i)(j) = 0

    var i: Int = 0
    var j: Int = 0

    for (k <- 1 until lines.size){
      val distances: List[String] = lines(k).replace("{", "").replace(".0,", " ").
        replace("},"," END").replace(".0", "").replace("}}", "").trim.split("\\s+").toList

      for(distance <- distances){
        if(!distance.equals("")){
          if(distance.equals("END")){
            i += 1
            j = 0
          }
          else{
            distanceMatrix(i)(j) = distance.toInt
            j += 1
          }
        }
      }
    }

    (distanceMatrix, size)

  }

}
