package utils

import scala.io.Source
import problems.LocationProblem

/**
  * Created by hector on 1/18/17.
  */
object PMedianParser {

  def parse(filename: String): (Array[Array[Double]], Int, Int, Int) = {
    val lines = Source.fromFile(filename).getLines().toList
    
    // Encontrar la primera línea no vacía que contiene datos
    val dataLines = lines.dropWhile(_.trim.isEmpty)
    
    // Extraer el número de ubicaciones (n) de la primera línea de datos
    val n = dataLines.head.trim.toInt
    
    // Asumimos que p es 100 basado en el nombre del archivo
    val p = 100
    
    // Parsear la matriz de distancias
    val distanceMatrix = parseDistanceMatrix(dataLines.tail, n)
    
    (distanceMatrix, n, n, p)
  }

  private def parseDistanceMatrix(lines: List[String], n: Int): Array[Array[Double]] = {
    val matrix = Array.ofDim[Double](n, n)
    var row = 0
    var col = 0
    
    lines.foreach { line =>
      val cleanedLine = line.trim.stripPrefix("{").stripSuffix("},").stripSuffix("}")
      cleanedLine.split(",").foreach { value =>
        if (row < n && col < n) {
          matrix(row)(col) = value.trim.toDouble
          col += 1
          if (col == n) {
            row += 1
            col = 0
          }
        }
      }
    }
    
    matrix
  }

}
