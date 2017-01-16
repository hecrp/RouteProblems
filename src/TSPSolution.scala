import scala.collection.mutable.ArrayBuffer

/**
  * Created by hector on 1/14/17.
  */

class TSPSolution(graphSize: Int) extends Solution{

  val tour:ArrayBuffer[(Int, Int)] = ArrayBuffer[(Int, Int)]()

  def isComplete: Boolean = tour.length == graphSize

  def isEmpty: Boolean = tour.isEmpty

  def addElement(node: Int, distance: Int) {
    tour.append((node, distance))
  }

  def addElementIn(index: Int, node: Int, distance: Int) {
    tour.insert(index, (node, distance))
  }

  def isInSolution(node: Int): Option[(Int, Int)] = {
    tour.find(_._1 == node)
  }

  def getLastNode: Int = {
    tour.last._1
  }

  def getLastDistance: Int = {
    tour.last._2
  }

  def getSize: Int = {
    tour.size
  }

  def clean = tour.clear()

  override def toString: String = {

    var result: String = ""
    tour.foreach(result += _)

    result
  }


}
