package solutions

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hector on 1/18/17.
  */

class LocationSolution(p: Int) extends Solution{
  var chosenLocations:ArrayBuffer[Int] = ArrayBuffer[Int]()
  var costs:ArrayBuffer[Int] = ArrayBuffer[Int]()
  var value:Int = 0

  def addElement(location: Int, newValue: Int): Unit = {
    chosenLocations.append(location)
    value = newValue
  }

  def removeElement(locationIndex: Int): Unit = {
    chosenLocations.remove(locationIndex)
  }

  def isComplete: Boolean = chosenLocations.length == p

  def isEmpty: Boolean = chosenLocations.isEmpty

  override def toString: String = {
    var result: String = ""
    result += "P-Median problem with:\n"
    result += " p value: " + p + "\n solution value: " + value + "\n Facilities in: "
    result += chosenLocations + "\n"
    result += " Cost from nearest facility to location: " + costs + "\n"

    result

  }

}
