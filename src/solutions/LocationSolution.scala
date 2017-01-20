package solutions

import problems.LocationProblem

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, BitSet}

/**
  * Created by hector on 1/18/17.
  */

class LocationSolution(p: Int) extends Solution with Cloneable{
  //var chosenLocations:ArrayBuffer[Int] = ArrayBuffer[Int]()
  var costs:ArrayBuffer[Int] = ArrayBuffer[Int]()
  var value:Int = 0

  val chosenLocations: BitSet = BitSet.empty

  def addElement(location: Int, newValue: Int): Unit = {
    chosenLocations.add(location)
    value = newValue
  }

  def removeElement(location: Int): Unit = {
    chosenLocations.remove(location)
  }

  def isInSolution(location: Int): Boolean =  chosenLocations.contains(location)

  def getNotChosenLocations(): mutable.BitSet = BitSet.empty.^(chosenLocations)

  def isComplete: Boolean = chosenLocations.size == p

  def isEmpty: Boolean = chosenLocations.isEmpty

  def updateCosts(problem: LocationProblem): Unit = {
    val newCosts = new ArrayBuffer[Int]()

    for(client <- 0 until problem.clients) {
      var minCost = Int.MaxValue
      for(location <- chosenLocations) {
        if(problem.distanceMatrix(location)(client) < minCost)
          minCost = problem.distanceMatrix(location)(client)
      }
      newCosts.append(minCost)
    }
    costs = newCosts
    value = costs.sum
  }

  override def toString: String = {
    var result: String = ""
    result += "P-Median problem with:\n"
    result += " p value: " + p + "\n solution value: " + value + "\n Facilities in: "
    result += chosenLocations + "\n"
    result += " Cost from nearest facility to location: " + costs + "\n"

    result

  }

}
