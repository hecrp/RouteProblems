package improvers

import problems.LocationProblem
import solutions.LocationSolution

import scala.collection.mutable.HashSet

/**
  * Created by hector on 1/18/17.
  */
abstract class LocationImprover(problem: LocationProblem) {
  def improve(initialSolution: LocationSolution, notChosenLocations: HashSet[Int]): LocationSolution

}
