package improvers

import problems.LocationProblem
import solutions.LocationSolution

import scala.collection.mutable.HashSet

/**
  * Created by hector on 1/18/17.
  */
trait LocationImprover {
  def improve(initialSolution: LocationSolution): LocationSolution
}
