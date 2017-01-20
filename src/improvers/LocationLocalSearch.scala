package improvers

import problems.LocationProblem
import solutions.LocationSolution
import scala.collection.mutable

/**
  * Created by hector on 1/14/17.
  */
class LocationLocalSearch(problem: LocationProblem) extends LocationImprover(problem) {
  override def improve(initialSolution: LocationSolution): LocationSolution = {
    var bestSolution: LocationSolution = initialSolution
    var candidate: LocationSolution = new LocationSolution(initialSolution.chosenLocations.size)
    var improve = 0

    val notChosenLocations = getNotChosenLocations(initialSolution)

    while(improve < 5000) {
      for(locationToRemove <- initialSolution.chosenLocations) {
        for(notChosen <- notChosenLocations){
          candidate = buildCandidateFrom(initialSolution)

          candidate.removeElement(locationToRemove)
          candidate.addElement(notChosen, 0)
          candidate.updateCosts(problem)

          if(candidate.value < bestSolution.value) {
            bestSolution = candidate
            notChosenLocations.add(locationToRemove)
            improve = 0
          }
          else
            improve += 1
        }
      }
    }
    bestSolution
  }

  def getNotChosenLocations(solution: LocationSolution): mutable.BitSet = {
    val notChosenLocations = mutable.BitSet.empty

    for (i <- 0 until problem.locations)
      if(!solution.isInSolution(i))
        notChosenLocations.add(i)

    notChosenLocations
  }


  def buildCandidateFrom(solution: LocationSolution): LocationSolution = {
    val candidate = new LocationSolution(solution.chosenLocations.size)

    for(node <- solution.chosenLocations)
      candidate.addElement(node, 0)
    candidate.updateCosts(problem)

    candidate
  }

}
