package improvers

import problems.LocationProblem
import solutions.LocationSolution

import scala.collection.mutable.{ArrayBuffer, HashSet}

/**
  * Created by hector on 1/14/17.
  */
class LocationLocalSearch(problem: LocationProblem) extends LocationImprover(problem) {
  override def improve(initialSolution: LocationSolution, notChosenLocations: HashSet[Int]): LocationSolution = {
    var bestSolution: LocationSolution = initialSolution
    var candidate: LocationSolution = new LocationSolution(initialSolution.chosenLocations.size)
    var improve = 0

    while(improve < 5000) {
      for(i <- initialSolution.chosenLocations.indices) {
        for(notChosen <- notChosenLocations){
          candidate = buildCandidateFrom(initialSolution)

          var removedLocation = candidate.chosenLocations(i)
          candidate.removeElement(i)
          candidate.addElement(notChosen, 0)
          updateCosts(candidate)

          if(candidate.value < bestSolution.value) {
            bestSolution = candidate
            notChosenLocations.add(removedLocation)
            improve = 0
          }
          else
            improve += 1
        }
      }
    }
    bestSolution
  }

  def updateCosts(solution: LocationSolution): Unit = {
    val costs:ArrayBuffer[Int] = ArrayBuffer[Int]()

    for(client <- 0 until problem.clients) {
      var minCost = Int.MaxValue
      for(location <- solution.chosenLocations) {
        if(problem.distanceMatrix(location)(client) < minCost)
          minCost = problem.distanceMatrix(location)(client)
      }
      costs.append(minCost)
    }
    solution.costs = costs
    solution.value = costs.sum
  }

  def buildCandidateFrom(solution: LocationSolution): LocationSolution = {
    val candidate = new LocationSolution(solution.chosenLocations.size)

    for(node <- solution.chosenLocations)
      candidate.addElement(node, 0)
    candidate.value = solution.costs.sum
    candidate
  }

}
