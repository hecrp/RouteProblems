package solvers

import improvers.{LocationLocalSearch, LocationSimmulatedAnnealing}
import problems.LocationProblem
import solutions.{LocationSolution, Solution}

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

/**
  * Created by hector on 1/18/17.
  */
class PMedianGreedy(problem: LocationProblem) {
  val improver: LocationSimmulatedAnnealing = new LocationSimmulatedAnnealing(problem)
  //val improver: LocationLocalSearch = new LocationLocalSearch(problem)


  def solve(): LocationSolution = {

    val solution: LocationSolution = new LocationSolution(problem.p)
    val temporalValues: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()

    initCosts(solution)


    while(! solution.isComplete){

      for(i <- 0 until (problem.locations - solution.chosenLocations.size)) {
        temporalValues.append(evaluateLocation(solution, i))
      }

      val minimumCostCandidate = temporalValues.minBy(_._2)
      solution.addElement(minimumCostCandidate._1, minimumCostCandidate._2)
      updateCosts(solution)
      temporalValues.clear()
    }

    improver.improve(solution)
    //solution

  }

  protected def initCosts(solution: LocationSolution): Unit = {
    for(i <- 0 until problem.clients)
      solution.costs.append(Int.MaxValue)
  }

  protected def evaluateLocation(solution: LocationSolution, candidate: Int): (Int, Int) = {
    var value: Int = 0

    for(i <- 0 until problem.clients){
      if(problem.distanceMatrix(candidate)(i) < solution.costs(i))
        value =  value + problem.distanceMatrix(candidate)(i)
      else
        value =  value + solution.costs(i)
    }

    (candidate, value)
  }

  protected def updateCosts(solution: LocationSolution): Unit = {
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
  }

}
