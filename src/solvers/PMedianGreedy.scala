package solvers

import problems.LocationProblem
import solutions.{LocationSolution, Solution}

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

/**
  * Created by hector on 1/18/17.
  */
class PMedianGreedy(problem: LocationProblem) extends MultiRunnable{

  def solve(): LocationSolution = {

    val solution: LocationSolution = new LocationSolution(problem.p)
    val notChosenLocations: HashSet[Int] = new HashSet()
    val temporalValues: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()

    initNotChosenLocations(notChosenLocations)
    initCosts(solution)


    while(! solution.isComplete){

      for(i <- notChosenLocations) {
        temporalValues.append(evaluateLocation(solution, i))
      }

      val minimumCostCandidate = temporalValues.minBy(_._2)
      solution.addElement(minimumCostCandidate._1, minimumCostCandidate._2)
      updateCosts(solution)
      temporalValues.clear()
    }

    solution

  }

  private def initNotChosenLocations(locations: HashSet[Int]): Unit = {
    for(i <- 0 until problem.locations)
      locations.add(i)
  }

  private def initCosts(solution: LocationSolution): Unit = {
    for(i <- 0 until problem.clients)
      solution.costs.append(Int.MaxValue)
  }

  private def evaluateLocation(solution: LocationSolution, candidate: Int): (Int, Int) = {
    var value: Int = 0

    for(i <- 0 until problem.clients){
      if(problem.distanceMatrix(candidate)(i) < solution.costs(i))
        value =  value + problem.distanceMatrix(candidate)(i)
      else
        value =  value + solution.costs(i)
    }

    (candidate, value)
  }

  private def updateCosts(solution: LocationSolution): Unit = {
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

  override def multiRun(arguments: List[Any]): List[Solution] = {
    val solutions = ListBuffer[Solution]()

    for(argument <- arguments){
      //Cast Any to Int using pattern matching
      val intArg = argument match {case n:Number => n.intValue()
      case x => throw new IllegalArgumentException(s"$x is not a number.")}

      solutions.append(solve())
    }
    solutions.toList
  }
}
