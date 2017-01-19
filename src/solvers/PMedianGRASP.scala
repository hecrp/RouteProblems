package solvers

import improvers.LocationLocalSearch
import problems.LocationProblem
import solutions.{LocationSolution, Solution}

import scala.collection.mutable.{ArrayBuffer, HashSet}
import scala.collection.mutable.ListBuffer

/**
  * Created by hector on 1/18/17.
  */
class PMedianGRASP(problem: LocationProblem) extends PMedianGreedy(problem) with MultiRunnable{
  var GRASPListSize = 5

  override def solve(): LocationSolution = {
    val solution: LocationSolution = new LocationSolution(problem.p)
    val notChosenLocations: HashSet[Int] = new HashSet()
    val temporalValues: ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int, Int)]()

    initNotChosenLocations(notChosenLocations)
    initCosts(solution)


    while(! solution.isComplete){

      for(i <- notChosenLocations) {
        temporalValues.append(evaluateLocation(solution, i))
      }

      temporalValues.sortBy(_._2)
      if(temporalValues.size < GRASPListSize)
        GRASPListSize = temporalValues.size

      val randomIndex = scala.util.Random.nextInt(GRASPListSize)
      var chosenNode = temporalValues(randomIndex)
      notChosenLocations.remove(temporalValues(randomIndex)._1)

      solution.addElement(chosenNode._1, chosenNode._2)
      updateCosts(solution)
      temporalValues.clear()
    }
    improver.improve(solution, notChosenLocations)
  }

  override def multiRun(arguments: List[Any]): List[Solution] = {
    val solutions = ListBuffer[Solution]()

    //Cast Any to Int using pattern matching
    val intArg = arguments(0) match {case n:Number => n.intValue()
    case x => throw new IllegalArgumentException(s"$x is not a number.")}

    for(i <- 0 until intArg)
      solutions.append(solve())

    solutions.toList
  }

}
