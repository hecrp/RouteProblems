package improvers
import problems.LocationProblem
import solutions.LocationSolution
import scala.collection.mutable.ArrayBuffer

/**
  * Created by hector on 1/19/17.
  */

class LocationSimmulatedAnnealing(problem: LocationProblem) extends LocationImprover(problem) {
  var temperature: Double = 10.0
  val iterationsPerTemperature: Int = 3000

  override def improve(initialSolution: LocationSolution) = {
    var notChosenLocations = getNotChosenLocations(initialSolution)
    var bestSolution: LocationSolution = initialSolution
    var temperatureChangeCnt: Int = 0
    var repetitionCnt: Int = 0
    var improve = 0

    while(improve < 10000) {
      repetitionCnt = 0
      temperatureChangeCnt = 0
      while(repetitionCnt < iterationsPerTemperature) {
        val oldFunctionValue: Int = bestSolution.value
        val randomSolution: LocationSolution = randomSubstitution(bestSolution, notChosenLocations)
        val newFunctionValue: Int = randomSolution.value

        if((newFunctionValue - oldFunctionValue).<=(0)) {
          bestSolution = randomSolution
          notChosenLocations = getNotChosenLocations(bestSolution)
          improve = 0
        }
        else {
          if(scala.util.Random.nextDouble() < Math.exp(-(newFunctionValue - oldFunctionValue) / temperature)) {
            bestSolution = randomSolution
            notChosenLocations = getNotChosenLocations(bestSolution)
            improve = 0
          }
          else
            improve += 1
        }
        repetitionCnt += 1
      }
      temperatureChangeCnt += 1
      temperature = 1.25 - (1.25 * temperatureChangeCnt / iterationsPerTemperature.toDouble)
    }

    bestSolution
  }

  private def getNotChosenLocations(solution: LocationSolution): ArrayBuffer[Int] = {
    val notChosenLocations = new ArrayBuffer[Int]

    for (i <- 0 until problem.locations)
      if(!solution.isInSolution(i))
        notChosenLocations.append(i)

    notChosenLocations
  }

  private def randomSubstitution(solution: LocationSolution, notChosenLocations: ArrayBuffer[Int]): LocationSolution = {
    val candidate = new LocationSolution(solution.chosenLocations.size)

    for(node <- solution.chosenLocations)
      candidate.addElement(node, 0)

    val elementToRemove = solution.chosenLocations.toIndexedSeq(scala.util.Random.nextInt(candidate.chosenLocations.size))
    val elementToAdd = notChosenLocations(scala.util.Random.nextInt(notChosenLocations.size))

    candidate.removeElement(elementToRemove)
    candidate.addElement(elementToAdd, 0)

    candidate.updateCosts(problem)
    candidate
  }
}
