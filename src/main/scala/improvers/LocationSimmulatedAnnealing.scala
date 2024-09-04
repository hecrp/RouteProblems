package improvers
import problems.LocationProblem
import solutions.LocationSolution
import scala.util.Random

/**
  * Created by hector on 1/19/17.
  */

// Implementación del algoritmo de recocido simulado para el problema de localización
class LocationSimulatedAnnealing(
  problem: LocationProblem,
  maxIterationsWithoutImprovement: Int,
  initialTemperature: Double,
  iterationsPerTemperature: Int,
  coolingRate: Double
) extends LocationImprover {
  private var temperature: Double = initialTemperature

  override def improve(initialSolution: LocationSolution): LocationSolution = {
    // Función recursiva para mejorar la solución
    def improveRecursive(bestSolution: LocationSolution, notChosenLocations: Set[Int], improve: Int): LocationSolution = {
      // Si se han realizado maxIterationsWithoutImprovement iteraciones sin mejora, se devuelve la mejor solución encontrada
      if (improve >= maxIterationsWithoutImprovement) bestSolution
      else {
        // Se realizan iteraciones a la temperatura actual
        val (newBestSolution, newNotChosenLocations) = 
          (0 until iterationsPerTemperature).foldLeft((bestSolution, notChosenLocations)) {
            case ((currentBest, currentNotChosen), _) =>
              // Se genera una solución aleatoria
              val randomSolution = randomSubstitution(currentBest, currentNotChosen.toSeq)
              // Se decide si se acepta la nueva solución
              if (acceptSolution(currentBest.value, randomSolution.value)) {
                (randomSolution, getNotChosenLocations(randomSolution))
              } else {
                (currentBest, currentNotChosen)
              }
          }
        
        // Se reduce la temperatura
        temperature *= coolingRate
        // Se llama recursivamente con la nueva mejor solución
        improveRecursive(newBestSolution, newNotChosenLocations, if (newBestSolution == bestSolution) improve + 1 else 0)
      }
    }

    improveRecursive(initialSolution, getNotChosenLocations(initialSolution), 0)
  }

  // Obtiene las ubicaciones no elegidas en la solución actual
  private def getNotChosenLocations(solution: LocationSolution): Set[Int] =
    (0 until problem.locations).filterNot(solution.isInSolution).toSet

  // Genera una nueva solución sustituyendo aleatoriamente una ubicación elegida por una no elegida
  private def randomSubstitution(solution: LocationSolution, notChosenLocations: Seq[Int]): LocationSolution = {
    val elementToRemove = solution.chosenLocations.toSeq(Random.nextInt(solution.chosenLocations.size))
    val elementToAdd = notChosenLocations(Random.nextInt(notChosenLocations.length))

    solution.chosenLocations
      .foldLeft(LocationSolution(Set.empty, Vector.fill(problem.clients)(Int.MaxValue), 0)) { (candidate, node) =>
        if (node != elementToRemove) candidate.addElement(node, 0) else candidate
      }
      .addElement(elementToAdd, 0)
      .updateCosts(problem)
  }

  // Decide si se acepta una nueva solución basándose en la diferencia de valor y la temperatura actual
  private def acceptSolution(oldValue: Int, newValue: Int): Boolean =
    newValue <= oldValue || Random.nextDouble() < Math.exp(-(newValue - oldValue) / temperature)
}
