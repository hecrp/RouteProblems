package improvers

import problems.LocationProblem
import solutions.LocationSolution
import scala.collection.immutable.BitSet

/**
  * Created by hector on 1/14/17.
  */
class LocationLocalSearch(problem: LocationProblem, maxIterationsWithoutImprovement: Int) extends LocationImprover {
  override def improve(initialSolution: LocationSolution): LocationSolution = {
    // Función recursiva para mejorar la solución
    def improveRecursive(bestSolution: LocationSolution, notChosenLocations: BitSet, improve: Int): LocationSolution = {
      // Si se han realizado maxIterationsWithoutImprovement iteraciones sin mejora, se devuelve la mejor solución encontrada
      if (improve >= maxIterationsWithoutImprovement) bestSolution
      else {
        // Se intenta mejorar la solución actual
        val (newBestSolution, newNotChosenLocations, newImprove) = 
          initialSolution.chosenLocations.foldLeft((bestSolution, notChosenLocations, improve)) {
            case ((currentBest, currentNotChosen, currentImprove), locationToRemove) =>
              // Para cada ubicación elegida, se prueba a sustituirla por una no elegida
              currentNotChosen.foldLeft((currentBest, currentNotChosen, currentImprove)) {
                case ((innerBest, innerNotChosen, innerImprove), notChosen) =>
                  // Se construye una solución candidata
                  val candidate = buildCandidateFrom(initialSolution)
                    .removeElement(locationToRemove)
                    .addElement(notChosen, 0)
                    .updateCosts(problem)

                  // Si la solución candidata es mejor, se actualiza la mejor solución
                  if (candidate.value < innerBest.value)
                    (candidate, innerNotChosen + locationToRemove, 0)
                  else
                    (innerBest, innerNotChosen, innerImprove + 1)
              }
          }
        // Se llama recursivamente con la nueva mejor solución
        improveRecursive(newBestSolution, newNotChosenLocations, newImprove)
      }
    }

    // Se inicia el proceso de mejora
    improveRecursive(initialSolution, getNotChosenLocations(initialSolution), 0)
  }

  // Obtiene las ubicaciones no elegidas en la solución actual
  def getNotChosenLocations(solution: LocationSolution): BitSet =
    BitSet.fromSpecific((0 until problem.locations).filterNot(solution.isInSolution))

  // Construye una nueva solución candidata a partir de una solución dada
  def buildCandidateFrom(solution: LocationSolution): LocationSolution =
    solution.chosenLocations.foldLeft(LocationSolution(Set.empty, Vector.fill(problem.clients)(Int.MaxValue), 0)) {
      (candidate, node) => candidate.addElement(node, 0)
    }.updateCosts(problem)
}
