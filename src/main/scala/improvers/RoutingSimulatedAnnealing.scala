package improvers

import problems.RoutingProblem
import solutions.RoutingSolution
import scala.util.Random

class RoutingSimulatedAnnealing(problem: RoutingProblem, initialTemperature: Double = 1000, coolingRate: Double = 0.995, iterations: Int = 10000) extends RoutingImprover {
  private val random = new Random()

  override def improve(initialSolution: RoutingSolution): RoutingSolution = {
    // Inicialización de variables
    var currentSolution = initialSolution
    var bestSolution = initialSolution
    var temperature = initialTemperature

    // Bucle principal del algoritmo de recocido simulado
    for (_ <- 1 to iterations) {
      // Generar una solución vecina
      val neighbor = generateNeighbor(currentSolution)
      
      // Calcular las energías (costos) de las soluciones actual y vecina
      val currentEnergy = currentSolution.getTotalCost
      val neighborEnergy = neighbor.getTotalCost

      // Decidir si se acepta la solución vecina
      if (shouldAccept(currentEnergy, neighborEnergy, temperature)) {
        currentSolution = neighbor
        // Actualizar la mejor solución si la nueva es mejor
        if (neighborEnergy < bestSolution.getTotalCost) {
          bestSolution = neighbor
        }
      }

      // Reducir la temperatura según la tasa de enfriamiento
      temperature *= coolingRate
    }

    // Devolver la mejor solución encontrada
    bestSolution
  }

  // Método para generar una solución vecina mediante un intercambio 2-opt aleatorio
  private def generateNeighbor(solution: RoutingSolution): RoutingSolution = {
    val size = solution.getSize
    // Seleccionar dos índices aleatorios diferentes
    val i = random.nextInt(size - 1)
    var j = random.nextInt(size - 1)
    while (j == i) {
      j = random.nextInt(size - 1)
    }
    // Realizar el intercambio 2-opt
    twoOptSwap(solution, math.min(i, j), math.max(i, j))
  }

  // Método para decidir si se acepta una nueva solución
  private def shouldAccept(currentEnergy: Double, newEnergy: Double, temperature: Double): Boolean = {
    if (newEnergy < currentEnergy) {
      // Siempre aceptar si la nueva solución es mejor
      true
    } else {
      // Calcular la probabilidad de aceptación para soluciones peores
      val acceptanceProbability = Math.exp((currentEnergy - newEnergy) / temperature)
      // Aceptar con una probabilidad basada en la diferencia de energía y la temperatura
      random.nextDouble() < acceptanceProbability
    }
  }

  // Método para realizar un intercambio 2-opt en la solución
  private def twoOptSwap(solution: RoutingSolution, i: Int, k: Int): RoutingSolution = {
    val tour = solution.tour.toVector
    // Invertir el segmento del tour entre i y k
    val newTour = tour.take(i) ++ tour.slice(i, k + 1).reverse ++ tour.drop(k + 1)
    
    // Crear una nueva solución con el tour modificado
    val newSolution = new RoutingSolution(Vector.empty)
    newTour.foldLeft(newSolution)((acc, node) => acc.addElement(Vector((node._1, 0)), problem))
  }
}
