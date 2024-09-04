package solvers

import scala.util.Random
import solutions.{RoutingSolution, Solution}
import problems.RoutingProblem
import improvers.RoutingTwoOPT

/**
 * This is a simplified version of the Kernighan-Lin algorithm for the Traveling Salesman Problem (TSP).
 * The Kernighan-Lin algorithm is a heuristic method for solving the TSP, which aims to find a good approximation
 * of the optimal tour in a reasonable amount of time. This implementation combines the basic idea of Kernighan-Lin
 * with a 2-opt local search improvement step.
 *
 * The algorithm works as follows:
 * 1. Generate an initial random tour
 * 2. Repeatedly apply 2-opt moves to improve the tour until no further improvement is possible
 * 3. Optionally, perform multiple runs and apply an additional 2-opt improvement step
 *
 * While this implementation is not the full Kernighan-Lin algorithm, it captures the spirit of iterative improvement
 * and can produce good results for many TSP instances.
 */

 // Clase que implementa el algoritmo de Kernighan-Lin para el Problema del Viajante de Comercio (TSP)
class TSPKernighanLinSolver(problem: RoutingProblem) extends MultiRunnable {
  private val random = new Random()

  def solve(): RoutingSolution = {
    // Generar un tour inicial aleatorio
    var tour = randomTour()
    var improved = true

    // Bucle principal: continuar mientras se puedan hacer mejoras
    while (improved) {
      improved = false
      // Iterar sobre todos los pares de ciudades
      for (i <- tour.indices) {
        for (j <- i + 1 until tour.length) {
          // Intentar un intercambio 2-opt
          val newTour = twoOpt(tour, i, j)
          // Si el nuevo tour es mejor, actualizarlo
          if (tourLength(newTour) < tourLength(tour)) {
            tour = newTour
            improved = true
          }
        }
      }
    }

    // Crear y devolver una solución de enrutamiento
    new RoutingSolution(tour.map(i => (i, 0)).toVector)
  }

  // Genera un tour aleatorio
  private def randomTour(): Array[Int] = {
    val tour = (0 until problem.graphSize).toArray
    // Mezclar el array usando el algoritmo de Fisher-Yates
    for (i <- tour.length - 1 to 1 by -1) {
      val j = random.nextInt(i + 1)
      val temp = tour(i)
      tour(i) = tour(j)
      tour(j) = temp
    }
    tour
  }

  // Realiza un intercambio 2-opt en el tour
  private def twoOpt(tour: Array[Int], i: Int, j: Int): Array[Int] = {
    val newTour = tour.clone()
    var x = i
    var y = j
    // Invertir el segmento entre i y j
    while (x < y) {
      val temp = newTour(x)
      newTour(x) = newTour(y)
      newTour(y) = temp
      x += 1
      y -= 1
    }
    newTour
  }

  // Calcula la longitud total del tour
  private def tourLength(tour: Array[Int]): Double = {
    var length = 0.0
    for (i <- tour.indices) {
      val from = tour(i)
      val to = tour((i + 1) % tour.length)
      length += problem.distanceMatrix(from)(to)
    }
    length
  }

  // Implementación de múltiples ejecuciones (para la interfaz por CLI solamente)
  override def multiRun(): List[Solution] = {
    println("Ingrese el número de ejecuciones:")
    val numRuns = scala.io.StdIn.readInt()

    println("Ingrese el número máximo de iteraciones sin mejora para 2OPT:")
    val maxIterationsWithoutImprovement = scala.io.StdIn.readInt()

    // Crear un objeto RoutingTwoOPT para mejorar las soluciones
    val routingTwoOPT = new RoutingTwoOPT(problem, maxIterationsWithoutImprovement)

    // Realizar múltiples ejecuciones
    (1 to numRuns).map { _ =>
      // Obtener una solución inicial
      val initialSolution = solve()
      // Mejorar la solución con 2-OPT
      val improvedSolution = routingTwoOPT.improve(initialSolution)
      
      // Calcular métricas adicionales para la solución mejorada
      val averageCost = improvedSolution.getAverageCost
      val maxCost = improvedSolution.getMaxCost
      val minCost = improvedSolution.getMinCost
      val maxDistance = improvedSolution.getMaxDistance
      val numberOfClients = improvedSolution.getNumberOfClients
      val numberOfLocations = improvedSolution.getNumberOfLocations

      // Crear una nueva instancia de RoutingSolution con las métricas incluidas
      new RoutingSolution(improvedSolution.getTour.map(node => (node, 0))) {
        override def getAverageCost: Double = averageCost
        override def getMaxCost: Int = maxCost
        override def getMinCost: Int = minCost
        override def getMaxDistance: Int = maxDistance
        override def getNumberOfClients: Int = numberOfClients
        override def getNumberOfLocations: Int = numberOfLocations
      }
    }.toList
  }
}
