package solvers

import scala.util.Random
import solutions.{RoutingSolution, Solution}
import problems.RoutingProblem
import improvers.RoutingTwoOPT

/**
 * This is an Ant Colony Optimization (ACO) implementation for the  Traveling Salesman Problem (TSP)
 *
 * ACO is a probabilistic technique for solving computational problems which can be reduced to finding good paths through graphs.
 * In the context of TSP, it mimics the behavior of ants in finding paths between their colony and food sources.
 *
 * Key components:
 * 1. Pheromone trails: Represent the desirability of path segments.
 * 2. Heuristic information: Usually the inverse of the distance between cities.
 * 3. Probabilistic decision making: Ants choose their paths based on pheromone levels and heuristic information.
 * 4. Pheromone update: After each iteration, pheromones are updated based on the quality of found solutions.
 *
 * Algorithm outline:
 * 1. Initialize pheromone trails.
 * 2. For each iteration:
 *    a. Construct ant solutions.
 *    b. Apply local search (optional).
 *    c. Update pheromones.
 * 3. Return the best solution found.
 *
 * Parameters:
 * - numAnts: Number of ants (solutions) per iteration.
 * - numIterations: Number of iterations to run the algorithm.
 * - alpha: Importance of pheromone trail.
 * - beta: Importance of heuristic information.
 * - rho: Pheromone evaporation rate.
 * - q: Pheromone deposit factor.
 */

 // Clase que implementa el algoritmo de Optimización por Colonia de Hormigas para el Problema del Viajante de Comercio (TSP)
class TSPAntColonyOptimization(problem: RoutingProblem, numAnts: Int, numIterations: Int, alpha: Double, beta: Double, rho: Double, q: Double) extends MultiRunnable {
  // Matriz de feromonas inicializada con valores de 1.0
  private val pheromoneMatrix: Array[Array[Double]] = Array.fill(problem.graphSize, problem.graphSize)(1.0)
  private val random = new Random()

  // Método principal para resolver el problema TSP
  def solve(): RoutingSolution = {
    var bestSolution: RoutingSolution = null
    var bestCost = Double.MaxValue

    // Iteración principal del algoritmo
    for (_ <- 1 to numIterations) {
      // Construir soluciones para cada hormiga
      val antSolutions = (1 to numAnts).map(_ => constructAntSolution())
      // Actualizar feromonas basadas en las soluciones de las hormigas
      updatePheromones(antSolutions)
      
      // Encontrar la mejor solución de esta iteración
      val iterationBestSolution = antSolutions.minBy(_.getTotalCost)
      if (iterationBestSolution.getTotalCost < bestCost) {
        bestSolution = iterationBestSolution
        bestCost = bestSolution.getTotalCost
      }
    }
    bestSolution
  }

  // Método para construir una solución individual de una hormiga
  private def constructAntSolution(): RoutingSolution = {
    // Seleccionar un nodo inicial aleatorio
    var currentNode = random.nextInt(problem.graphSize)
    val tour = scala.collection.mutable.ArrayBuffer((currentNode, 0))
    var unvisited = (0 until problem.graphSize).filter(_ != currentNode).toSet
    var totalCost = 0

    // Construir el tour visitando todos los nodos
    while (unvisited.nonEmpty) {
      val nextNode = selectNextNode(currentNode, unvisited)
      val distance = problem.distanceMatrix(currentNode)(nextNode)
      totalCost += distance
      tour += ((nextNode, totalCost))
      unvisited = unvisited - nextNode
      currentNode = nextNode
    }
    
    // Añadir el coste de regresar al nodo inicial
    totalCost += problem.distanceMatrix(currentNode)(tour.head._1)

    RoutingSolution(tour.toVector)
  }

  // Método para seleccionar el siguiente nodo basado en feromonas y distancia
  private def selectNextNode(currentNode: Int, unvisited: Set[Int]): Int = {
    // Calcular probabilidades para cada nodo no visitado
    val probabilities = unvisited.map { node =>
      val pheromone = pheromoneMatrix(currentNode)(node)
      val distance = 1.0 / problem.distanceMatrix(currentNode)(node)
      val probability = Math.pow(pheromone, alpha) * Math.pow(distance, beta)
      (node, probability)
    }.toArray

    // Selección basada en ruleta
    val totalProbability = probabilities.map(_._2).sum
    var cumulativeProbability = 0.0
    val threshold = random.nextDouble() * totalProbability

    for ((node, probability) <- probabilities) {
      cumulativeProbability += probability
      if (cumulativeProbability >= threshold) {
        return node
      }
    }

    // En caso de errores de redondeo, devolver el último nodo
    probabilities.last._1
  }

  // Método para actualizar las feromonas después de cada iteración
  private def updatePheromones(solutions: Seq[RoutingSolution]): Unit = {
    // Evaporación de feromonas
    for (i <- pheromoneMatrix.indices; j <- pheromoneMatrix(i).indices) {
      pheromoneMatrix(i)(j) *= (1 - rho)
    }
    // Depósito de nuevas feromonas
    for (solution <- solutions) {
      val deposit = q / solution.getTotalCost
      val route = solution.getTour
      for (i <- route.indices) {
        val from = route(i)
        val to = route((i + 1) % route.length)
        pheromoneMatrix(from)(to) += deposit
        pheromoneMatrix(to)(from) += deposit
      }
    }
  }

  // Método para realizar múltiples ejecuciones del algoritmo (para la interfaz por CLI solamente)
  override def multiRun(): List[Solution] = {
    println("Ingrese el número de ejecuciones:")
    val numRuns = scala.io.StdIn.readInt()

    println("Ingrese el número máximo de iteraciones sin mejora para 2OPT:")
    val maxIterationsWithoutImprovement = scala.io.StdIn.readInt()

    // Crear un objeto de mejora 2-OPT
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
      new RoutingSolution(improvedSolution.tour) {
        override def getAverageCost: Double = averageCost
        override def getMaxCost: Int = maxCost
        override def getMinCost: Int = minCost
        override def getMaxDistance: Int = maxDistance
        override def getNumberOfClients: Int = numberOfClients
        override def getNumberOfLocations: Int = numberOfLocations
        
        override def getRoute: Array[Int] = improvedSolution.getTour.toArray
      }
    }.toList
  }
}
