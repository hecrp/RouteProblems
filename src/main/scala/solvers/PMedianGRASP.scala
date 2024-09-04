package solvers

import improvers.LocationLocalSearch
import problems.LocationProblem
import solutions.{LocationSolution, Solution}
import improvers.LocationSimulatedAnnealing
import scala.util.Random

/**
  * Created by hector on 1/18/17.
  */
class PMedianGRASP(problem: LocationProblem, graspListSize: Int) extends PMedianGreedy(problem) with MultiRunnable {
  // Método principal para resolver el problema P-Mediana usando GRASP
  override def solve(): LocationSolution = {
    // Inicializa la solución con un conjunto vacío de ubicaciones
    val initialSolution = LocationSolution(Set.empty, Vector.fill(problem.clients)(Int.MaxValue), 0)
    
    // Itera hasta encontrar una solución completa
    val finalSolution = Iterator.iterate(initialSolution)(solutionStep)
      .dropWhile(!_.isComplete(problem.p))
      .next()
      return finalSolution
  }

  // Realiza un paso en la construcción de la solución
  private def solutionStep(solution: LocationSolution): LocationSolution = {
    // Obtiene las ubicaciones no elegidas
    val notChosenLocations = (0 until problem.locations).toSet -- solution.chosenLocations
    
    // Evalúa y ordena las ubicaciones no elegidas
    val evaluatedLocations = notChosenLocations.toVector.map(loc => (loc, evaluateLocation(solution, loc)))
    val sortedLocations = evaluatedLocations.sortBy(_._2)
    
    // Selecciona aleatoriamente una ubicación de la lista de candidatos
    val candidateSize = math.min(graspListSize, sortedLocations.length)
    val chosenNode = sortedLocations(Random.nextInt(candidateSize))
    
    // Añade la ubicación elegida a la solución y actualiza los costos
    solution.addElement(chosenNode._1, chosenNode._2).updateCosts(problem)
  }

  // Inicializa el conjunto de ubicaciones no elegidas
  private def initNotChosenLocations: Set[Int] = (0 until problem.locations).toSet
  // Implementation of multiRun to execute the algorithm multiple times
  override def multiRun(): List[Solution] = {
    println("Enter the number of runs (default: 10):")
    val numRuns = scala.io.StdIn.readLine().toIntOption.getOrElse(10)

    println("Choose the improvement method:")
    println("1. Local Search")
    println("2. Simulated Annealing")
    val improvementChoice = scala.io.StdIn.readLine().toIntOption.getOrElse(1)

    val improver = improvementChoice match {
      case 1 =>
        println("Enter the maximum number of iterations without improvement for local search (default: 100):")
        val maxIterationsWithoutImprovement = scala.io.StdIn.readLine().toIntOption.getOrElse(100)
        new LocationLocalSearch(problem, maxIterationsWithoutImprovement)
      case 2 =>
        println("Enter the initial temperature for simulated annealing (default: 100.0):")
        val initialTemperature = scala.io.StdIn.readLine().toDoubleOption.getOrElse(100.0)
        println("Enter the cooling rate for simulated annealing (default: 0.95):")
        val coolingRate = scala.io.StdIn.readLine().toDoubleOption.getOrElse(0.95)
        println("Enter the maximum iterations without improvement for simulated annealing (default: 1000):")
        val maxIterationsWithoutImprovement = scala.io.StdIn.readLine().toIntOption.getOrElse(1000)
        println("Enter the iterations per temperature for simulated annealing (default: 100):")
        val iterationsPerTemperature = scala.io.StdIn.readLine().toIntOption.getOrElse(100)
        new LocationSimulatedAnnealing(problem, maxIterationsWithoutImprovement, initialTemperature, iterationsPerTemperature, coolingRate)
      case _ =>
        println("Invalid choice. Using Local Search as default.")
        new LocationLocalSearch(problem, 100)
    }

    (0 until numRuns).map { _ =>
      val initialSolution = solve()
      improver.improve(initialSolution)
    }.toList
  }

  // Evalúa el beneficio de añadir una ubicación candidata a la solución actual
  private def evaluateLocation(solution: LocationSolution, candidate: Int): Int = {
    (0 until problem.clients).foldLeft(0) { (sum, client) =>
      sum + math.min(problem.distanceMatrix(candidate)(client), solution.costs(client))
    }
  }
}
