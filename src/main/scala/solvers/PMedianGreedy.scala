package solvers

import improvers.LocationSimulatedAnnealing
import problems.LocationProblem
import solutions.LocationSolution
import solvers.MultiRunnable
import solutions.Solution
import improvers.LocationLocalSearch

/**
  * Created by hector on 1/18/17.
  */
class PMedianGreedy(problem: LocationProblem) extends MultiRunnable {
  // Método principal para resolver el problema P-Mediana de forma voraz
  def solve(): LocationSolution = {
    // Inicializa la solución con un conjunto vacío de ubicaciones y costos máximos
    val initialSolution = LocationSolution(Set.empty, Vector.fill(problem.clients)(Int.MaxValue), 0)

    // Construye la solución iterativamente, añadiendo la mejor ubicación en cada paso
    val finalSolution = (0 until problem.p).foldLeft(initialSolution) { (solution, _) =>
      // Encuentra la mejor ubicación y su valor asociado
      val (bestLocation, bestValue) = (0 until problem.locations)
        .map(location => (location, evaluateLocation(solution, location)))
        .minBy(_._2)
      // Añade la mejor ubicación a la solución y actualiza los costos
      solution.addElement(bestLocation, bestValue).updateCosts(problem)
    }
    return finalSolution
  }

  // Evalúa el beneficio de añadir una ubicación candidata a la solución actual
  private def evaluateLocation(solution: LocationSolution, candidate: Int): Int = {
    (0 until problem.clients).foldLeft(0) { (sum, client) =>
      // Calcula el mínimo entre la distancia al candidato y el costo actual para cada cliente
      sum + math.min(problem.distanceMatrix(candidate)(client), solution.costs(client))
    }
  }
  // Implementación de multiRun para ejecutar el algoritmo múltiples veces
  override def multiRun(): List[Solution] = {
    println("Enter the number of runs:")
    val numRuns = scala.io.StdIn.readInt()

    println("Choose an improvement method:")
    println("1. Local Search")
    println("2. Simulated Annealing")
    println("3. No improvement")
    val improveChoice = scala.io.StdIn.readLine().trim

    val improver = improveChoice match {
      case "1" =>
        println("Enter the maximum number of iterations without improvement for local search:")
        val maxIterationsWithoutImprovement = scala.io.StdIn.readInt()
        new LocationLocalSearch(problem, maxIterationsWithoutImprovement)
      case "2" =>
        println("Enter the maximum number of iterations without improvement for simulated annealing:")
        val maxIterationsWithoutImprovement = scala.io.StdIn.readInt()
        println("Enter the initial temperature:")
        val initialTemperature = scala.io.StdIn.readDouble()
        println("Enter the number of iterations per temperature:")
        val iterationsPerTemperature = scala.io.StdIn.readInt()
        println("Enter the cooling rate:")
        val coolingRate = scala.io.StdIn.readDouble()
        new LocationSimulatedAnnealing(problem, maxIterationsWithoutImprovement, initialTemperature, iterationsPerTemperature, coolingRate)
      case _ => null
    }

    // Ejecuta el algoritmo el número de veces especificado y aplica la mejora si se seleccionó
    (0 until numRuns).map { _ =>
      val initialSolution = solve()
      if (improver != null) {
        improver.improve(initialSolution)
      } else {
        initialSolution
      }
    }.toList
  }
}
