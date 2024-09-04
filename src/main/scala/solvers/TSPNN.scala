package solvers

import improvers.{RoutingImprover, RoutingTwoOPT}
import problems.RoutingProblem
import solutions.{RoutingSolution, Solution}

import scala.annotation.tailrec

/**
  * Created by hector on 1/14/17.
  */

class TSPNN(problem: RoutingProblem) extends MultiRunnable {
  // Método principal para resolver el TSP usando el algoritmo del vecino más cercano
  def solve(firstNode: Int = 0): RoutingSolution = {
    NNStrategy(firstNode)
  }

  // Implementación de la estrategia del vecino más cercano
  private def NNStrategy(firstNode: Int): RoutingSolution = {
    @tailrec
    def buildSolution(solution: RoutingSolution, notChosenNodes: Set[Int]): RoutingSolution = {
      if (solution.isComplete(problem.graphSize)) {
        // Si la solución está completa, añade el nodo inicial para cerrar el ciclo
        solution.addElement(Vector((firstNode, 0)), problem)
      } else {
        // Encuentra el nodo más cercano no elegido y lo añade a la solución
        val nodeToAdd = findNearestNotChosen(solution, notChosenNodes)
        buildSolution(solution.addElement(Vector((nodeToAdd, 0)), problem), notChosenNodes - nodeToAdd)
      }
    }

    // Inicializa la solución con el nodo de inicio
    val initialSolution = new RoutingSolution(Vector.empty).addElement(Vector((firstNode, 0)), problem)
    // Crea un conjunto con todos los nodos excepto el inicial
    val initialNotChosenNodes = (0 until problem.graphSize).toSet - firstNode

    buildSolution(initialSolution, initialNotChosenNodes)
  }

  // Encuentra el nodo más cercano entre los no elegidos
  private def findNearestNotChosen(solution: RoutingSolution, notChosenNodes: Set[Int]): Int = {
    notChosenNodes.minBy(node => problem.distanceMatrix(solution.getLastNode)(node))
  }

  // Implementación de multiRun para ejecutar el algoritmo múltiples veces
  override def multiRun(): List[Solution] = {
    println("Ingrese el número de ejecuciones:")
    val numRuns = scala.io.StdIn.readInt()

    println("Ingrese el número máximo de iteraciones sin mejora para 2OPT:")
    val maxIterationsWithoutImprovement = scala.io.StdIn.readInt()

    val RoutingTwoOPT = new RoutingTwoOPT(problem, maxIterationsWithoutImprovement)

    (0 until numRuns).map { _ =>
      val initialSolution = solve()
      RoutingTwoOPT.improve(initialSolution)
    }.toList
  }
}
