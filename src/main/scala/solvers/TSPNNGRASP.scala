package solvers

import problems.RoutingProblem
import solutions.RoutingSolution
import solutions.Solution
import improvers.RoutingTwoOPT

import scala.util.Random

/**
  * Created by hector on 1/18/17.
  */
class TSPNNGRASP(problem: RoutingProblem, graspListSize: Int) extends TSPNN(problem) with MultiRunnable {

  // Método principal para resolver el TSP usando GRASP
  override def solve(first: Int = 0): RoutingSolution = {
    nnGraspStrategy(first)
  }

  // Implementación de la estrategia GRASP para el TSP
  private def nnGraspStrategy(firstNode: Int): RoutingSolution = {
    // Función recursiva para construir la solución
    def buildSolution(solution: RoutingSolution, notChosenNodes: Set[Int]): RoutingSolution = {
      if (solution.isComplete(problem.graphSize)) {
        // Si la solución está completa, añade el nodo inicial para cerrar el ciclo
        solution.addElement(Vector((firstNode, 0)), problem)
      } else {
        // Selecciona aleatoriamente un nodo de la lista de candidatos GRASP
        val nodeToAdd = popGraspNotChosen(solution, notChosenNodes)
        buildSolution(solution.addElement(Vector((nodeToAdd, 0)), problem), notChosenNodes - nodeToAdd)
      }
    }

    // Inicializa la solución con el nodo de inicio
    val initialSolution = new RoutingSolution(Vector.empty).addElement(Vector((firstNode, 0)), problem)
    // Crea un conjunto con todos los nodos excepto el inicial
    val initialNotChosenNodes = (0 until problem.graphSize).toSet - firstNode

    buildSolution(initialSolution, initialNotChosenNodes)
  }

  // Selecciona aleatoriamente un nodo de la lista de candidatos GRASP
  private def popGraspNotChosen(solution: RoutingSolution, notChosenNodes: Set[Int]): Int = {
    // Ajusta el tamaño de la lista GRASP al mínimo entre el tamaño predefinido y los nodos disponibles
    val currentGraspListSize = Math.min(graspListSize, notChosenNodes.size)

    // Crea la lista de candidatos GRASP ordenada por distancia
    val graspList = notChosenNodes.toList
      .sortBy(node => problem.distanceMatrix(solution.getLastNode)(node))
      .take(currentGraspListSize)

    // Selecciona aleatoriamente un nodo de la lista de candidatos
    graspList(Random.nextInt(currentGraspListSize))
  }

  // Encuentra el nodo más cercano entre los no elegidos (no se usa en GRASP, pero se mantiene por compatibilidad)
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

