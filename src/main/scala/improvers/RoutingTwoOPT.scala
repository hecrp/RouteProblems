package improvers

import problems.RoutingProblem
import solutions.RoutingSolution

/**
  * Created by hector on 1/18/17.
  */
class RoutingTwoOPT(problem: RoutingProblem, maxIterationsWithoutImprovement: Int) extends RoutingImprover {

  override def improve(initialSolution: RoutingSolution): RoutingSolution = {
    // Función recursiva para mejorar la solución
    def improveRecursive(bestSolution: RoutingSolution, improve: Int): RoutingSolution = {
      // Si se han realizado maxIterationsWithoutImprovement iteraciones sin mejora, se devuelve la mejor solución encontrada
      if (improve >= maxIterationsWithoutImprovement) bestSolution
      else {
        val size = bestSolution.getSize
        // Se busca la mejor solución aplicando el intercambio 2-opt a todos los pares posibles
        val newBestSolution = (for {
          i <- 0 until (size - 1)
          k <- (i + 1) until size
          candidate = RoutingTwoOPTSwap(bestSolution, i, k)
          // Se selecciona el candidato si mejora la distancia actual
          if candidate.getLastDistance < bestSolution.getLastDistance
        } yield candidate).headOption.getOrElse(bestSolution)

        // Si no se ha encontrado una mejor solución, se incrementa el contador de mejoras
        // Si se ha encontrado una mejor solución, se reinicia el contador
        if (newBestSolution == bestSolution) improveRecursive(bestSolution, improve + 1)
        else improveRecursive(newBestSolution, 0)
      }
    }

    // Se inicia el proceso de mejora con la solución inicial
    improveRecursive(initialSolution, 0)
  }

  // Realiza el intercambio 2-opt entre las posiciones i y k
  private def RoutingTwoOPTSwap(solution: RoutingSolution, iValue: Int, kValue: Int): RoutingSolution = {
    val size = problem.graphSize + 1
    val candidate = new RoutingSolution(Vector.empty)

    // Se divide el tour en tres partes: antes del intercambio, la parte a invertir, y después del intercambio
    val beforeReverse = (0 until iValue).map(solution.getNodeIn)
    val reversed = (iValue to kValue).reverse.map(solution.getNodeIn)
    val afterReverse = ((kValue + 1) until (size - 1)).map(solution.getNodeIn)

    // Se construye el nuevo tour concatenando las tres partes
    val newTour = (beforeReverse ++ reversed ++ afterReverse)
      .foldLeft(candidate)((acc, node) => acc.addElement(Vector((node, 0)), problem))

    // Se añade el primer nodo al final para cerrar el ciclo, solo si el tour no está vacío
    if (newTour.tour.nonEmpty) {
      newTour.addElement(Vector((newTour.tour.head._1, 0)), problem)
    } else {
      newTour
    }
  }
}
