package improvers

import problems.RoutingProblem
import solutions.RoutingSolution

/**
  * Created by hector on 1/18/17.
  */

class RoutingThreeOPT(problem: RoutingProblem, maxIterationsWithoutImprovement: Int = 100) extends RoutingImprover {
  override def improve(initialSolution: RoutingSolution): RoutingSolution = {
    def improveRecursive(bestSolution: RoutingSolution, improve: Int): RoutingSolution = {
      if (improve >= maxIterationsWithoutImprovement) bestSolution
      else {
        val size = bestSolution.getSize
        val newBestSolution = (for {
          i <- 0 until (size - 4)
          j <- (i + 1) until (size - 3)
          k <- (j + 1) until (size - 2)
          candidate = threeOptSwap(bestSolution, i, j, k)
          if candidate.getLastDistance < bestSolution.getLastDistance
        } yield candidate).headOption.getOrElse(bestSolution)

        if (newBestSolution == bestSolution) improveRecursive(bestSolution, improve + 1)
        else improveRecursive(newBestSolution, 0)
      }
    }

    improveRecursive(initialSolution, 0)
  }

  private def threeOptSwap(solution: RoutingSolution, i: Int, j: Int, k: Int): RoutingSolution = {
    val size = problem.graphSize + 1
    val tour = solution.tour.map(_._1)
    
    val segment1 = tour.slice(0, i + 1)
    val segment2 = tour.slice(i + 1, j + 1)
    val segment3 = tour.slice(j + 1, k + 1)
    val segment4 = tour.slice(k + 1, size - 1)

    val possibleTours = List(
      segment1 ++ segment2 ++ segment3 ++ segment4,
      segment1 ++ segment3.reverse ++ segment2.reverse ++ segment4,
      segment1 ++ segment2.reverse ++ segment3 ++ segment4,
      segment1 ++ segment3 ++ segment2 ++ segment4,
      segment1 ++ segment3.reverse ++ segment2 ++ segment4,
      segment1 ++ segment2 ++ segment3.reverse ++ segment4,
      segment1 ++ segment3 ++ segment2.reverse ++ segment4
    )

    val bestTour = possibleTours.minBy { tour =>
      tour.zip(tour.tail :+ tour.head).map { case (a, b) =>
        problem.distanceMatrix(a)(b)
      }.sum
    }

    val candidate = new RoutingSolution(Vector.empty)
    bestTour.foldLeft(candidate)((acc, node) => acc.addElement(Vector((node, 0)), problem))
      .addElement(Vector((bestTour.head, 0)), problem)
  }
}