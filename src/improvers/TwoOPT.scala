package improvers

import problems.RoutingProblem
import solutions.RoutingSolution

/**
  * Created by hector on 1/18/17.
  */
class TwoOPT(problem: RoutingProblem) extends RoutingImprover{

  private val ALPHA = 500

  override def improve(initialSolution: RoutingSolution): RoutingSolution = {
    val size: Int = initialSolution.getSize
    var improve: Int = 0
    //Before generating a "2OPted" solution, the initial is the best
    var bestSolution: RoutingSolution = initialSolution
    //When we reach ALPHA iterations without improve the solution, we close the loop
    while (improve < ALPHA) {
      for (i <- 0 until (size - 1)) {
        for (k <- (i + 1) until size) {
          val candidate: RoutingSolution = TwoOptSwap(bestSolution, i, k)
          if (candidate.getLastDistance < bestSolution.getLastDistance) {
            bestSolution = candidate
            improve = 0
          }
        }
      }
      improve += 1
    }
    bestSolution
  }

  private def TwoOptSwap(solution: RoutingSolution, iValue: Int, kValue: Int): RoutingSolution = {
    val size: Int = problem.graphSize + 1
    val candidate: RoutingSolution = new RoutingSolution(problem.graphSize)

    //From 0 to iValue, the elements in the same order as the initial solution
    for(c <- 0 until iValue)
      candidate.addElement(solution.getNodeIn(c), problem)


    //From iValue to kValue, the elements in reverse order
    var dec: Int = 0
    for(c <- iValue to kValue) {
      candidate.addElement(solution.getNodeIn(kValue - dec), problem)
      dec += 1
    }

    //After kValue, the elements in the same order as the initial solution again
    for(c <- kValue + 1 until size - 1){
      candidate.addElement(solution.getNodeIn(c), problem)
    }

    //Now we close the path appending the first node to the solution
    candidate.addElement(candidate.getFirstNode, problem)
    candidate
  }


}
