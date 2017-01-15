import scala.collection.mutable

/**
  * Created by hector on 1/14/17.
  */

class Heuristic(problem: Problem) {

  private val notChosenNodes: mutable.HashSet[Int] = new mutable.HashSet()

  var initialSolution = new Solution(problem.graphSize)
  var bestSolution: Solution = new Solution(problem.graphSize)

  private var firstNode = 0
  private val ALPHA = 5000

  def NNStrategy(first: Int = 0): Solution = {
    firstNode = first
    initNotChosenList()

    addElementToSolution(firstNode, initialSolution)

    while(!initialSolution.isComplete) {
      val nodeToInsert: Int = popNearestNotChosen()
      addElementToSolution(nodeToInsert, initialSolution)
    }

    addElementToSolution(firstNode, initialSolution)

    initialSolution
  }

  def TwoOpt(): Solution = {
    val size: Int = initialSolution.getSize
    var improve: Int = 0
    //Before generating a "2OPted" solution, the initial is the best
    bestSolution = initialSolution
    //When we reach ALPHA iterations without improve the solution, we close the loop
    while (improve < ALPHA) {
      for (i <- 0 until (size - 1)) {
        for (k <- (i + 1) until size) {
          val candidate: Solution = TwoOptSwap(i, k)
          if (candidate.getLastDistance < bestSolution.getLastDistance) {
            updateSolution(candidate)
            improve = 0
          }
        }
      }
      improve += 1
    }
    bestSolution
  }

  private def TwoOptSwap(iValue: Int, kValue: Int): Solution = {
    val size: Int = initialSolution.getSize
    val candidate: Solution = new Solution(problem.graphSize)

    //From 0 to iValue, the elements in the same order as the initial solution
    for(c <- 0 until iValue)
      addElementToSolution(bestSolution.tour(c)._1, candidate)

    //From iValue to kValue, the elements in reverse order
    var dec: Int = 0
    for(c <- iValue to kValue) {
      addElementToSolution(bestSolution.tour(kValue - dec)._1, candidate)
      dec += 1
    }

    //After kValue, the elements in the same order as the initial solution again
    for(c <- kValue + 1 until size - 1){
      addElementToSolution(bestSolution.tour(c)._1, candidate)
    }

    //Now we close the path appending the first node to the solution
    addElementToSolution(candidate.tour.head._1, candidate)
    candidate
  }

  private def updateSolution(updated: Solution): Unit = {
    bestSolution = updated
  }

  private def initNotChosenList(): Unit = {
    for(i <- 0 until problem.graphSize)
      notChosenNodes.add(i)
    notChosenNodes.remove(firstNode)
  }

  private def addElementToSolution(node: Int, solution: Solution): Unit = {
    if(solution.isEmpty)
      solution.addElement(node, 0)
    else
      solution.addElement(node, solution.getLastDistance + problem.distanceMatrix(solution.getLastNode)(node))
  }



  private def popNearestNotChosen(): Int = {
    var (nearestNode, nearestDistance): (Int, Int) = (-1, Int.MaxValue)

    for(node <- notChosenNodes)
      if(problem.distanceMatrix(initialSolution.getLastNode)(node) < nearestDistance)
        (nearestNode, nearestDistance) = (node, problem.distanceMatrix(initialSolution.getLastNode)(node))

    notChosenNodes.remove(nearestNode)
    nearestNode
  }
}
