package solvers

import problems.RoutingProblem
import solutions.RoutingSolution

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by hector on 1/18/17.
  */
class TSPNNGRASP(problem: RoutingProblem) extends TSPNN(problem) {

  private var GRASPListSize = 3

  override def solve(first: Int = 0): RoutingSolution = {
    NNGRASPStrategy(first)
  }

  private def NNGRASPStrategy(firstNode: Int): RoutingSolution = {
    val solution = new RoutingSolution(problem.graphSize)
    val notChosenNodes: mutable.HashSet[Int] = new mutable.HashSet()

    initNotChosenList(notChosenNodes)
    notChosenNodes.remove(firstNode)

    solution.addElement(firstNode, problem)

    while(!solution.isComplete){
      val nodeToAdd = popGRASPNotChosen(solution, notChosenNodes)
      solution.addElement(nodeToAdd, problem)
    }

    solution.addElement(firstNode, problem)

    solution
  }

  private def popGRASPNotChosen(solution: RoutingSolution, notChosenNodes: mutable.HashSet[Int]): Int = {
    var (chosenNode, nearestDistance): (Int, Int) = (-1, Int.MaxValue)
    //List which contains GRASPListSize elements in which we will extract a random element
    val GRASPList = new ListBuffer[Int]()
    //Temporal List used to build GRASPList
    val temporalGraspNodes =notChosenNodes.clone()

    //When we have less unchosen nodes than the GRASP list size, we have to resize that list
    if(notChosenNodes.size < GRASPListSize)
      GRASPListSize = notChosenNodes.size

    //We build the GRASPList choosing the nearest not chosen node in each iteration
    for(i <- 0 until GRASPListSize) {
      var chosenGRASPNode: Int = -1
      for(node <- temporalGraspNodes)
        if(problem.distanceMatrix(solution.getLastNode)(node) < nearestDistance) {
          chosenGRASPNode = node
          nearestDistance = problem.distanceMatrix(solution.getLastNode)(node)
        }
      //We add the nearest node to the GRASPList
      temporalGraspNodes.remove(chosenGRASPNode)
      GRASPList.append(chosenGRASPNode)
      //Reset the minimum distance
      nearestDistance = Int.MaxValue
    }

    //We choose a random element in the GRASPList which will be the node to add in the NNGRASP solution
    val randomIndex = scala.util.Random.nextInt(GRASPListSize)
    chosenNode = GRASPList(randomIndex)
    //Now we can extract the final chosen node from the notChosenNodes list
    notChosenNodes.remove(chosenNode)
    chosenNode
  }
}
