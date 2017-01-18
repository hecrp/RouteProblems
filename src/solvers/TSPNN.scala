package solvers

import improvers.{RoutingImprover, TwoOPT}
import problems.RoutingProblem
import solutions.{RoutingSolution, Solution}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by hector on 1/14/17.
  */

class TSPNN(problem: RoutingProblem) extends MultiRunnable{

  val twoOPT: TwoOPT = new TwoOPT(problem)

  def solve(firstNode: Int = 0): RoutingSolution = {
    NNStrategy(firstNode)
  }

  private def NNStrategy(firstNode: Int): RoutingSolution = {
    val solution = new RoutingSolution(problem.graphSize)
    val notChosenNodes: mutable.HashSet[Int] = new mutable.HashSet()

    initNotChosenList(notChosenNodes)
    notChosenNodes.remove(firstNode)

    solution.addElement(firstNode, problem)

    while(!solution.isComplete){
      val nodeToAdd = popNearestNotChosen(solution, notChosenNodes)
      solution.addElement(nodeToAdd, problem)
    }

    solution.addElement(firstNode, problem)

    solution
  }

  protected def initNotChosenList(notChosenNodes: mutable.HashSet[Int]): Unit = {
    for(i <- 0 until problem.graphSize)
      notChosenNodes.add(i)
  }

  private def popNearestNotChosen(solution: RoutingSolution, notChosenNodes: mutable.HashSet[Int]): Int = {
    var (nearestNode, nearestDistance): (Int, Int) = (-1, Int.MaxValue)

    for(node <- notChosenNodes)
      if(problem.distanceMatrix(solution.getLastNode)(node) < nearestDistance) {
        nearestNode = node
        nearestDistance = problem.distanceMatrix(solution.getLastNode)(node)
      }

    notChosenNodes.remove(nearestNode)
    nearestNode
  }

  override def multiRun(arguments: List[Any]): List[Solution] = {
    val solutions = ListBuffer[Solution]()

    for(argument <- arguments){
      //Cast Any to Int using pattern matching
      val intArg = argument match {case n:Number => n.intValue()
        case x => throw new IllegalArgumentException(s"$x is not a number.")}

      solutions.append(twoOPT.improve(solve(intArg)))
    }
    solutions.toList
  }
}
