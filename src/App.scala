import problems.RoutingProblem
import solvers.{TSPNN, TSPNNGRASP}
import utils.Parser

/**
  * Created by hector on 1/15/17.
  */

object App {
  def main(args: Array[String]): Unit = {
    val parsedFile = Parser.parse("graphs/matrix3.txt")
    val problem = new RoutingProblem(parsedFile._1, parsedFile._2)

    val TSPArguments: List[Int] = List(0,1,2,3,4,5,6,7,8,9)

    println("GREEDY")
    new TSPNN(problem).multiRun(TSPArguments).foreach(println(_))
    println("GRASP")
    new TSPNNGRASP(problem).multiRun(TSPArguments).foreach(println(_))
  }
}
