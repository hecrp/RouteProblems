import problems.{Problem, RoutingProblem, LocationProblem}
import solvers.{TSPNN, TSPNNGRASP, PMedianGreedy}
import utils.Parser

/**
  * Created by hector on 1/15/17.
  */

object App {
  def main(args: Array[String]): Unit = {
    val parsedFile = Parser.parse("graphs/matrix3PMEDIAN.txt")
    var TSPproblem = new RoutingProblem(parsedFile._1, parsedFile._2)

    val TSPArguments: List[Int] = List(0,1,2,3,4,5,6,7,8,9)
    /*
    println("GREEDY")
    new TSPNN(TSPproblem).multiRun(TSPArguments).foreach(println(_))
    println("GRASP")
    new TSPNNGRASP(TSPproblem).multiRun(TSPArguments).foreach(println(_))
    */
    println("P-MEDIAN")
    var PMEDIANproblem = new LocationProblem(parsedFile._1, parsedFile._2, parsedFile._2, (parsedFile._2 / 3))
    println(new PMedianGreedy(PMEDIANproblem).solve())

  }
}
