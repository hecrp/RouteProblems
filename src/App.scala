import problems.{Problem, RoutingProblem, LocationProblem}
import solvers.{TSPNN, TSPNNGRASP, PMedianGreedy, PMedianGRASP}
import utils.{Parser, PMedianParser}

/**
  * Created by hector on 1/15/17.
  */

object App {
  def main(args: Array[String]): Unit = {
    val parsedFile = Parser.parse("graphs/tsp/matrix3.txt")
    var TSPproblem = new RoutingProblem(parsedFile._1, parsedFile._2)

    val TSPArguments: List[Int] = List(0,1,2,3,4,5,6,7,8,9)
    /*
    println("GREEDY")
    new TSPNN(TSPproblem).multiRun(TSPArguments).foreach(println(_))
    println("GRASP")
    new TSPNNGRASP(TSPproblem).multiRun(TSPArguments).foreach(println(_))
    */
    val PMedianParsedFile = PMedianParser.parse("graphs/pmedian/pmed17.txt.table.p100.A.txt")
    var PMEDIANproblem = new LocationProblem(PMedianParsedFile._1, 200, 200, 10)
    println("P-MEDIAN GREEDY\n")
    println(new PMedianGreedy(PMEDIANproblem).solve())
    println("P-MEDIAN GRASP\n")
    println(new PMedianGRASP(PMEDIANproblem).multiRun(List(5)))

  }
}
