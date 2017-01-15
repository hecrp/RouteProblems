/**
  * Created by hector on 1/15/17.
  */
object App {
  def main(args: Array[String]): Unit = {
    val parsedFile = Parser.parse("graphs/matrix3.txt")
    val problem = new Problem(parsedFile._1, parsedFile._2)

    val solver: Heuristic = new Heuristic(problem)
    var solution: Solution = solver.NNStrategy(0)
    println(solution)
    solution = solver.TwoOpt()
    println(solution)
  }
}
