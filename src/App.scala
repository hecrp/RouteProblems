/**
  * Created by hector on 1/15/17.
  */
object App {
  def main(args: Array[String]): Unit = {
    val parsedFile = Parser.parse("graphs/matrix2.txt")
    val problem = new Problem(parsedFile._1, parsedFile._2)

    val TSPArguments: List[Int] = List(0,1,2,3,4,5,6,7,8,9)

    new Heuristic(problem).multirun(TSPArguments).foreach(println(_))
  }
}
