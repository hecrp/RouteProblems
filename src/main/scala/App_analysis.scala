import problems.RoutingProblem
import solvers.{TSPNNGRASP, TSPAntColonyOptimization}
import improvers._
import solutions.RoutingSolution
import java.io.{FileWriter, BufferedWriter}
import scala.io.Source
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object App_Analysis {
  def main(args: Array[String]): Unit = {
    // Configuration to enable/disable solvers
    val enableTSPNNGRASP = false
    val enableACO = true

    // Load TSP problems
    val problemFiles = List("matrix2.txt", "matrix3.txt", "matrix4.txt") // Add more problem files as needed
    val problems = problemFiles.map(file => (file, loadTSPProblem(file))).toMap
    
    // Define parameter ranges for analysis
    val multiRunCounts = List(1, 3, 5)
    val twoOptMaxIterations = List(10, 25, 50)
    val threeOptMaxIterations = List(10, 25, 50)
    val graspListSizes = List(3, 5, 7)
    
    // Updated Simulated Annealing parameters
    val saInitialTemperatures = List(1000.0, 5000.0, 10000.0)
    val saCoolingRates = List(0.99, 0.995, 0.999)
    val saIterations = List(10, 50, 100)
    
    // ACO parameters
    val numAnts = List(10, 20, 30)
    val numIterations = List(10, 20, 30)
    val alpha = List(1.0, 2.0, 3.0)
    val beta = List(2.0, 3.0, 4.0)
    val rho = List(0.1, 0.2, 0.3)
    val q = List(1.0, 2.0, 3.0)
    
    // Create a log file with timestamp
    val timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd_HHmmss"))
    val logFileName = s"NOW_tsp_analysis_results_${timestamp}.csv"
    val logWriter = new BufferedWriter(new FileWriter(logFileName))
    
    // Updated column names to match the CSV format
    logWriter.write("ProblemFile;Solver;Improver;MultiRuns;TwoOptMaxIter;ThreeOptMaxIter;GraspListSize;SAInitialTemp;SACoolingRate;SAIterations;NumAnts;NumIterations;Alpha;Beta;Rho;Q;TotalCost;AvgCost;MaxCost;MinCost;MaxDistance;Time;Route\n")

    // Run experiments for each problem
    for ((problemFile, problem) <- problems) {
      // TSPNNGRASP experiments
      if (enableTSPNNGRASP) {
        for {
          multiRuns <- multiRunCounts
          graspListSize <- graspListSizes
        } {
          val solver = new TSPNNGRASP(problem, graspListSize)

          // TwoOPT
          for (maxIter <- twoOptMaxIterations) {
            val improver = new RoutingTwoOPT(problem, maxIter)
            runExperiment(logWriter, problemFile, solver, improver, multiRuns, maxIter, 0, graspListSize, 0.0, 0.0, 0, 0, 0, 0.0, 0.0, 0.0, 0.0)
          }

          // ThreeOPT
          for (maxIter <- threeOptMaxIterations) {
            val improver = new RoutingThreeOPT(problem, maxIter)
            runExperiment(logWriter, problemFile, solver, improver, multiRuns, 0, maxIter, graspListSize, 0.0, 0.0, 0, 0, 0, 0.0, 0.0, 0.0, 0.0)
          }

          // SimulatedAnnealing
          for {
            initialTemp <- saInitialTemperatures
            coolingRate <- saCoolingRates
            iterations <- saIterations
          } {
            val improver = new RoutingSimulatedAnnealing(problem, initialTemp, coolingRate, iterations)
            runExperiment(logWriter, problemFile, solver, improver, multiRuns, 0, 0, graspListSize, initialTemp, coolingRate, iterations, 0, 0, 0.0, 0.0, 0.0, 0.0)
          }

          println(s"Completed TSPNNGRASP: Problem = $problemFile, MultiRuns = $multiRuns, GRASP List Size = $graspListSize")
        }
      }

      // ACO experiments
      if (enableACO) {
        for {
          multiRuns <- multiRunCounts
          ants <- numAnts
          iter <- numIterations
          a <- alpha
          b <- beta
          r <- rho
          qq <- q
        } {
          val solver = new TSPAntColonyOptimization(problem, ants, iter, a, b, r, qq)

          // TwoOPT
          for (maxIter <- twoOptMaxIterations) {
            val improver = new RoutingTwoOPT(problem, maxIter)
            runExperiment(logWriter, problemFile, solver, improver, multiRuns, maxIter, 0, 0, 0.0, 0.0, 0, ants, iter, a, b, r, qq)
          }

          // ThreeOPT
          for (maxIter <- threeOptMaxIterations) {
            val improver = new RoutingThreeOPT(problem, maxIter)
            runExperiment(logWriter, problemFile, solver, improver, multiRuns, 0, maxIter, 0, 0.0, 0.0, 0, ants, iter, a, b, r, qq)
          }

          // SimulatedAnnealing
          for {
            initialTemp <- saInitialTemperatures
            coolingRate <- saCoolingRates
            iterations <- saIterations
          } {
            val improver = new RoutingSimulatedAnnealing(problem, initialTemp, coolingRate, iterations)
            runExperiment(logWriter, problemFile, solver, improver, multiRuns, 0, 0, 0, initialTemp, coolingRate, iterations, ants, iter, a, b, r, qq)
          }

          println(s"Completed ACO: Problem = $problemFile, MultiRuns = $multiRuns, Ants = $ants, Iterations = $iter")
        }
      }
    }

    logWriter.close()
    println(s"Analysis complete. Results written to $logFileName")
  }

  // Add missing methods
  def loadTSPProblem(filePath: String): RoutingProblem = {
    val lines = Source.fromFile(s"graphs/tsp/$filePath").getLines().toList
    val dimension = lines.length
    val distanceMatrix = lines.map { line =>
      line.trim.split("\\s+").map(_.toInt).toArray
    }.toArray
    RoutingProblem(distanceMatrix, dimension)
  }

  def runExperiment(logWriter: BufferedWriter, problemFile: String, solver: Any, improver: Any, multiRuns: Int, twoOptMaxIter: Int, threeOptMaxIter: Int, graspListSize: Int, saInitialTemp: Double, saCoolingRate: Double, saIterations: Int, numAnts: Int, numIterations: Int, alpha: Double, beta: Double, rho: Double, q: Double): Unit = {
    val startTime = System.currentTimeMillis()
    val solutions = (1 to multiRuns).map { _ =>
      val initialSolution = solver.asInstanceOf[TSPAntColonyOptimization].solve()
      improver match {
        case twoOpt: RoutingTwoOPT => twoOpt.improve(initialSolution)
        case threeOpt: RoutingThreeOPT => threeOpt.improve(initialSolution)
        case sa: RoutingSimulatedAnnealing => sa.improve(initialSolution)
        case _ => initialSolution
      }
    }.toList
    val executionTime = System.currentTimeMillis() - startTime

    val costs = solutions.map(_.getTotalCost)
    val bestCost = costs.min
    val worstCost = costs.max
    val avgCost = costs.sum.toDouble / multiRuns
    val maxDistance = solutions.map(_.getMaxDistance).max

    val improverName = improver match {
      case _: RoutingTwoOPT => "TwoOPT"
      case _: RoutingThreeOPT => "ThreeOPT"
      case _: RoutingSimulatedAnnealing => "SimulatedAnnealing"
      case _ => "None"
    }

    val solverName = solver match {
      case _: TSPNNGRASP => "TSPNNGRASP"
      case _: TSPAntColonyOptimization => "ACO"
      case _ => "Unknown"
    }

    val bestSolution = solutions.minBy(_.getTotalCost)
    val route = bestSolution.getRoute.mkString("[", ", ", "]")

    val resultLine = s"$problemFile;$solverName;$improverName;$multiRuns;$twoOptMaxIter;$threeOptMaxIter;$graspListSize;$saInitialTemp;$saCoolingRate;$saIterations;$numAnts;$numIterations;$alpha;$beta;$rho;$q;$bestCost;$avgCost;$worstCost;$bestCost;$maxDistance;$executionTime;$route"
    logWriter.write(resultLine + "\n")
    logWriter.flush()
  }
}
