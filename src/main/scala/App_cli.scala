import problems.{RoutingProblem, LocationProblem}
import solvers.{TSPNN, TSPNNGRASP, PMedianGreedy, PMedianGRASP}
import improvers.{RoutingTwoOPT, LocationLocalSearch, LocationSimulatedAnnealing}
import utils.{Parser, PMedianParser}
import scala.io.StdIn
import java.io.File

object App {
  def main(args: Array[String]): Unit = {
    println("""
      Welcome to the Route Problems Solver! 
      Author: Héctor Rodríguez Pérez
    """)
    while (true) {
      println("\nChoose a problem to solve:")
      println("1. Traveling Salesman Problem (TSP)")
      println("2. P-Median Problem")
      println("3. Multi-run TSP")
      println("4. Multi-run P-Median")
      println("5. Exit")
      
      val choice = StdIn.readLine("Enter your choice (1-5): ").trim
      
      choice match {
        case "1" => solveTSP()
        case "2" => solvePMedian()
        case "3" => multiRunTSP()
        case "4" => multiRunPMedian()
        case "5" => 
          println("Exiting. Goodbye!")
          System.exit(0)
        case _ => println("Invalid choice. Please try again.")
      }
    }
  }

  def solveTSP(): Unit = {
    println("\nSolving Traveling Salesman Problem (TSP)")
    val tspFile = selectFile("graphs/tsp")
    val (distanceMatrix, size) = Parser.parse(tspFile)
    val tspProblem = RoutingProblem(distanceMatrix, size)
    
    println("\nChoose a TSP solver:")
    println("1. Nearest Neighbor (NN)")
    println("2. Nearest Neighbor with GRASP")
    
    val solverChoice = StdIn.readLine("Enter your choice (1-2): ").trim
    
    val solver = solverChoice match {
      case "1" => new TSPNN(tspProblem)
      case "2" => 
        println("Enter GRASP list size (default is 5):")
        val graspListSize = StdIn.readLine().trim match {
          case "" => 5
          case n => n.toInt
        }
        new TSPNNGRASP(tspProblem, graspListSize)
      case _ => 
        println("Invalid choice. Using Nearest Neighbor.")
        new TSPNN(tspProblem)
    }
    
    val solution = solver.solve()
    println(s"Initial solution: $solution")
    
    println("\nDo you want to improve the solution using 2-Opt? (y/n)")
    val improveChoice = StdIn.readLine().trim.toLowerCase
    
    if (improveChoice == "y") {
      println("Enter max iterations without improvement for 2-Opt (default is 1000):")
      val maxIterationsWithoutImprovement = StdIn.readLine().trim match {
        case "" => 1000
        case n => n.toInt
      }
      val improver = new RoutingTwoOPT(tspProblem, maxIterationsWithoutImprovement)
      val improvedSolution = improver.improve(solution)
      println(s"Improved solution: $improvedSolution")
    }
  }

  def solvePMedian(): Unit = {
    println("\nSolving P-Median Problem")
    val pMedianFile = selectFile("graphs/pmedian")
    println(s"Parsing file: $pMedianFile")
    
    try {
      val (distanceMatrix, locations, clients, p) = PMedianParser.parse(pMedianFile)
      val intMatrix = distanceMatrix.map(_.map(d => (d * 100).round.toInt))
      val pMedianProblem = LocationProblem(intMatrix, locations, clients, p)
      
      println("\nChoose a P-Median solver:")
      println("1. Greedy")
      println("2. GRASP")
      
      val solverChoice = StdIn.readLine("Enter your choice (1-2): ").trim
      
      val solver = solverChoice match {
        case "1" => new PMedianGreedy(pMedianProblem)
        case "2" => 
          println("Enter GRASP list size (default is 5):")
          val graspListSize = StdIn.readLine().trim match {
            case "" => 5
            case n => n.toInt
          }
          new PMedianGRASP(pMedianProblem, graspListSize)
        case _ => 
          println("Invalid choice. Using Greedy.")
          new PMedianGreedy(pMedianProblem)
      }
      
      val solution = solver.solve()
      println(s"Initial solution: $solution")
      
      println("\nChoose an improvement method:")
      println("1. Local Search")
      println("2. Simulated Annealing")
      println("3. No improvement")
      
      val improveChoice = StdIn.readLine("Enter your choice (1-3): ").trim
      
      val improvedSolution = improveChoice match {
        case "1" => 
          println("Enter max iterations without improvement for Local Search (default is 1000):")
          val maxIterationsWithoutImprovement = StdIn.readLine().trim match {
            case "" => 1000
            case n => n.toInt
          }
          val improver = new LocationLocalSearch(pMedianProblem, maxIterationsWithoutImprovement)
          improver.improve(solution)
        case "2" =>
          println("Enter max iterations without improvement for Simulated Annealing (default is 1000):")
          val maxIterationsWithoutImprovement = StdIn.readLine().trim match {
            case "" => 1000
            case n => n.toInt
          }
          println("Enter initial temperature (default is 100.0):")
          val initialTemperature = StdIn.readLine().trim match {
            case "" => 100.0
            case n => n.toDouble
          }
          println("Enter iterations per temperature (default is 100):")
          val iterationsPerTemperature = StdIn.readLine().trim match {
            case "" => 100
            case n => n.toInt
          }
          println("Enter cooling rate (default is 0.95):")
          val coolingRate = StdIn.readLine().trim match {
            case "" => 0.95
            case n => n.toDouble
          }
          val improver = new LocationSimulatedAnnealing(pMedianProblem, maxIterationsWithoutImprovement, initialTemperature, iterationsPerTemperature, coolingRate)
          improver.improve(solution)
        case _ => solution
      }
      
      println(s"Final solution: $improvedSolution")
      println(s"Total cost: ${improvedSolution.getTotalCost}")
      println(s"Number of locations: ${improvedSolution.getNumberOfLocations}")
      println(s"Number of clients: ${improvedSolution.getNumberOfClients}")
      println(s"Average cost: ${improvedSolution.getAverageCost}")
      println(s"Max cost: ${improvedSolution.getMaxCost}")
      println(s"Min cost: ${improvedSolution.getMinCost}")
    } catch {
      case e: Exception => 
        println(s"Error solving P-Median problem: ${e.getMessage}")
        e.printStackTrace()
    }
  }
  def multiRunTSP(): Unit = {
    println("\nMulti-run Traveling Salesman Problem (TSP)")
    val tspFile = selectFile("graphs/tsp")
    val (distanceMatrix, size) = Parser.parse(tspFile)
    val tspProblem = RoutingProblem(distanceMatrix, size)
    
    println("\nChoose a TSP solver:")
    println("1. Nearest Neighbor (NN)")
    println("2. Nearest Neighbor with GRASP")
    
    val solverChoice = StdIn.readLine("Enter your choice (1-2): ").trim
    
    val solver = solverChoice match {
      case "1" => new TSPNN(tspProblem)
      case "2" => 
        println("Enter GRASP list size (default is 5):")
        val graspListSize = StdIn.readLine().trim match {
          case "" => 5
          case n => n.toInt
        }
        new TSPNNGRASP(tspProblem, graspListSize)
      case _ => 
        println("Invalid choice. Using Nearest Neighbor.")
        new TSPNN(tspProblem)
    }
    
    val solutions = solver.multiRun()
    
    println("\nResults of multi-run:")
    solutions.zipWithIndex.foreach { case (solution, index) =>
      println(s"Run ${index + 1}: ${solution.getTotalCost}")
    }
    
    val bestSolution = solutions.minBy(solution => solution.getTotalCost)
    println(s"\nBest solution found: ${bestSolution.getTotalCost}")
    println(s"Max distance: ${bestSolution.getMaxDistance}")
  }

  def multiRunPMedian(): Unit = {
    println("\nMulti-run P-Median Problem")
    val pMedianFile = selectFile("graphs/pmedian")
    val (distanceMatrix, clients, locations, p) = PMedianParser.parse(pMedianFile)
    val pMedianProblem = LocationProblem(distanceMatrix.map(_.map(_.toInt)), clients, locations, p)
    
    println("\nChoose a P-Median solver:")
    println("1. Greedy")
    println("2. GRASP")
    
    val solverChoice = StdIn.readLine("Enter your choice (1-2): ").trim
    
    val solver = solverChoice match {
      case "1" => new PMedianGreedy(pMedianProblem)
      case "2" => 
        println("Enter GRASP list size (default is 5):")
        val graspListSize = StdIn.readLine().trim match {
          case "" => 5
          case n => n.toInt
        }
        new PMedianGRASP(pMedianProblem, graspListSize)
      case _ => 
        println("Invalid choice. Using Greedy.")
        new PMedianGreedy(pMedianProblem)
    }
    
    val solutions = solver.multiRun()
    
    println("\nResults of multi-run:")
    solutions.zipWithIndex.foreach { case (solution, index) =>
      println(s"Run ${index + 1}: ${solution.getTotalCost}")
    }
    
    val bestSolution = solutions.minBy(_.getTotalCost)
    println(s"\nBest solution found: ${bestSolution.getTotalCost}")
    println(s"Number of locations: ${bestSolution.getNumberOfLocations}")
    println(s"Number of clients: ${bestSolution.getNumberOfClients}")
    println(s"Average cost: ${bestSolution.getAverageCost}")
    println(s"Max cost: ${bestSolution.getMaxCost}")
    println(s"Min cost: ${bestSolution.getMinCost}")
  }

  def selectFile(directory: String): String = {
    val dir = new File(directory)
    val files = dir.listFiles().filter(_.isFile).map(_.getName)
    
    println(s"\nAvailable files in $directory:")
    files.zipWithIndex.foreach { case (file, index) =>
      println(s"${index + 1}. $file")
    }
    
    val choice = StdIn.readLine(s"Select a file (1-${files.length}): ").trim.toInt
    if (choice < 1 || choice > files.length) {
      println("Invalid choice. Using the first file.")
      s"$directory/${files.head}"
    } else {
      s"$directory/${files(choice - 1)}"
    }
  }
}
