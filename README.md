# Heuristics for the TSP and P-Median Problem

This repository contains solvers for two classic optimization problems. This project was developed as a part of the "Intelligent Systems" course during my master's degree and I just wanted to build something in [Scala](https://www.scala-lang.org/) at the time. It took me time to complete the project for the course, so I'm coming back to it in 2024 to give the love it deserved. The code is now refactored and improved to at least use more of the functional programming features of Scala than it did when I first wrote it. Comments are in Spanish and describe the algorithms and the code in a very basic way.

## Problem Descriptions

### Traveling Salesman Problem (TSP)
The [Traveling Salesman Problem (TSP)](https://en.wikipedia.org/wiki/Travelling_salesman_problem) involves finding the shortest possible route that visits each city exactly once and returns to the starting city. It has applications in logistics, planning, and microchip manufacturing. 

### P-Median Problem
The [P-Median problem](https://en.wikipedia.org/wiki/Facility_location_problem#P-median) involves selecting P facilities from a set of potential locations to minimize the total weighted distance between demand points and their nearest facilities. It has applications in various fields, including logistics, urban planning, and network design.
## Implemented Solvers and Improvers

This project implements various solvers and improvers for both the TSP and P-Median problems. Here's an overview of the implemented algorithms:

### TSP Solvers
- [Nearest Neighbor](https://en.wikipedia.org/wiki/Nearest_neighbour_algorithm): A greedy algorithm that constructs a tour by always choosing the nearest unvisited city.
- [Nearest Neighbor with GRASP](https://en.wikipedia.org/wiki/Greedy_randomized_adaptive_search_procedure): A metaheuristic that combines the NN search procedure with local search.

### TSP Improvers
- [2-Opt](https://en.wikipedia.org/wiki/2-opt): A local search algorithm that improves a tour by swapping two edges. It iteratively applies the 2-opt swap operation to improve the tour until no further improvement is possible or a maximum number of iterations is reached.

### P-Median Solvers
- [Greedy Algorithm](https://en.wikipedia.org/wiki/Greedy_algorithm): Iteratively selects facilities based on the greatest reduction in total weighted distance.
- [GRASP](https://en.wikipedia.org/wiki/Greedy_randomized_adaptive_search_procedure): A metaheuristic that combines greedy randomized adaptive search procedure with local search.

### P-Median Improvers
- [Local Search](https://en.wikipedia.org/wiki/Local_search_(optimization)): Attempts to improve the solution by making small changes to the current set of facilities. It iteratively tries to replace each chosen location with a non-chosen one, updating the solution if an improvement is found. The process continues until no improvement is made for a specified number of iterations.
- [Simulated Annealing](https://en.wikipedia.org/wiki/Simulated_annealing): A metaheuristic that simulates the annealing process in metallurgy to avoid getting trapped in local minima. It uses a temperature parameter to control the acceptance of worse solutions during the search process.

## Getting Started

### Prerequisites

- [Java JDK 8 or higher](https://www.oracle.com/java/technologies/javase-jdk11-downloads.html)
- [SBT (Scala Build Tool)](https://www.scala-sbt.org/download.html)

### Installation

1. Clone the repository:
   ```
   git clone https://github.com/hecrp/RouteProblems.git
   ```
2. Navigate to the project directory:
   ```
   cd RouteProblems
   ```
3. Build the project using SBT:
   ```
   sbt compile
   ```

### Usage

To run the project, you should use the following command:
```
sbt run
```
