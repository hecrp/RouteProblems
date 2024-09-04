package solutions

import problems.RoutingProblem

/**
  * Created by hector on 1/14/17.
  */

// Clase que representa una solución para el problema de enrutamiento
case class RoutingSolution(tour: Vector[(Int, Int)]) extends Solution {
  // Verifica si la solución es completa (ha visitado todos los nodos)
  def isComplete(graphSize: Int): Boolean = tour.length == graphSize
  
  // Verifica si la solución está vacía
  def isEmpty: Boolean = tour.isEmpty
  
  // Añade nuevos nodos al tour y calcula las distancias acumuladas
  def addElement(nodes: Vector[(Int, Int)], problem: RoutingProblem): RoutingSolution = {
    val newTour = tour ++ nodes.map { case (node, _) =>
      val newDistance = if (tour.isEmpty) {
        0 // Si el tour está vacío, la distancia es 0
      } else {
        val lastNode = tour.last._1
        tour.last._2 + problem.distanceMatrix(lastNode)(node)
      }
      (node, newDistance)
    }
    copy(tour = newTour)
  }
  
  // Busca si un nodo específico está en la solución
  def isInSolution(node: Int): Option[(Int, Int)] = tour.find(_._1 == node)
  
  // Obtiene el nodo en una posición específica del tour
  def getNodeIn(index: Int): Int = tour(index)._1
  
  // Obtiene el primer nodo del tour, si existe
  def getFirstNode: Option[Int] = tour.headOption.map(_._1)
  
  // Obtiene el último nodo del tour, o -1 si está vacío
  def getLastNode: Int = tour.lastOption.map(_._1).getOrElse(-1)
  
  // Obtiene la distancia total recorrida hasta el último nodo
  def getLastDistance: Int = tour.lastOption.map(_._2).getOrElse(0)
  
  // Obtiene el tamaño del tour (número de nodos visitados)
  def getSize: Int = tour.size

  // Obtiene el tour completo
  def getTour: Vector[Int] = tour.map(_._1)

  // Obtiene el costo total de la solución
  override def getTotalCost: Int = getLastDistance

  // Obtiene el costo promedio por nodo
  def getAverageCostPerNode: Double = if (getSize > 0) getTotalCost.toDouble / getSize else 0.0

  // Implementaciones de los métodos abstractos de Solution
  override def getAverageCost: Double = getAverageCostPerNode
  override def getMaxCost: Int = if (tour.nonEmpty) tour.map(_._2).max else 0
  override def getMaxDistance: Int = getMaxCost // En este caso, es equivalente al costo máximo
  override def getMinCost: Int = if (tour.nonEmpty) tour.map(_._2).min else 0
  override def getNumberOfClients: Int = getSize
  override def getNumberOfLocations: Int = getSize

  // Obtiene la distancia máxima entre dos nodos consecutivos
  def getMaxConsecutiveDistance(problem: RoutingProblem): Int = {
    tour match {
      case Vector() | Vector(_) => 0
      case _ =>
        (tour.sliding(2).map {
          case Vector((node1, _), (node2, _)) => problem.distanceMatrix(node1)(node2)
          case _ => 0 // This case should never happen due to sliding(2)
        } ++ Iterator(problem.distanceMatrix(getLastNode)(getFirstNode.getOrElse(0)))).max
    }
  }

  // Obtiene la distancia mínima entre dos nodos consecutivos
  def getMinConsecutiveDistance(problem: RoutingProblem): Int = {
    tour match {
      case Vector() | Vector(_) => 0
      case _ =>
        (tour.sliding(2).map {
          case Vector((node1, _), (node2, _)) => problem.distanceMatrix(node1)(node2)
          case _ => Int.MaxValue // This case should never happen due to sliding(2)
        } ++ Iterator(problem.distanceMatrix(getLastNode)(getFirstNode.getOrElse(0)))).min
    }
  }

  // Método para imprimir convenientemente la solución
  override def toString: String = {
    val tourString = tour.map(_._1).mkString(" -> ")
    val totalDistance = getTotalCost
    val numNodes = getSize
    
    s"""
    |Tour: $tourString
    |Total Distance: $totalDistance
    |Number of Nodes: $numNodes
    |Average Distance per Node: ${f"${getAverageCostPerNode}%.2f"}
    |Is Complete: ${if (numNodes > 0) isComplete(numNodes) else "N/A"}
    """.stripMargin
  }

  // Modify these methods
  def getCost: Int = getTotalCost

  override def getRoute: Array[Int] = tour.map(_._1).toArray

  // Add this method to get just the nodes
  def getNodes: Vector[Int] = tour.map(_._1)

  // If you need to compare RoutingSolutions, add this method
  def compare(that: RoutingSolution): Int = this.getCost.compare(that.getCost)
}
