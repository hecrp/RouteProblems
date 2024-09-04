package solutions

import problems.LocationProblem

/**
  * Created by hector on 1/18/17.
  */

// Clase que representa una solución para el problema de localización
case class LocationSolution(chosenLocations: Set[Int], costs: Vector[Int], value: Int) extends Solution {
  // Añade una nueva ubicación a la solución
  def addElement(location: Int, newValue: Int): LocationSolution =
    copy(chosenLocations = chosenLocations + location, value = newValue)

  // Elimina una ubicación de la solución
  def removeElement(location: Int): LocationSolution =
    copy(chosenLocations = chosenLocations - location)

  // Verifica si una ubicación está en la solución actual
  def isInSolution(location: Int): Boolean = chosenLocations.contains(location)

  // Obtiene las ubicaciones que no han sido elegidas
  def getNotChosenLocations(totalLocations: Int): Set[Int] =
    (0 until totalLocations).toSet.diff(chosenLocations)

  // Verifica si la solución está completa (tiene el número requerido de ubicaciones)
  def isComplete(p: Int): Boolean = chosenLocations.size == p

  // Verifica si la solución está vacía
  def isEmpty: Boolean = chosenLocations.isEmpty

  // Actualiza los costos de la solución basándose en las distancias del problema
  def updateCosts(problem: LocationProblem): LocationSolution = {
    // Calcula el costo mínimo para cada cliente
    val newCosts = (0 until problem.clients).toVector.map { client =>
      chosenLocations.map(location => problem.distanceMatrix(location)(client)).min
    }
    // Actualiza la solución con los nuevos costos y el valor total
    copy(costs = newCosts, value = newCosts.sum)
  }

  // Obtiene el costo total de la solución
  def getTotalCost: Int = value

  // Obtiene el número de ubicaciones elegidas
  def getNumberOfLocations: Int = chosenLocations.size

  // Obtiene el número de clientes
  def getNumberOfClients: Int = costs.length

  // Obtiene el costo promedio por cliente
  def getAverageCost: Double = if (costs.nonEmpty) costs.sum.toDouble / costs.length else 0.0

  // Obtiene el costo máximo
  def getMaxCost: Int = if (costs.nonEmpty) costs.max else 0

  // Obtiene el costo mínimo
  def getMinCost: Int = if (costs.nonEmpty) costs.min else 0

  // Implementación del método getMaxDistance heredado de Solution
  def getMaxDistance: Int = getMaxCost

  override def getRoute: Array[Int] = chosenLocations.toArray

  // Método para imprimir convenientemente la solución
  override def toString: String = {
    val locationsString = chosenLocations.toSeq.sorted.mkString(", ")

    s"""
    |P-Median Solution:
    |Chosen Locations: $locationsString
    |Total Cost: ${getTotalCost}
    |Number of Locations: ${getNumberOfLocations}
    |Number of Clients: ${getNumberOfClients}
    |Average Cost per Client: ${f"${getAverageCost}%.2f"}
    |Maximum Cost: ${getMaxCost}
    |Minimum Cost: ${getMinCost}
    """.stripMargin
  }
}
