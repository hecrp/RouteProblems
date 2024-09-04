package problems

/**
  * Trait base para representar problemas de optimización.
  * Created by hector on 1/18/17.
  */
trait Problem

// Eliminamos la definición de RoutingProblem de aquí (i will leave this ancient comment.)

/**
 * Representa un problema de enrutamiento.
 *
 * @param distanceMatrix Matriz de distancias entre nodos del grafo.
 * @param graphSize Tamaño del grafo (número de nodos).
 *
 * Este problema busca encontrar la ruta más corta que visite todos los nodos
 * del grafo exactamente una vez y regrese al punto de inicio (problema del viajante).
 */
case class RoutingProblem(
    distanceMatrix: Array[Array[Int]], 
    graphSize: Int
) extends Problem
