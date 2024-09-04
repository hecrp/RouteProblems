package problems

/**
 * Representa el problema de localización de instalaciones.
 *
 * @param distanceMatrix Matriz de distancias entre ubicaciones y clientes.
 * @param locations Número total de posibles ubicaciones para las instalaciones.
 * @param clients Número total de clientes a ser atendidos.
 * @param p Número de instalaciones a ser ubicadas.
 *
 * Este problema busca determinar las mejores ubicaciones para un número fijo de instalaciones
 * con el objetivo de minimizar la distancia total entre los clientes y las instalaciones más cercanas.
 */
case class LocationProblem(
    distanceMatrix: Array[Array[Int]], 
    locations: Int, 
    clients: Int, 
    p: Int
) extends Problem
