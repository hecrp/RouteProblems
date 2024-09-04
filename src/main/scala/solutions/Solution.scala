package solutions

/**
  * Created by hector on 1/15/17.
  */
trait Solution {
  // The approach I took here is incorrect in OOP terms. Not all Solution objects will have the same methods (TSP or Pmedian).
  // This is a case of using inheritance to implement polymorphism and ensure compatibility.
  def getTotalCost: Int
  def getNumberOfLocations: Int
  def getNumberOfClients: Int
  def getAverageCost: Double
  def getMaxCost: Int
  def getMinCost: Int
  def getMaxDistance: Int
  def getRoute: Array[Int]
}
