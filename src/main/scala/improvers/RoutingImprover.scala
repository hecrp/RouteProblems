package improvers

import solutions.RoutingSolution

/**
  * Created by hector on 1/18/17.
  */
abstract class RoutingImprover {
  def improve(initialSolution: RoutingSolution): RoutingSolution
}
