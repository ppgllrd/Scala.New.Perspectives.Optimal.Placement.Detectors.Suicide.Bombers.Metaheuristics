package opsbd.OptPathsProb

import opsbd.util.Logger
import opsbd.{Instance, Seconds, SomeInstances, VerticesCache, util}

case class Greedy(instance: Instance, logger: Logger[Seconds] = Logger()) {
  import VerticesCache.Idx

  val cache = instance.cache
  val map = instance.map
  val vertices = map.verticesCache

  val fitnessEvaluator = FitnessEvaluator(instance)

  def eval(detectors: Seq[Idx]): Double = { return fitnessEvaluator.eval(detectors) }

  def eval2(detectors: Seq[Idx]): (Double, Array[Int]) = {
    return fitnessEvaluator.evalGreedy(detectors)
    // return fitnessEvaluator.eval2(detectors)
  }

  // candidates to place detectors
  val candidates = cache.allCandidates()

  var usedPaths = Array[Int]()

  var sol = List[Idx]()
  var solLen = 0
  while (solLen < instance.nDetectors) {
    // search best placement
    var bestFitness = Double.MaxValue
    var bestVtx: Idx = -1
    for (v <- candidates) {
      // print("+")
      val tentative = v :: sol
      val (fitness, bestPaths) = eval2(tentative)
      if (fitness < bestFitness) {
        bestFitness = fitness
        bestVtx = v
        usedPaths = bestPaths
      }
    }

    // add it to sol and remove from candidates
    sol = bestVtx :: sol
    solLen += 1
    candidates -= bestVtx
  }

  sol = sol.reverse
  //println(sol, eval(sol))

  logger.register("GREEDY:%8d %15.5f %10.2f", 0, eval(sol))
}

object TestGreedy extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(new Locale.Builder().setLanguage("en").setRegion("US").build())

  for (i <- 0 until 5) {

    val timer = util.Timer()
    val logger = Logger[Double](timer, echo = true)

    val inst = SomeInstances.random32Instances(i)

    opsbd.OptPathsProb.Greedy(inst)

    println
    logger.print()
  }
}
