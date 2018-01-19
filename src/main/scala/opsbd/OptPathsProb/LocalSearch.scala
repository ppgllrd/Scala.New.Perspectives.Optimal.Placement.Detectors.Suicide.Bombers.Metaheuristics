package opsbd.OptPathsProb

import opsbd.EA.Seconds
import opsbd.VerticesCache.Idx
import opsbd.util.Logger
import opsbd.{Instance, Sol}

import scala.util.Random

object LocalSearch {

  def HillClimbing(
      instance: Instance,
      sol: Sol,
      logger: Logger[Seconds],
      bestKnownFitness: Double,
      iter: Idx = 0,
      maxTime: Seconds,
      log: Boolean = true
  ): Double = {
    val cache = instance.cache
    val map = instance.map

    val nDetectors = instance.nDetectors
    val nPaths = instance.pathsInfo.length

    // candidates to place detectors
    val candidates = cache.allCandidates()

    val fitnessEvaluator = FitnessEvaluator(instance)

    def eval(detectors: Seq[Idx]): Double = { return fitnessEvaluator.evalGreedy(detectors)._1 }

    var fitness = eval(sol)

    if (log && fitness < bestKnownFitness) logger.register(" %8d %15.5f %10.2f ", iter, fitness)

    var improvement = true
    while (improvement && (logger.timer.elapsedTime() < maxTime)) {
      improvement = false

      for (i <- 0 until nDetectors) {
        // remove detector

        for (tentativeDetectorIdx <- (candidates -- sol)) {
          // evaluate tentative sol
          val detectorIdx = sol(i)
          sol(i) = tentativeDetectorIdx

          var tentativeFitness = eval(sol)

          if (tentativeFitness < fitness) {
            // accept this move

            fitness = tentativeFitness
            if (log && fitness < bestKnownFitness) {
              //val solStr = sol.sorted.mkString("[",",","] ")
              logger.register("HC:   %8d %15.5f %10.2f ", iter, fitness)
            }

            improvement = true
          } else sol(i) = detectorIdx
        }
      }
    }
    return fitness
  }
}

case class RepeatedLocalSearch(
    instance: Instance,
    rnd: Random,
    logger: Logger[Double] = Logger[Double]()
) {

  private def randomPerm(): Sol = {
    val candidates = instance.cache.allCandidates()
    val xs = candidates.toArray

    for (i <- 0 until instance.nDetectors) {
      val p = i + rnd.nextInt(xs.length - i)
      val temp = xs(i)
      xs(i) = xs(p)
      xs(p) = temp
    }
    xs
  }

  def run(maxTime: Seconds): Sol = {
    var bestSol: Sol = null
    var bestFitness = Double.MaxValue

    var iter = 0
    while (logger.timer.elapsedTime() < maxTime) {
      val tentative = randomPerm().take(instance.nDetectors)

      val fitness = opsbd.OptPathsProb.LocalSearch
        .HillClimbing(instance, tentative, logger, bestFitness, iter, maxTime, log = true)
      if (fitness < bestFitness) {
        bestFitness = fitness
        bestSol = tentative
        //logger.register("RLS:  %8d %15.5f %10.2f", iter, bestFitness)
      }
      iter += 1
    }

    return bestSol
  }
}
