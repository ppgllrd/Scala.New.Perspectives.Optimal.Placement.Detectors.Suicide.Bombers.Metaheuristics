package opsbd.OptPathsProb

import opsbd.VerticesCache._
import opsbd.util.Logger
import opsbd.{Instance, Seconds, Sol}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object TabuSearch {
  def ts1(
      instance: Instance,
      sol: Sol,
      logger: Logger[Seconds],
      rnd: Random,
      bestKnownFitness: Seconds,
      iter: Idx = 0,
      log: Boolean = true
  ): Double = {
    val cache = instance.cache
    val map = instance.map
    val infinite = Double.MaxValue

    val notNeutralizingProb = 1 - instance.neutralizingProb
    val nDetectors = instance.nDetectors
    val nPaths = instance.pathsInfo.length

    val usePathProb =
      instance
        .usePathProb // should be defined inside evaluation loop were it different for each path

    // candidates to place detectors
    val candidates = cache.allCandidates()

    val N = candidates.size
    val maxIter = N / 2 + rnd.nextInt(N / 4)
    val minTabu = 0 * maxIter / 10
    val maxTabu = 0 * maxIter / 50

    // println(maxIter,minTabu,maxTabu)

    val tabuList = Array.ofDim[Int](map.rows * map.columns)

    val currentSol = sol.clone()

    // Info for fitness that can be reused from current solution
    val totalIntersections = Array.fill[Double](nPaths)(0)
    for (detectorIdx <- currentSol) for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) +=
      cache.intersection(detectorIdx)(pathIdx)

    var initialFitness = 0.0
    for (pathIdx <- 0 until nPaths) {
      val expectedCasualties = map.expectedCasualties(instance.pathsInfo(pathIdx).objectiveOrder)
      initialFitness +=
        expectedCasualties * usePathProb *
          (notNeutralizingProb +
            instance.neutralizingProb /
            math.exp(instance.instantaneousDetectionRate * totalIntersections(pathIdx)))
    }

    var bestFitness = initialFitness

    for (iter <- 0 until maxIter) {

      for (i <- 0 until nDetectors) {
        var bestMoveFitness = bestFitness
        val bestMoves = ArrayBuffer[(Int, Idx)]()

        // remove detector from cache
        val detectorIdx = currentSol(i)
        for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) -=
          cache.intersection(detectorIdx)(pathIdx)

        // replace it by another one
        for (tentativeDetectorIdx <- candidates) {
          // evaluate tentative sol
          var tentativeFitness = 0.0
          for (pathIdx <- 0 until nPaths) {
            val totalIntersection = totalIntersections(pathIdx) +
              cache.intersection(tentativeDetectorIdx)(pathIdx)
            val expectedCasualties = map
              .expectedCasualties(instance.pathsInfo(pathIdx).objectiveOrder)
            tentativeFitness +=
              expectedCasualties * usePathProb *
                (notNeutralizingProb +
                  instance.neutralizingProb /
                  math.exp(instance.instantaneousDetectionRate * totalIntersection))
          }

          if (tentativeFitness < bestMoveFitness) {
            bestMoveFitness = tentativeFitness
            bestMoves.clear()
            bestMoves.append((i, tentativeDetectorIdx))
            if (log && bestMoveFitness < bestKnownFitness) logger
              .register("TT:   %8d %15.5f %10.2f", iter, bestMoveFitness)
          }
        }

        // restore cache
        for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) +=
          cache.intersection(detectorIdx)(pathIdx)
        // All movements may be in tabu state
        if (bestMoves.nonEmpty) {
          // select move
          val p = 0 //rnd.nextInt(bestMoves.length)
          val (_, newDetectorIdx) = bestMoves(p)

          val oldDetectorIdx = detectorIdx

          // do move
          currentSol(i) = newDetectorIdx

          // make it tabu
          tabuList(newDetectorIdx) = iter + minTabu + (if (maxTabu > 0) rnd.nextInt(maxTabu) else 0)

          // recompute cache
          for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) +=
            (cache.intersection(newDetectorIdx)(pathIdx) -
              cache.intersection(oldDetectorIdx)(pathIdx))

          // Update best solution found, if needed
          if (bestMoveFitness < bestFitness) {
            bestFitness = bestMoveFitness
            for (i <- 0 until sol.length) sol(i) = currentSol(i)

            if (log && bestFitness < bestKnownFitness) logger
              .register("TS:   %8d %15.5f %10.2f", iter, bestFitness)
          } else {
            //printf("%.5f ",bestMoveFitness)
          }
        }
      }
    }
    return bestFitness
  }

  def ts2(
      instance: Instance,
      sol: Sol,
      logger: Logger[Seconds],
      rnd: Random,
      bestKnownFitness: Seconds,
      iter: Idx = 0,
      log: Boolean = true
  ): Double = {
    val cache = instance.cache
    val map = instance.map

    val notNeutralizingProb = 1 - instance.neutralizingProb
    val nDetectors = instance.nDetectors
    val nPaths = instance.pathsInfo.length

    val usePathProb =
      instance
        .usePathProb // should be defined inside evaluation loop were it different for each path

    // candidates to place detectors
    val candidates = cache.allCandidates()

    val currentSol = sol.clone()

    // Info for fitness that can be reused from current solution
    val totalIntersections = Array.fill[Double](nPaths)(0)
    for (detectorIdx <- currentSol) for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) +=
      cache.intersection(detectorIdx)(pathIdx)

    var currentFitness = 0.0
    for (pathIdx <- 0 until nPaths) {
      val expectedCasualties = map.expectedCasualties(instance.pathsInfo(pathIdx).objectiveOrder)
      currentFitness +=
        expectedCasualties * usePathProb *
          (notNeutralizingProb +
            instance.neutralizingProb /
            math.exp(instance.instantaneousDetectionRate * totalIntersections(pathIdx)))
    }

    val N = candidates.size
    val maxIter = 2 * nDetectors //N / 2 + rnd.nextInt(N/4)
    val minTabu = maxIter / 1
    val maxTabu = maxIter / 2

    val tabuList = Array.ofDim[Int](nDetectors, map.rows * map.columns)

    var bestFitness = currentFitness
    var noImprovement = 0
    while (noImprovement < maxIter) {
      for (i <- 0 until nDetectors) {
        // remove detector
        val detectorIdx = currentSol(i)
        for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) -=
          cache.intersection(detectorIdx)(pathIdx)

        // replace it by another one
        val bestMoves = ArrayBuffer[(Int, Idx)]()
        var bestMoveFitness = Double.MaxValue

        for (tentativeDetectorIdx <- candidates) {
          // evaluate tentative sol
          var tentativeFitness = 0.0
          for (pathIdx <- 0 until nPaths) {
            val totalIntersection = totalIntersections(pathIdx) +
              cache.intersection(tentativeDetectorIdx)(pathIdx)
            val expectedCasualties = map
              .expectedCasualties(instance.pathsInfo(pathIdx).objectiveOrder)
            tentativeFitness +=
              expectedCasualties * usePathProb *
                (notNeutralizingProb +
                  instance.neutralizingProb /
                  math.exp(instance.instantaneousDetectionRate * totalIntersection))
          }

          if ((tabuList(i)(tentativeDetectorIdx) < iter) || (tentativeFitness < bestFitness)) {
            if ((tentativeFitness - bestMoveFitness).abs < 0.00001) {
              bestMoves.append((i, tentativeDetectorIdx))
            } else if (tentativeFitness < bestMoveFitness) {
              bestMoves.clear()
              bestMoves.append((i, tentativeDetectorIdx))
              bestMoveFitness = tentativeFitness
            }
          }
        }

        for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) +=
          cache.intersection(detectorIdx)(pathIdx)

        noImprovement += 1
        if (bestMoves.nonEmpty) {
          val dIdx = bestMoves(rnd.nextInt(bestMoves.length))._2
          currentSol(i) = dIdx
          currentFitness = bestMoveFitness
          // make it tabu
          tabuList(i)(dIdx) = iter + minTabu + (if (maxTabu > 0) rnd.nextInt(maxTabu) else 0)

          // Do improvement move or keep original detector
          for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) +=
            (cache.intersection(dIdx)(pathIdx) - cache.intersection(detectorIdx)(pathIdx))

          if (currentFitness < bestFitness) {
            bestFitness = currentFitness
            for (i <- 0 until nDetectors) sol(i) = currentSol(i)
            if (log && currentFitness < bestKnownFitness) logger
              .register("TS:   %8d %15.5f %10.2f", iter, bestMoveFitness)
            noImprovement = 0
          }
        }
      }
    }
    return bestFitness
  }

  def ts3(
      instance: Instance,
      sol: Sol,
      logger: Logger[Seconds],
      rnd: Random,
      bestKnownFitness: Seconds,
      iter: Idx = 0,
      log: Boolean = true
  ): Double = {
    val cache = instance.cache
    val map = instance.map

    val notNeutralizingProb = 1 - instance.neutralizingProb
    val nDetectors = instance.nDetectors
    val nPaths = instance.pathsInfo.length

    val usePathProb =
      instance
        .usePathProb // should be defined inside evaluation loop were it different for each path

    // candidates to place detectors
    val candidates = cache.allCandidates()

    val currentSol = sol.clone()

    // Info for fitness that can be reused from current solution
    val totalIntersections = Array.fill[Double](nPaths)(0)
    for (detectorIdx <- currentSol) for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) +=
      cache.intersection(detectorIdx)(pathIdx)

    var currentFitness = 0.0
    for (pathIdx <- 0 until nPaths) {
      val expectedCasualties = map.expectedCasualties(instance.pathsInfo(pathIdx).objectiveOrder)
      currentFitness +=
        expectedCasualties * usePathProb *
          (notNeutralizingProb +
            instance.neutralizingProb /
            math.exp(instance.instantaneousDetectionRate * totalIntersections(pathIdx)))
    }

    val N = candidates.size
    val maxIter = nDetectors //N / 2 + rnd.nextInt(N/4)
    val minTabu = maxIter / 2
    val maxTabu = maxIter / 2

    val tabuList = Array.ofDim[Int](nDetectors, map.rows * map.columns)

    var bestFitness = currentFitness
    var noImprovement = 0
    while (noImprovement < maxIter) {
      // replace it by another one
      val bestMoves = ArrayBuffer[(Int, Idx)]()
      var bestMoveFitness = Double.MaxValue

      for (i <- 0 until nDetectors) {
        // remove detector
        val detectorIdx = currentSol(i)
        for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) -=
          cache.intersection(detectorIdx)(pathIdx)

        for (tentativeDetectorIdx <- candidates) {
          // evaluate tentative sol
          var tentativeFitness = 0.0
          for (pathIdx <- 0 until nPaths) {
            val totalIntersection = totalIntersections(pathIdx) +
              cache.intersection(tentativeDetectorIdx)(pathIdx)
            val expectedCasualties = map
              .expectedCasualties(instance.pathsInfo(pathIdx).objectiveOrder)
            tentativeFitness +=
              expectedCasualties * usePathProb *
                (notNeutralizingProb +
                  instance.neutralizingProb /
                  math.exp(instance.instantaneousDetectionRate * totalIntersection))
          }

          if ((tabuList(0)(tentativeDetectorIdx) < iter) || (tentativeFitness < bestFitness)) {
            if ((tentativeFitness - bestMoveFitness).abs < 0.00001) {
              bestMoves.append((i, tentativeDetectorIdx))
            } else if (tentativeFitness < bestMoveFitness) {
              bestMoves.clear()
              bestMoves.append((i, tentativeDetectorIdx))
              bestMoveFitness = tentativeFitness
            }
          }
        }
        for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) +=
          cache.intersection(detectorIdx)(pathIdx)
      }

      noImprovement += 1
      if (bestMoves.nonEmpty) {
        val (i, dIdx) = bestMoves(rnd.nextInt(bestMoves.length))
        val oldDetectorIdx = currentSol(i)
        currentSol(i) = dIdx
        currentFitness = bestMoveFitness
        // make it tabu
        tabuList(0)(dIdx) = iter + minTabu + (if (maxTabu > 0) rnd.nextInt(maxTabu) else 0)

        // Do improvement move or keep original detector
        for (pathIdx <- 0 until nPaths) totalIntersections(pathIdx) +=
          (cache.intersection(dIdx)(pathIdx) - cache.intersection(oldDetectorIdx)(pathIdx))

        if (currentFitness < bestFitness) {
          bestFitness = currentFitness
          for (i <- 0 until nDetectors) sol(i) = currentSol(i)
          if (log && currentFitness < bestKnownFitness) logger
            .register(" %8d %15.5f %10.2f ", iter, bestMoveFitness)
          noImprovement = 0
        }
      }
    }
    return bestFitness
  }

  def ts4(
      instance: Instance,
      sol: Sol,
      logger: Logger[Seconds],
      maxTime: Seconds,
      rnd: Random,
      bestKnownFitness: Seconds,
      iter: Idx = 0,
      log: Boolean = true
  ): Double = {
    val cache = instance.cache
    val map = instance.map

    val nDetectors = instance.nDetectors

    // candidates to place detectors
    val candidates = cache.allCandidates()

    val fitnessEvaluator = FitnessEvaluator(instance)

    def eval(detectors: Seq[Idx]): Double = { return fitnessEvaluator.evalGreedy(detectors)._1 }

    val currentSol = sol.clone()
    var currentFitness = eval(sol)

    if (log && currentFitness < bestKnownFitness) logger
      .register(" %8d %15.5f %10.2f ", iter, currentFitness)

    val N = candidates.size
    val maxIter = 2 * nDetectors //N / 2 + rnd.nextInt(N/4)
    val minTabu = maxIter / 2
    val maxTabu = maxIter / 2

    val tabuList = Array.ofDim[Int](nDetectors, map.rows * map.columns)

    var bestFitness = currentFitness
    var noImprovement = 0
    while (noImprovement < maxIter && logger.timer.elapsedTime() < maxTime) {
      // replace it by another one
      val bestMoves = ArrayBuffer[(Int, Idx)]()
      var bestMoveFitness = Double.MaxValue

      for (i <- 0 until nDetectors) {

        for (tentativeDetectorIdx <- candidates) {
          // evaluate tentative sol
          val detectorIdx = currentSol(i)
          currentSol(i) = tentativeDetectorIdx
          val tentativeFitness = eval((currentSol))
          currentSol(i) = detectorIdx

          if ((tabuList(i)(tentativeDetectorIdx) < iter) || (tentativeFitness < bestFitness)) {

            if ((tentativeFitness - bestMoveFitness).abs < 0.00001) {
              bestMoves.append((i, tentativeDetectorIdx))
            } else if (tentativeFitness < bestMoveFitness) {
              bestMoves.clear()
              bestMoves.append((i, tentativeDetectorIdx))
              bestMoveFitness = tentativeFitness
            }
          }
        }
      }

      noImprovement += 1
      if (bestMoves.nonEmpty) {
        val (i, dIdx) = bestMoves(rnd.nextInt(bestMoves.length))
        currentSol(i) = dIdx
        currentFitness = bestMoveFitness
        // make it tabu
        tabuList(i)(dIdx) = iter + minTabu + (if (maxTabu > 0) rnd.nextInt(maxTabu) else 0)

        if (currentFitness < bestFitness) {
          bestFitness = currentFitness
          for (i <- 0 until nDetectors) sol(i) = currentSol(i)
          if (log && currentFitness < bestKnownFitness) logger
            .register(" %8d %15.5f %10.2f ", iter, bestMoveFitness)
          noImprovement = 0
        }
      }
    }
    return bestFitness
  }
}

case class RepeatedTabuSearch(
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

  def run(maxTime: Seconds) {
    var bestSol: Sol = null
    var bestFitness = Double.MaxValue

    var iter = 0
    while (logger.timer.elapsedTime() < maxTime) {
      val tentative = randomPerm().take(instance.nDetectors)

      val fitness = opsbd.OptPathsProb.TabuSearch
        .ts4(instance, tentative, logger, maxTime, rnd, bestFitness, iter, log = true)
      if (fitness < bestFitness) {
        bestFitness = fitness
        bestSol = tentative
        //logger.register("RLS:  %8d %15.5f %10.2f", iter, bestFitness)
      }
      iter += 1
    }
  }
}
