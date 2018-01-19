package opsbd.OptPathsProb

import opsbd._
import opsbd.util.Logger

import scala.util.Random

case class GRASP(
    instance: Instance,
    alpha: Double,
    maxTime: Seconds,
    rnd: Random,
    logger: Logger[Seconds] = Logger(),
    bestLastDetector: Boolean = true
) {
  import VerticesCache._
  var bestSol: Sol = null
  var bestFitness = Double.MaxValue

  val fitnessEvaluator = FitnessEvaluator(instance)

  def eval(detectors: Seq[Idx]): Double = { return fitnessEvaluator.eval(detectors) }

  run()

  private def run() {
    val cache = instance.cache
    val map = instance.map

    val notNeutralizingProb = 1 - instance.neutralizingProb
    val nPaths = instance.pathsInfo.length

    case class RCLItem(detectorIdx: Idx, fitness: Double)
    val infiniteRCLItem = RCLItem(0, Double.MaxValue)

    // candidates to place detectors
    val allCandidates = cache.allCandidates()

    def oneIteration(): (Sol, Double) = {
      val candidates = allCandidates.clone()

      val RCL = new Array[RCLItem](candidates.size)

      val sol = new Array[Idx](instance.nDetectors)
      var solLen = 0
      var solFitness = 0.0
      while (solLen < instance.nDetectors) {
        // Populate RCL
        var RCLsize = 0
        for (detectorIdx <- candidates) {
          // evaluate tentative

          sol(solLen) = detectorIdx
          val solAux = sol.take(solLen + 1)

          val fitness = eval(solAux)

          RCL(RCLsize) = RCLItem(detectorIdx, fitness)
          RCLsize += 1
        }

        // Sort according to fitness
        for (i <- RCLsize until RCL.length) RCL(i) = infiniteRCLItem
        scala.util.Sorting.quickSort(RCL)(Ordering by (_.fitness))

        val minFitness = RCL(0).fitness
        val maxFitness = RCL(RCLsize - 1).fitness
        val admissibleFitness = minFitness + alpha * (maxFitness - minFitness)

        // is it worth to use binary search?
        var lastAdmissibleIdx = 0
        while (lastAdmissibleIdx < RCLsize && RCL(lastAdmissibleIdx).fitness <= admissibleFitness)
          lastAdmissibleIdx += 1

        val selectedIdx =
          if (!bestLastDetector || solLen < instance.nDetectors - 1) rnd.nextInt(lastAdmissibleIdx)
          else 0
        val selected = RCL(selectedIdx)
        val selectedDetectorIdx = selected.detectorIdx
        solFitness = selected.fitness

        // add best detector to sol and remove from candidates
        sol(solLen) = selectedDetectorIdx
        solLen += 1

        // remove best detector from candidates for next iteration
        candidates -= selectedDetectorIdx
      }
      (sol, solFitness)
    }

    var iter = 0
    while (logger.timer.elapsedTime() < maxTime) {
      val (sol, fitness0) = oneIteration()
      if (fitness0 < bestFitness) {
        bestFitness = fitness0
        bestSol = sol
        logger.register("GRASP:%8d %15.5f %10.2f", iter, bestFitness)
      }
      iter += 1
    }
  }
}

object TestGRASP extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(new Locale.Builder().setLanguage("en").setRegion("US").build())

  for (i <- 0 until 5) {

    val timer = util.Timer()
    val logger = Logger[Double](timer, echo = true)
    val rnd = new Random(0)

    val inst = SomeInstances.random32Instances(i)

    val alpha = 0.1
    val maxTime = 50
    opsbd.OptPathsProb.GRASP(inst, alpha, maxTime, rnd, logger)

    println
    logger.print()
  }
}
