package opsbd.OptPathsProb

import opsbd.EA._
import opsbd.VerticesCache.Idx
import opsbd.util.{Logger, Timer}
import opsbd.{Instance, Sol, SomeInstances}

import scala.util.Random

object EvolutionaryAlgorithms {
  type Fitness = Double
  type Gene = Idx

  var lastPath: Array[Int] = Array()

  case class OPSBDProblem(instance: Instance) extends Problem[Gene, Fitness] {
    val cache = instance.cache
    val map = instance.map
    val usePathProb = instance.usePathProb
    val notNeutralizingProb = 1 - instance.neutralizingProb
    val candidates = cache.allCandidates().toArray
    val numVars = instance.nDetectors

    override def isOptimal(ind: Individual[Gene, Fitness]): Boolean = false

    val fitnessEvaluator = FitnessEvaluator(instance)

    override def computeFitness(ind: Individual[Gene, Fitness]): Fitness = {
      // val (fitness1, path1) =fitnessEvaluator.eval2(ind.chromosome.toList)
      val (fitness, path) = fitnessEvaluator.evalGreedy(ind.chromosome.toList)

      lastPath = path
      return fitness
    }
  }

  class EA(seed: Int, instance: Instance, maxTime: Seconds) extends {
    val opsbdproblem = opsbd.OptPathsProb.EvolutionaryAlgorithms.OPSBDProblem(instance)
  } with StandardOperatorsSteadyStateNonRepeatedPopEA[Gene, Fitness](
    seed = seed,
    logger = Logger[Fitness](echo = false),
    problem = opsbdproblem,
    params = new StandardParams(opsbdproblem)
      .copy(maxRunTime = maxTime, isMaximization = false, popSize = 100)
  ) with TimedEA[Gene, Fitness] {

    val intersections = instance.cache
    val candidates = opsbdproblem.candidates

    private def randomPerm(xs: Sol, rnd: Random) {
      for (i <- 0 until instance.nDetectors) {
        val p = i + rnd.nextInt(xs.length - i)
        val temp = xs(i)
        xs(i) = xs(p)
        xs(p) = temp
      }
    }

    // should also assign fitness to new individual
    override def initialize(
        ind: Individual[Gene, Fitness],
        idx: Gene,
        eaState: EAState[Gene, Fitness]
    ): Unit = {
      val xs = candidates.clone()
      randomPerm(xs, eaState.rnd)
      for (i <- ind.chromosome.indices) ind.chromosome(i) = xs(i)
      ind.fitness = problem.computeFitness(ind)
    }

    override def mutate(ind: Individual[Gene, Fitness], eaState: EAState[Gene, Fitness]): Unit = {
      for (i <- ind.chromosome.indices) if (eaState.rnd.nextDouble() < eaState.params.mutProb) {
        var ok = false
        while (!ok) {
          val p = eaState.rnd.nextInt(candidates.length)
          val d = opsbdproblem.candidates(p)
          if (!ind.chromosome.contains(d)) {
            ind.chromosome(i) = d
            ok = true
          }
        }
      }
    }

    override def evaluate(
        ind: Individual[Gene, Fitness],
        eaState: EAState[Gene, Fitness]
    ): Fitness = {
      problem.computeFitness(ind)
      //opsbdproblem.computeFitnessLS(eaState, ind)
    }

    override def recombine(
        child: Individual[Gene, Fitness],
        parent1: Individual[Gene, Fitness],
        parent2: Individual[Gene, Fitness],
        eaState: EAState[Gene, Fitness]
    ) {
      val xs = parent1.chromosome.toArray
      val ys = parent2.chromosome.toArray
      var i = 0
      var j = 0
      for (p <- child.chromosome.indices) {
        var ok = false
        while (!ok) {
          val bool =
            if (i < xs.length && j < ys.length) eaState.rnd.nextBoolean() else (i < xs.length)
          if (bool) {
            val pos = i + eaState.rnd.nextInt(xs.length - i)
            val temp = xs(i)
            xs(i) = xs(pos)
            xs(pos) = temp

            child.chromosome(p) = xs(i)
            i += 1
          } else {
            val pos = j + eaState.rnd.nextInt(xs.length - j)
            val temp = ys(j)
            ys(j) = ys(pos)
            ys(pos) = temp

            child.chromosome(p) = ys(j)
            j += 1
          }

          ok = true
          var k = 0
          while (ok && k < p) {
            if (child.chromosome(k) == child.chromosome(p)) ok = false else k += 1
          }
        }
      }
    }

    var bestPath = Array[Int]()
    override def onUpdateBest(): Unit = { bestPath = lastPath }
  }
}

object EATest extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(new Locale.Builder().setLanguage("en").setRegion("US").build())

  val seed = 0
  val dim = 32
  val nObjectives = 3
  val nEntrances = 2
  val nDetectors = 5
  val detRad = 20
  val cellDim = 5

  val i = 20
  val instance = SomeInstances.random(dim, nObjectives, nEntrances, nDetectors, detRad, cellDim, i)
  //val instance = Instance.fromFile("rows_128__cols_128__objs_20__ents_8__blks_15__detcs_5__detcRad_20__cellDim_5__inst_0.txt")

  val rnd = new Random(seed)
  val alpha = 2

  val timer = Timer()
  //GRASP(instance, alpha, maxIters = 100, rnd, Logger[Double](), bestLastDetector = false)
  val maxTimeGRASP = timer.elapsedTime()


  import opsbd.OptPathsProb.EvolutionaryAlgorithms._

  val ea = new EA(seed, instance, maxTime = 25)
  val resultEA = ea.run()
  println("Final solution: " + resultEA.best)
  resultEA.logger.print()
}
