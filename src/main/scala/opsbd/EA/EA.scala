/** ****************************************************************************
  *
  * Evolutionary Algorithms
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

import opsbd.util.Logger

import java.util.Random

case class EAParams(
    popSize: Int = 100,
    crossProb: Probability = 0.9,
    mutProb: Probability,
    maxRunTime: Seconds = Double.MaxValue,
    maxIters: Int = Int.MaxValue,
    isMaximization: Boolean = true
) {
  override def toString = {
    val fmt =
      "EAParams(popSize=%d, crossProb=%.3f, mutProb=1/%.2f, maxRunTime=%.2f segs., maxIters=%d)"
    fmt.format(popSize, crossProb, 1.0 / mutProb, maxRunTime, maxIters)
  }
}

// Current state of EA
case class EAState[Gene, Fitness](
    population: Population[Gene, Fitness],
    best: Individual[Gene, Fitness],
    var iter: Int,
    logger: Logger[Fitness],
    rnd: Random,
    params: EAParams,
    problem: Problem[Gene, Fitness]
)

// Information returned after running EA
case class EAResult[Gene, Fitness](
    best: Individual[Gene, Fitness] // best solution found
    ,
    population: Population[Gene, Fitness] // state of population at end of execution
    ,
    lastIter: Int // last iteration that was done
    ,
    bestIter: Int // iteration where best sol was found
    ,
    logger: Logger[Fitness] // log of EA execution
) {
  override def toString = "EAResult(best=%s, lastIter=%d, bestIter=%d)"
    .format(best, lastIter, bestIter)
}

abstract class EA[Gene, Fitness](
    seed: Int,
    val params: EAParams,
    val problem: Problem[Gene, Fitness]
)(implicit ord: Ordering[Fitness]) {

  import ord._

  protected val population: Population[Gene, Fitness]

  // should also assign fitness to new individual
  def initialize(ind: Individual[Gene, Fitness], idx: Int, eaState: EAState[Gene, Fitness])

  def evaluate(ind: Individual[Gene, Fitness], eaState: EAState[Gene, Fitness]): Fitness = {
    problem.computeFitness(ind)
  }

  def mutate(ind: Individual[Gene, Fitness], eaState: EAState[Gene, Fitness])

  def select(eaState: EAState[Gene, Fitness])(implicit
      ord: Ordering[Fitness]
  ): Individual[Gene, Fitness]

  def recombine(
      child: Individual[Gene, Fitness],
      parent1: Individual[Gene, Fitness],
      parent2: Individual[Gene, Fitness],
      eaState: EAState[Gene, Fitness]
  )

  def replace(ind: Individual[Gene, Fitness], eaState: EAState[Gene, Fitness])

  def endCondition(eaState: EAState[Gene, Fitness]): Boolean

  def onUpdateBest()

  def run(): EAResult[Gene, Fitness]

  val isBetter =
    if (params.isMaximization) (ind1: Individual[Gene, Fitness], ind2: Individual[Gene, Fitness]) =>
      ind1.fitness > ind2.fitness
    else (ind1: Individual[Gene, Fitness], ind2: Individual[Gene, Fitness]) =>
      ind1.fitness < ind2.fitness
}

abstract class SteadyStateEA[Gene: Manifest, Fitness](
    seed: Int,
    logger: Logger[Fitness],
    params: EAParams,
    problem: Problem[Gene, Fitness]
)(implicit ord: Ordering[Fitness])
    extends EA[Gene, Fitness](seed, params, problem) {

  override def run(): EAResult[Gene, Fitness] = {

    val state = EAState[Gene, Fitness](
      population = population,
      best = new ArrayIndividual[Gene, Fitness](problem.numVars),
      iter = 0,
      logger = logger,
      rnd = new Random(seed),
      params = params,
      problem = problem
    )

    //println("Seed=%d".format(seed))
    // println(params)

    // initialize population
    population.initialize(state)

    val ind = new ArrayIndividual[Gene, Fitness](problem.numVars)

    state.best.copyFrom(state.population.best())

    evaluate(state.best, state)
    onUpdateBest()

    state.logger.register(state.iter, state.best.fitness)

    var bestIter = state.iter

    while (!endCondition(state)) {
      state.iter += 1

      if (state.rnd.nextDouble() < params.crossProb) {
        val parent1 = select(state)
        val parent2 = select(state)

        recombine(ind, parent1, parent2, state)
      } else ind.copyFrom(Selection.random(state.population, state.rnd))

      mutate(ind, state)

      ind.fitness = evaluate(ind, state)

//      if(ind.fitness > state.best.fitness) {
      if (isBetter(ind, state.best)) {
        onUpdateBest()
        state.best.copyFrom(ind)
        state.logger.register(state.iter, state.best.fitness)
        bestIter = state.iter
      }

      replace(ind, state)
    }
    EAResult(
      best = state.best,
      population = state.population,
      lastIter = state.iter,
      bestIter = bestIter,
      logger = state.logger
    )
  }
}

trait StandardOperators[Gene, Fitness] {
  def select(eaState: EAState[Gene, Fitness])(implicit ord: Ordering[Fitness]) = Selection
    .binaryTournament(eaState.population, eaState.rnd)

  def recombine(
      child: Individual[Gene, Fitness],
      parent1: Individual[Gene, Fitness],
      parent2: Individual[Gene, Fitness],
      eaState: EAState[Gene, Fitness]
  ) = Recombination.uniform(child.chromosome, parent1.chromosome, parent2.chromosome, eaState.rnd)

  def replace(ind: Individual[Gene, Fitness], eaState: EAState[Gene, Fitness]) {
    Replacement.worst(eaState.population, ind)
  }

  def onUpdateBest(): Unit = {}
}

abstract class StandardOperatorsSteadyStateEA[Gene: Manifest, Fitness](
    seed: Int,
    logger: Logger[Fitness],
    params: EAParams,
    problem: Problem[Gene, Fitness]
)(implicit ord: Ordering[Fitness])
    extends SteadyStateEA[Gene, Fitness](seed, logger, params, problem)
    with StandardOperators[Gene, Fitness] {
  override val population = StandardPopulation[Gene, Fitness](params.popSize, this)
}

abstract class StandardOperatorsSteadyStateNonRepeatedPopEA[Gene: Manifest, Fitness](
    seed: Int,
    logger: Logger[Fitness],
    params: EAParams,
    problem: Problem[Gene, Fitness]
)(implicit ord: Ordering[Fitness])
    extends SteadyStateEA[Gene, Fitness](seed, logger, params, problem)
    with StandardOperators[Gene, Fitness] {
  override val population = NonRepeatedPopulation(params.popSize, this)
}

class StandardParams[Gene, Fitness](problem: Problem[Gene, Fitness])
    extends EAParams(popSize = 100, crossProb = 0.9, mutProb = 1.0 / problem.numVars)

trait TimedEA[Gene, Fitness] {
  val problem: Problem[Gene, Fitness]

  def endCondition(eaState: EAState[Gene, Fitness]) =
    eaState.logger.timer.elapsedTime() > eaState.params.maxRunTime ||
      problem.isOptimal(eaState.best)
}

abstract class StandardSteadyStateTimedEA[Gene: Manifest, Fitness](
    seed: Int,
    logger: Logger[Fitness],
    override val problem: Problem[Gene, Fitness],
    maxRunTime: Seconds
)(implicit ord: Ordering[Fitness])
    extends StandardOperatorsSteadyStateEA[Gene, Fitness](
      seed,
      logger,
      new StandardParams(problem).copy(maxRunTime = maxRunTime),
      problem
    )
    with TimedEA[Gene, Fitness]

abstract class StandardSteadyStateNonRepeatedPopTimedEA[Gene: Manifest, Fitness](
    seed: Int,
    logger: Logger[Fitness],
    override val problem: Problem[Gene, Fitness],
    maxRunTime: Seconds
)(implicit ord: Ordering[Fitness])
    extends StandardOperatorsSteadyStateNonRepeatedPopEA[Gene, Fitness](
      seed,
      logger,
      new StandardParams(problem).copy(maxRunTime = maxRunTime),
      problem
    )
    with TimedEA[Gene, Fitness]

trait IteratedEA[Gene, Fitness] {
  val problem: Problem[Gene, Fitness]

  def endCondition(eaState: EAState[Gene, Fitness]) = eaState.iter >= eaState.params.maxIters ||
    problem.isOptimal(eaState.best)
}

abstract class StandardSteadyStateIteratedEA[Gene: Manifest, Fitness](
    seed: Int,
    logger: Logger[Fitness],
    override val problem: Problem[Gene, Fitness],
    maxIters: Int
)(implicit ord: Ordering[Fitness])
    extends StandardOperatorsSteadyStateEA[Gene, Fitness](
      seed,
      logger,
      new StandardParams(problem).copy(maxIters = maxIters),
      problem
    )
    with IteratedEA[Gene, Fitness]

abstract class StandardSteadyStateNonRepeatedPopIteratedEA[Gene: Manifest, Fitness](
    seed: Int,
    logger: Logger[Fitness],
    override val problem: Problem[Gene, Fitness],
    maxIters: Int
)(implicit ord: Ordering[Fitness])
    extends StandardOperatorsSteadyStateNonRepeatedPopEA[Gene, Fitness](
      seed,
      logger,
      new StandardParams(problem).copy(maxIters = maxIters),
      problem
    )
    with IteratedEA[Gene, Fitness]

trait BinaryRandomInitialization[Fitness] {
  def initialize(ind: Individual[Bit, Fitness], idx: Int, eaState: EAState[Bit, Fitness]) {
    BinaryInitialization.random(ind.chromosome, eaState.rnd)
    ind.fitness = eaState.problem.computeFitness(ind)
  }
}

trait BinaryBitFlipMutation[Fitness] {
  def mutate(ind: Individual[Bit, Fitness], eaState: EAState[Bit, Fitness]) {
    BinaryMutation.flipBit(ind.chromosome, eaState.params.mutProb, eaState.rnd)
  }
}

case class StandardSteadyStateIteratedBinaryEA[Fitness](
    seed: Int,
    logger: Logger[Fitness],
    override val problem: Problem[Bit, Fitness],
    maxIters: Int
)(implicit ord: Ordering[Fitness])
    extends StandardSteadyStateIteratedEA[Bit, Fitness](seed, logger, problem, maxIters)
    with BinaryRandomInitialization[Fitness]
    with BinaryBitFlipMutation[Fitness]

case class StandardSteadyStateNonRepeatedPopIteratedBinaryEA[Fitness](
    seed: Int,
    logger: Logger[Fitness],
    override val problem: Problem[Bit, Fitness],
    maxIters: Int
)(implicit ord: Ordering[Fitness])
    extends StandardSteadyStateNonRepeatedPopIteratedEA[Bit, Fitness](
      seed,
      logger,
      problem,
      maxIters
    )
    with BinaryRandomInitialization[Fitness]
    with BinaryBitFlipMutation[Fitness]

case class StandardSteadyStateTimedBinaryEA[Fitness](
    seed: Int,
    logger: Logger[Fitness],
    override val problem: Problem[Bit, Fitness],
    maxRunTime: Seconds
)(implicit ord: Ordering[Fitness])
    extends StandardSteadyStateTimedEA[Bit, Fitness](seed, logger, problem, maxRunTime)
    with BinaryRandomInitialization[Fitness]
    with BinaryBitFlipMutation[Fitness]

case class StandardSteadyStateNonRepeatedPopTimedBinaryEA[Fitness](
    seed: Int,
    logger: Logger[Fitness],
    override val problem: Problem[Bit, Fitness],
    maxRunTime: Seconds
)(implicit ord: Ordering[Fitness])
    extends StandardSteadyStateNonRepeatedPopTimedEA[Bit, Fitness](
      seed,
      logger,
      problem,
      maxRunTime
    )
    with BinaryRandomInitialization[Fitness]
    with BinaryBitFlipMutation[Fitness]
