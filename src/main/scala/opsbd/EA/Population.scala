/** ****************************************************************************
  *
  * Population of individuals. It's kept sorted in ascending order wrt
  * fitness of individuals
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

trait Population[Gene, Fitness] {

  implicit protected val ord: Ordering[Fitness]

  val size: Int // number of individuals

  protected val ea: EA[Gene, Fitness] // access to EA where this population is used

  protected val individuals: Array[Individual[Gene, Fitness]]

  def apply(idx: Int): Individual[Gene, Fitness] = individuals(idx)

  val worstIdx: Int = if (ea.params.isMaximization) 0 else size - 1
  def worst(): Individual[Gene, Fitness] = individuals(worstIdx)

  val bestIdx: Int = if (ea.params.isMaximization) size - 1 else 0
  def best(): Individual[Gene, Fitness] = individuals(bestIdx)

  def sort(): Unit = {
    // sorted in ascending order wrt fitness
    scala.util.Sorting.quickSort(individuals)(Ordering by (_.fitness))
  }

  def initialize(eaState: EAState[Gene, Fitness]) {
    for (i <- 0 until size) ea.initialize(individuals(i), i, eaState)
    sort()
  }

  // replace an individual and keep resulting population sorted
  def replace(idx: Int, ind: Individual[Gene, Fitness]): Unit = {
    import ord._
    val toReplace = individuals(idx)
    var i = idx

    if (i > 0 && ind.fitness < individuals(i - 1).fitness) {
      // float upwards
      do {
        individuals(i) = individuals(i - 1)
        i -= 1
      } while (i > 0 && ind.fitness < individuals(i - 1).fitness)
    } else if (i < size - 1 && ind.fitness > individuals(i + 1).fitness) {
      // push downwards
      do {
        individuals(i) = individuals(i + 1)
        i += 1
      } while (i < size - 1 && ind.fitness > individuals(i + 1).fitness)
    }
    toReplace.copyFrom(ind)
    individuals(i) = toReplace
  }

  private def checkInOrder() {
    import ord._
    for (i <- 0 until size - 1) if (individuals(i).fitness > individuals(i + 1).fitness) sys
      .error("checkInOrder: failed. Individuals are not in order")
  }
}

case class StandardPopulation[Gene: Manifest, Fitness](size: Int, ea: EA[Gene, Fitness])(implicit
    val ord: Ordering[Fitness]
) extends Population[Gene, Fitness] {
  protected val individuals = Array
    .fill[Individual[Gene, Fitness]](size)(new ArrayIndividual[Gene, Fitness](ea.problem.numVars))
}

// Repeated individuals are not allowed
case class NonRepeatedPopulation[Gene: Manifest, Fitness](size: Int, ea: EA[Gene, Fitness])(implicit
    val ord: Ordering[Fitness]
) extends Population[Gene, Fitness] {
  protected val individuals = Array
    .fill[Individual[Gene, Fitness]](size)(new ArrayIndividual[Gene, Fitness](ea.problem.numVars))

  private def contains(ind: Individual[Gene, Fitness], maxIdx: Int): Boolean = {
    for (i <- 0 until maxIdx) if (ind == individuals(i)) return true
    false
  }

  override def initialize(eaState: EAState[Gene, Fitness]) {
    for (i <- 0 until size)
      do ea.initialize(individuals(i), i, eaState) while (contains(individuals(i), i))
    sort()
  }

  // replace an individual and keep resulting population sorted
  override def replace(idx: Int, ind: Individual[Gene, Fitness]): Unit = {
    if (!contains(ind, size)) super.replace(idx, ind)
  }
}
