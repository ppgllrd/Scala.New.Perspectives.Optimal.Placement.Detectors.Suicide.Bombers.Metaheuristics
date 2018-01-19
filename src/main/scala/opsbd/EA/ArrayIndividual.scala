/** ****************************************************************************
  *
  * Individual with ArrayChromosome
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

case class ArrayIndividual[Gene: Manifest, Fitness](numVars: Int)
    extends Individual[Gene, Fitness] {
  val chromosome = new ArrayChromosome[Gene](numVars)

  def copyFrom(that: Individual[Gene, Fitness]): Unit = {
    this.chromosome.copyFrom(that.chromosome)
    this.fitness = that.fitness
  }

  override def equals(that: Any): Boolean = that match {
    case ind: ArrayIndividual[Gene, Fitness] => this.chromosome.sameGenes(ind.chromosome)
    case _                                   => false
  }

  override def toString: String = {
    val fitnessFormat = Fitness.fitness2Format(fitness)
    val sb = new StringBuilder(("ArrayIndividual(" + fitnessFormat + ", ").format(fitness))
    sb.append(chromosome)
    sb.append(")")
    sb.toString()
  }
}
