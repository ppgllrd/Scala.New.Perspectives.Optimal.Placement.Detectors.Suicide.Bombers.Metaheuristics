/** ****************************************************************************
  *
  * Recombination operators
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

import java.util.Random

object Recombination {
  def uniform[Gene](
      child: Chromosome[Gene],
      parent1: Chromosome[Gene],
      parent2: Chromosome[Gene],
      rnd: Random
  ): Unit = {
    for (i <- 0 until child.size) child(i) = (if (rnd.nextDouble() < 0.5) parent1 else parent2)(i)
  }

  def singlePoint[Gene](
      child: Chromosome[Gene],
      parent1: Chromosome[Gene],
      parent2: Chromosome[Gene],
      rnd: Random
  ): Unit = {
    val p = rnd.nextInt(child.size)

    for (i <- 0 until p) child(i) = parent1(i)
    for (i <- p until child.size) child(i) = parent2(i)
  }

  def singlePoint[Gene](
      child: ArrayChromosome[Gene],
      parent1: ArrayChromosome[Gene],
      parent2: ArrayChromosome[Gene],
      rnd: Random
  ): Unit = {
    val p = rnd.nextInt(child.size)

    child.copyFrom(parent1, 0, 0, p)
    child.copyFrom(parent2, p, p, child.size - p)
  }
}
