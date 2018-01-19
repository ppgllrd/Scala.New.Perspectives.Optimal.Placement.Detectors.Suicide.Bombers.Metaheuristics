/** ****************************************************************************
  *
  * Mutation operators
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

import java.util.Random

object BinaryMutation {
  def flipBit(chromosome: Chromosome[Bit], mutProb: Probability, rnd: Random): Unit = {
    for (i <- 0 until chromosome.size)
      if (rnd.nextDouble() < mutProb) chromosome(i) = Bits.flip(chromosome(i))
  }
}
