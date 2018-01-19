/** ****************************************************************************
  *
  * Initialization operators
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

import java.util.Random

object Initialization {
  def random[Gene](chromosome: Chromosome[Gene], rndGen: () => Gene): Unit = {
    for (i <- 0 until chromosome.size) chromosome(i) = rndGen()
  }
}

object BinaryInitialization {
  def random(chromosome: Chromosome[Bit], rnd: Random): Unit = {
    for (i <- 0 until chromosome.size) chromosome(i) = rnd.nextInt(2).toByte
  }
}
