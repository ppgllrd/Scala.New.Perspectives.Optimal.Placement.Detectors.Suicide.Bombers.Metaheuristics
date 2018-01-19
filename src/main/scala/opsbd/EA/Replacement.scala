/** ****************************************************************************
  *
  * Replacement operators
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

import java.util.Random

object Replacement {
  def worst[Gene, Fitness](pop: Population[Gene, Fitness], ind: Individual[Gene, Fitness]): Unit = {
    pop.replace(pop.worstIdx, ind)
  }

  def random[Gene, Fitness](
      pop: Population[Gene, Fitness],
      ind: Individual[Gene, Fitness],
      rnd: Random
  ): Unit = { pop.replace(rnd.nextInt(pop.size), ind) }

  def randomButBest[Gene, Fitness](
      pop: Population[Gene, Fitness],
      ind: Individual[Gene, Fitness],
      rnd: Random
  ): Unit = { pop.replace(rnd.nextInt(pop.size - 1), ind) }

  def randomButBest[Gene, Fitness](
      n: Int,
      pop: Population[Gene, Fitness],
      ind: Individual[Gene, Fitness],
      rnd: Random
  ): Unit = { pop.replace(rnd.nextInt(pop.size - n), ind) }

  def best[Gene, Fitness](pop: Population[Gene, Fitness], ind: Individual[Gene, Fitness]): Unit = {
    pop.replace(pop.bestIdx, ind)
  }
}
