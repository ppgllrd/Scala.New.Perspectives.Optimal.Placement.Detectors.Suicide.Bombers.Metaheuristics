/** ****************************************************************************
  *
  * Individuals
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

trait Individual[Gene, Fitness] {

  var fitness: Fitness = _
  val chromosome: Chromosome[Gene]

  def copyFrom(that: Individual[Gene, Fitness])
}
