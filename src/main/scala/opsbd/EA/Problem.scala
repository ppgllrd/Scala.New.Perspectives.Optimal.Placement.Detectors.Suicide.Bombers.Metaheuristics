/** ****************************************************************************
  *
  * Especification of combinatorial problems to be solved
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

trait Problem[Gene, Fitness] {
  // number of variables in chromosome
  def numVars: Int

  // evaluates the chromosome corresponding to a solution
  def computeFitness(ind: Individual[Gene, Fitness]): Fitness

  // checks if solution is optimal
  def isOptimal(ind: Individual[Gene, Fitness]): Boolean
}
