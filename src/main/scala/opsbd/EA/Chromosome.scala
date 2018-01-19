/** ****************************************************************************
  *
  * A Chromosome is an indexed & mutable sequence of genes
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

trait Chromosome[Gene] extends collection.mutable.IndexedSeq[Gene] {
  def copyFrom(that: Chromosome[Gene])
}
