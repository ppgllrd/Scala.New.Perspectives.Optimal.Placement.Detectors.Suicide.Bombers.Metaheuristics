/** ****************************************************************************
  *
  * An implementation of a Chromosome by using an array
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

case class ArrayChromosome[Gene: Manifest](override val size: Int) extends Chromosome[Gene] {

  val length = size

  protected val xs = new Array[Gene](size)

  def apply(idx: Int): Gene = xs(idx)

  def update(idx: Int, g: Gene) = xs(idx) = g

  // toDo avoid dynamic casting
  def copyFrom(that: Chromosome[Gene]): Unit = that match {
    case that: ArrayChromosome[gene] => Array.copy(that.xs, 0, this.xs, 0, size)
    case _                           => sys.error("ArrayChromosome.copyFrom: ArrayChromosome expected")
  }

  def copyFrom(that: ArrayChromosome[Gene], srcPos: Int, destPos: Int, length: Int): Unit = Array
    .copy(that.xs, srcPos, this.xs, destPos, length)

  def sameGenes(that: ArrayChromosome[Gene]): Boolean = xs.sameElements(that.xs)

  override def toString: String = {
    val sb = new StringBuilder("ArrayChromosome(")
    for (i <- 0 until size - 1) sb.append(xs(i) + ",")
    if (size > 0) sb.append(xs(size - 1))
    sb.append(")")
    sb.toString()
  }
}
