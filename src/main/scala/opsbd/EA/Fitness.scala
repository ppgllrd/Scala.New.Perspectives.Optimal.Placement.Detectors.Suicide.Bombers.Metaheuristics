/** ****************************************************************************
  *
  * Operations related to Fitness
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

object Fitness {

  def fitness2Format[Fitness](fitness: Fitness) = fitness match {
    case i: Int    => "%d"
    case f: Float  => "%.5f"
    case d: Double => "%.5f"
    case _         => "%s"
  }

}
