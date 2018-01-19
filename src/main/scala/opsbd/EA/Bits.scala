/** ****************************************************************************
  *
  * Bits, represented as bytes (value is either 0 or 1)
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.EA

object Bits {
  type BitImp = Byte // Either 0 or 1

  def flip(b: Bit): Bit = (1 - b).toByte

  val zero: Bit = 0.toByte

  val one: Bit = 1.toByte
}
