package opsbd

object Cell {
  val free: Cell = '_'.toByte
  val entrance: Cell = 'E'.toByte
  val objective: Cell = 'O'.toByte
  val blocked: Cell = '*'.toByte

  def isAccessible(cell: Cell): Boolean = cell != blocked
}
