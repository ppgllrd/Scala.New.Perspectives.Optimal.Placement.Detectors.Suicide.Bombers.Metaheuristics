package opsbd

// x in [0,map.columns)  and  y in [0,map.rows)
case class Vertex(x: Int, y: Int) {
  override lazy val hashCode = 31 * (31 * 17 + x) + y
}
