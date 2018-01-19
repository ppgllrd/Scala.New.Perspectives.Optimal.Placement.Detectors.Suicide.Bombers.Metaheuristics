package opsbd

// x in horizontal axis and y is vertical one.
// In Double coordinates (see vertex2Coord for relationship between Vertex and Coord)
case class Coord(x: Double, y: Double) {
  override lazy val hashCode = {
    def toInt(d: Double) = {
      val l = java.lang.Double.doubleToLongBits(d)
      (l ^ (l >>> 32)).toInt
    }
    31 * (31 * 17 + toInt(x)) + toInt(y)
  }
}
