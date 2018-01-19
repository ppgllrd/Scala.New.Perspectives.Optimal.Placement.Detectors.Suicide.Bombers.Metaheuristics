package opsbd

object VerticesCache {
  type Idx = Int
}

case class VerticesCache(map: Map) {
  import VerticesCache._

  val vertices = new Array[Vertex](map.rows * map.columns)
  init()

  private def init(): Unit = {
    var idx = 0
    for (y <- 0 until map.rows) for (x <- 0 until map.columns) {
      vertices(idx) = Vertex(x, y)
      idx += 1
    }
  }

  def apply(idx: Idx): Vertex = vertices(idx)

  val size: Int = vertices.length

  def find(x: Int, y: Int): Idx = vertices.indexWhere(v => v.x == x && v.y == y)

  def foreach[A](p: Idx => A) { for (i <- 0 until vertices.length) p(i) }
}
