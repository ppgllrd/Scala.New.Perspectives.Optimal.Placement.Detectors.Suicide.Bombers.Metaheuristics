package opsbd

// Dijkstra's shortest paths algorithm
object Dijkstra {
  import VerticesCache._

  case class NeighborCache(map: Map) {
    private val cellSz = map.cellDimension
    private val cellSizeDiv2 = cellSz.toDouble / 2

    private val UNKNOWN = 0.toByte
    private val FALSE = 1.toByte
    private val TRUE = 2.toByte
    private val cache = Array.ofDim[Byte](map.verticesCache.size, map.verticesCache.size)

    def areNeighbor(vertex1: Idx, vertex2: Idx): Boolean = {
      if (cache(vertex1)(vertex2) == UNKNOWN) {
        val res = if (computeAreNeighbor(vertex1, vertex2)) TRUE else FALSE
        cache(vertex1)(vertex2) = res
        cache(vertex2)(vertex1) = res
      }
      return cache(vertex1)(vertex2) == TRUE
    }

    // if line from vertex1 to vertex2 does not cross a block
    def computeAreNeighbor(vertex1: Idx, vertex2: Idx): Boolean = {
      val coord1 = map.vertex2Coord(vertex1)
      val coord2 = map.vertex2Coord(vertex2)

      for (blockIdx <- map.blocks) {
        val coordBlock = map.vertex2Coord(blockIdx)

        // check if line crosses diagonals of blocked cell
        if (Geometry.lineIntersectCell(coord1, coord2, coordBlock, cellSizeDiv2)) return false
      }
      return true
    }
  }

  def shortestPathsFromTo(
      src: Idx,
      dests: Seq[Idx],
      neighborCache: NeighborCache
  ): List[IdxPath] = {
    val disconnected = -1
    val infinite = Double.MaxValue

    val map = neighborCache.map

    val distances = Array.fill[Double](map.verticesCache.size)(infinite)
    val predecessors = Array.fill[Idx](map.verticesCache.size)(disconnected)
    var q = scala.collection.mutable.PriorityQueue[Idx]()(Ordering by (-distances(_)))

    for (vtx <- map.verticesCache) {
      if (vtx == src) {
        distances(src) = 0
        predecessors(src) = src
      }
      val Vertex(x, y) = map.verticesCache(vtx)
      if (Cell.isAccessible(map.cells(x)(y))) q.enqueue(vtx)
    }

    var destinations = dests.size

    while (q.nonEmpty && destinations > 0) {
      val u = q.dequeue()
      if (predecessors(u) == disconnected) sys
        .error("shortestPathsFromTo: not all destinations could be reached")

      if (dests.contains(u)) destinations -= 1 // we have reached a new destination

      if (destinations > 0) {
        for (v <- q; if neighborCache.areNeighbor(u, v)) {
          val d = distances(u) + map.distance(u, v)

          if (d < distances(v)) {
            distances(v) = d
            predecessors(v) = u
          }
        }

        // inefficiently reorder elements in queue :(
        // should use a Fibonacci Heap instead
        q = q.clone()
      }
    }

    // this shouldn't happen
    if (destinations > 0) sys.error("shortestPathsFromTo: some destinations couldn't be reached!")

    def getPath(vertex0: Idx): IdxPath = {
      var vertex = vertex0
      var path = List[Idx](vertex)
      while (vertex != src) {
        vertex = predecessors(vertex)
        path ::= vertex
      }
      return path
    }

    return for (d <- dests.toList) yield getPath(d)
  }
}
