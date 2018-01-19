package opsbd

object Geometry {
  import java.awt.geom.Line2D.linesIntersect
  import scala.math._

  // Tests whether line segment from pt0 to pt1 intersects with cell at ptc
  // by testing if line intersects one of the diagonals of cell
  def lineIntersectCell(pt0: Coord, pt1: Coord, ptc: Coord, cellRadius: Double): Boolean =
    if (
      linesIntersect(
        pt0.x,
        pt0.y,
        pt1.x,
        pt1.y,
        ptc.x - cellRadius,
        ptc.y - cellRadius,
        ptc.x + cellRadius,
        ptc.y + cellRadius
      )
    ) true
    else if (
      linesIntersect(
        pt0.x,
        pt0.y,
        pt1.x,
        pt1.y,
        ptc.x - cellRadius,
        ptc.y + cellRadius,
        ptc.x + cellRadius,
        ptc.y - cellRadius
      )
    ) true
    else false

  // Length of intersection of segment from pt0 to pt1 to circle at ptc with given `radius'
  def pathCircleIntersection(pt0: Coord, pt1: Coord, ptc: Coord, radius: Double): Double = {
    // a goes from pt0 to center of circle
    val ax = ptc.x - pt0.x
    val ay = ptc.y - pt0.y

    // b goes from pt0 to pt1
    val bx = pt1.x - pt0.x
    val by = pt1.y - pt0.y
    val pbb = bx * bx + by * by
    // len is modulus of b
    val len = sqrt(pbb)

    val pab = ax * bx + ay * by

    // a1 is dx from pt0 to center of circle
    val a1 = pab / len

    val f = pab / pbb

    // a2 goes from center of circle and is perpendicular to b
    val a2x = ax - f * bx
    val a2y = ay - f * by

    // a2Sq is square of dy from pt0 to center of circle
    val a2Sq = a2x * a2x + a2y * a2y

    val disc = radius * radius - a2Sq
    if (disc < 0) 0 // too far apart
    else {
      val sqr = sqrt(disc)
      val xplus = a1 + sqr
      val xminus = a1 - sqr

      val c1 = xminus max 0
      val c2 = xplus min len
      (c2 - c1) max 0
    }
  }

  def trimEnd(path: Path, len0: Double): Path = {
    var xs = path.reverse
    var ys = List[Coord]()
    var len = len0
    var stop = false
    while (!stop) {
      val xst = xs.tail
      if (xs.isEmpty || xst.isEmpty) {
        return List[Coord]() //sys.error("shortenEnd: cannot shorten path")
      } else {
        val c1 = xs.head
        val c2 = xst.head
        val dx = c1.x - c2.x
        val dy = c1.y - c2.y
        val d = sqrt(dx * dx + dy * dy)
        if (d < len) {
          // remove last segment
          len = len - d
          xs = xst
        } else if (d == len) {
          xs = xst
          stop = true
        } else {
          // remove len units from last segment
          val dPrime = d - len
          val f = dPrime / d
          val c1b = Coord(c2.x + dx * f, c2.y + dy * f)
          ys = c1b :: ys
          for (c <- xst) ys = c :: ys
          stop = true
        }
      }
    }
    ys
  }

  val oneDegree = 2 * Pi / 360

  def extendBegin(path: Path, len: Double, map: Map): Path = {

    def rot(alpha: Double, coord: Coord, len: Double) =
      Coord(coord.x + cos(alpha) * len, coord.y + sin(alpha) * len)

    val xs = path
    val xst = xs.tail
    if (xs.isEmpty || xst.isEmpty) { sys.error("extendBegin: cannot extend path") }
    else {
      val c1 = xs.head
      val c2 = xst.head
      val dx = c2.x - c1.x
      val dy = c2.y - c1.y
      val d = sqrt(dx * dx + dy * dy)

      // check if extending last segment does not get out of map
      val cellDiag = 1.05 * sqrt(2 * pow(map.cellDimension / 2.0, 2))
      val ct = {
        val dPrime = d + cellDiag
        val f = dPrime / d
        Coord(c2.x - dx * f, c2.y - dy * f)
      }

      if (map.inMap(ct)) {
        // we need to add another segment and rotate it out of map
        var alpha = atan2(-dy, -dx)

        var delta1 = 0.0
        var pt = rot(alpha + delta1, c1, cellDiag)
        while (map.inMap(pt)) {
          delta1 += oneDegree
          pt = rot(alpha + delta1, c1, cellDiag)
        }

        var delta2 = 0.0
        pt = rot(alpha + delta2, c1, cellDiag)
        while (map.inMap(pt)) {
          delta2 -= oneDegree
          pt = rot(alpha + delta2, c1, cellDiag)
        }

        alpha += (if (delta2.abs > delta1) delta1 else delta2)

        val c1b = rot(alpha, c1, len)
        c1b :: path
      } else {
        // Just extend last segment in same direction
        val dPrime = d + len
        val f = dPrime / d
        val c1b = Coord(c2.x - dx * f, c2.y - dy * f)
        c1b :: xst
      }
    }
  }

  def bresenham(x0: Int, y0: Int, x1: Int, y1: Int): Iterator[Vertex] = {
    val dx = math.abs(x1 - x0)
    val sx = if (x0 < x1) 1 else -1
    val dy = math.abs(y1 - y0)
    val sy = if (y0 < y1) 1 else -1

    def it = new Iterator[Vertex] {
      var x = x0
      var y = y0
      var err = (if (dx > dy) dx else -dy) / 2
      def next = {
        val res = Vertex(x, y)
        val e2 = err
        if (e2 > -dx) { err -= dy; x += sx }
        if (e2 < dy) { err += dx; y += sy }
        res
      }
      def hasNext = sx * x <= sx * x1 && sy * y <= sy * y1
    }
    it
  }
}

object GeomTest extends App {
  val it1 = Geometry.bresenham(0, 0, 1, 5)
  for (c <- it1) print(c + " ")
}

object GeomTest2 extends App {
  val l = Geometry.pathCircleIntersection(Coord(20, 10), Coord(50, 50), Coord(20, 20), 10)
  println(l)
}
