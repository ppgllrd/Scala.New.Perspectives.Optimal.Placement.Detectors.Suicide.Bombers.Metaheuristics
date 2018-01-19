package opsbd

import opsbd.VerticesCache.Idx

import scala.util.Random

object Map {
  def fromString(xs: String, cellDimension: Int, expectedCasualties: Array[Double]): Map = {
    val rows = xs.lines.length
    val columns = xs.lines.next().length

    require(
      xs.lines.forall(_.length == columns),
      "Map.fromString: all lines in map should be of same length"
    )

    val nEntrances = xs.count(_.toByte == Cell.entrance)
    val nObjectives = xs.count(_.toByte == Cell.objective)
    val nBlocks = xs.count(_.toByte == Cell.blocked)

    val m = Map(
      rows,
      columns,
      cellDimension,
      nEntrances,
      nObjectives,
      nBlocks,
      expectedCasualties,
      fromFile = false
    )

    var i = 0
    var j = 0
    for (x <- xs) {
      if (x == '\r' || x == '\n') {
        if (j > 0) i += 1
        j = 0
      } else {
        m.cells(j)(i) = x.toByte
        j += 1
      }
    }
    m.update()
    return m
  }

  def fromScanner(sc: Scanner): Map = {
    val rows = sc.nextIntLn()
    val columns = sc.nextIntLn()
    val cellDimension = sc.nextIntLn()

    val nEntrances = sc.nextIntLn()
    val entrances = Array.ofDim[Idx](nEntrances)
    var scLn = Scanner(sc.nextLine())
    for (i <- 0 until nEntrances) entrances(i) = scLn.nextInt()

    val nObjectives = sc.nextIntLn()
    val objectives = Array.ofDim[Idx](nObjectives)
    scLn = Scanner(sc.nextLine())
    for (i <- 0 until nObjectives) objectives(i) = scLn.nextInt()

    val nBlocks = sc.nextIntLn()
    val blocks = Array.ofDim[Idx](nBlocks)
    scLn = Scanner(sc.nextLine())
    for (i <- 0 until nBlocks) blocks(i) = scLn.nextInt()

    val expectedCasualties = Array.ofDim[Double](nObjectives)
    scLn = Scanner(sc.nextLine())
    for (i <- 0 until nObjectives) expectedCasualties(i) = scLn.nextDouble()

    val m = Map(
      rows,
      columns,
      cellDimension,
      nEntrances,
      nObjectives,
      nBlocks,
      expectedCasualties,
      fromFile = true
    )

    for (i <- 0 until rows) for (j <- 0 until columns) m.cells(j)(i) = Cell.free

    for (e <- entrances) {
      val Vertex(x, y) = m.verticesCache(e)
      m.cells(x)(y) = Cell.entrance
    }

    for (o <- objectives) {
      val Vertex(x, y) = m.verticesCache(o)
      m.cells(x)(y) = Cell.objective
    }

    for (b <- blocks) {
      val Vertex(x, y) = m.verticesCache(b)
      m.cells(x)(y) = Cell.blocked
    }

    // read shortest paths
    for (i <- 0 until m.idxPaths.length) {
      val len = sc.nextIntLn()
      val scLn = Scanner(sc.nextLine())
      var idxPath = List[Idx]()
      for (i <- 0 until len) idxPath ::= scLn.nextInt()
      m.idxPaths(i) = idxPath.reverse
    }

    m.update()
    return m
  }

  def toFileName(
      rows: Int,
      columns: Int,
      nObjectives: Int,
      nEntrances: Int,
      nBlocks: Int,
      cellDimension: Int,
      nInst: Int
  ): String = {
    val fmt = "rows_%d__cols_%d__objs_%d__ents_%d__blks_%d__cellDim_%d__inst_%d.txt"
    val fileName = fmt.format(rows, columns, nObjectives, nEntrances, nBlocks, cellDimension, nInst)
    return fileName
  }

  def random(
      seed: Int,
      rows: Int,
      columns: Int,
      cellDimension: Int,
      nEntrancesPerSide: Int,
      nObjectives: Int,
      nBlocks: Int,
      expectedCasualties: Array[Double] // per objective (in top.bottom, left to right order
  ): Map = random(
    new Random(seed),
    rows,
    columns,
    cellDimension,
    nEntrancesPerSide,
    nObjectives,
    nBlocks,
    expectedCasualties
  )

  def random(
      rnd: Random,
      rows: Int,
      columns: Int,
      cellDimension: Int,
      nEntrancesPerSide: Int,
      nObjectives: Int,
      nBlocks: Int,
      expectedCasualties: Array[Double] // per objective (in top.bottom, left to right order
  ): Map = {

    val m = Map(
      rows,
      columns,
      cellDimension,
      nEntrancesPerSide * 4,
      nObjectives,
      nBlocks,
      expectedCasualties,
      fromFile = false
    )

    // place entrances on top/bottom
    for (y <- Array(0, rows - 1)) {
      var entrancesLeft = nEntrancesPerSide
      while (entrancesLeft > 0) {
        val x = rnd.nextInt(columns)
        if (m.cells(x)(y) == Cell.free) {
          m.cells(x)(y) = Cell.entrance
          entrancesLeft -= 1
        }
      }
    }

    // place entrances on left/right
    for (x <- Array(0, columns - 1)) {
      var entrancesLeft = nEntrancesPerSide
      while (entrancesLeft > 0) {
        val y = rnd.nextInt(rows)
        if (m.cells(x)(y) == Cell.free) {
          m.cells(x)(y) = Cell.entrance
          entrancesLeft -= 1
        }
      }
    }

    def placeRandom(n: Int, cellType: Cell): Unit = {
      var nLeft = n
      while (nLeft > 0) {
        val x = rnd.nextInt(columns)
        val y = rnd.nextInt(rows)
        if (m.cells(x)(y) == Cell.free) {
          m.cells(x)(y) = cellType
          nLeft -= 1
        }
      }
    }

    placeRandom(nBlocks, Cell.blocked)
    placeRandom(nObjectives, Cell.objective)

    m.update()
    return m
  }
}

case class Map(
    rows: Int,
    columns: Int,
    var cellDimension: Int,
    nEntrances: Int,
    nObjectives: Int,
    nBlocks: Int,
    var expectedCasualties: Array[Double] // per objective (in top.bottom, left to right order
    ,
    fromFile: Boolean
) {

  require(
    expectedCasualties.length == nObjectives,
    "Map: length of populationDensity must be same as nObjectives %d %d"
      .format(expectedCasualties.length, nObjectives)
  )

  val cells = Array.ofDim[Cell](columns, rows)
  for (y <- 0 until rows) for (x <- 0 until columns) cells(x)(y) = Cell.free

  import VerticesCache._

  val verticesCache = VerticesCache(this)

  val entrances = new Array[Idx](nEntrances)
  val objectives = new Array[Idx](nObjectives)
  val blocks = new Array[Idx](nBlocks)

  val idxPaths = Array.ofDim[IdxPath](nEntrances * nObjectives)

  // Search for coordinates of entries and objectives in map
  def update(): Unit = {
    // Find entrances, objectives and blocks
    var ents = 0
    var objs = 0
    var blks = 0

    var y = 0
    while ((ents < nEntrances || objs < nObjectives || blks < nBlocks) && y < rows) {
      var x = 0
      while ((ents < nEntrances || objs < nObjectives || blks < nBlocks) && x < columns) {
        val cell = cells(x)(y)
        if (cell == Cell.entrance) {
          entrances(ents) = verticesCache.find(x, y)
          ents += 1
        } else if (cell == Cell.objective) {
          objectives(objs) = verticesCache.find(x, y)
          objs += 1
        } else if (cell == Cell.blocked) {
          blocks(blks) = verticesCache.find(x, y)
          blks += 1
        }
        x += 1
      }
      y += 1
    }

    // Compute shortest paths from entrances to objectives
    if (!fromFile) {
      val neighborCache = Dijkstra.NeighborCache(this)
      var nPaths = 0
      if (objectives.length < entrances.length) for (objs <- objectives) {
        for (idxPath <- Dijkstra.shortestPathsFromTo(objs, entrances, neighborCache)) {
          idxPaths(nPaths) = idxPath.reverse
          nPaths += 1
        }
      }
      else for (ent <- entrances) {
        for (idxPath <- Dijkstra.shortestPathsFromTo(ent, objectives, neighborCache)) {
          idxPaths(nPaths) = idxPath
          nPaths += 1
        }
      }
    }
  }

  private val cellSz = cellDimension

  def vertex2Coord(vertex: Vertex): Coord =
    Coord((vertex.x - 0.5) * cellSz, (vertex.y - 0.5) * cellSz)

  def vertex2Coord(vertexIdx: Idx): Coord = vertex2Coord(verticesCache(vertexIdx))

  def distance(vertex1: Vertex, vertex2: Vertex): Double = {
    val dx = (vertex1.x - vertex2.x) * cellSz
    val dy = (vertex1.y - vertex2.y) * cellSz
    math.sqrt(dx * dx + dy * dy)
  }

  def distance(vertexIdx1: Idx, vertexIdx2: Idx): Double =
    distance(verticesCache(vertexIdx1), verticesCache(vertexIdx2))

  private val (left, top, right, bottom) = {
    val Coord(left0, top0) = vertex2Coord(Vertex(0, 0))
    val Coord(right0, bottom0) = vertex2Coord(Vertex(columns - 1, rows - 1))
    val r = cellSz / 2.0
    (left0 - r, top0 - r, right0 + r, bottom0 + r)

  }

  def inMap(coord: Coord): Boolean = left < coord.x && coord.x < right && top < coord.y &&
    coord.y < bottom

  override def toString = {
    val sb = new StringBuilder()
    sb.append(rows + "\trows\n")
    sb.append(columns + "\tcolumns\n")
    sb.append(cellDimension + "\tcellDimension\n")
    sb.append(nEntrances + "\tnEntrances\n")
    sb.append(nObjectives + "\tnObjectives\n")
    sb.append(nBlocks + "\tnBlocks\n")
    for (pd <- expectedCasualties) sb.append(pd + " ")
    sb.append("\texpectedCasualties\n")
    for (i <- 0 until rows) {
      for (j <- 0 until columns) sb.append(cells(j)(i).toChar)
      sb.append("\n")
    }
    sb.toString()
  }

  def toPrintWriter(pw: PrintWriter) {
    pw.println(rows, "rows")
    pw.println(columns, "columns")
    pw.println(cellDimension, "cellDimension")
    pw.println(nEntrances, "nEntrances")
    for (e <- entrances) pw.print(e + " ")
    pw.println()
    pw.println(nObjectives, "nObjectives")
    for (o <- objectives) pw.print(o + " ")
    pw.println()
    pw.println(nBlocks, "nBlocks")
    for (b <- blocks) pw.print(b + " ")
    pw.println()
    var ln = ""
    for (pd <- expectedCasualties) ln += ("%.3f " format pd)
    pw.println(ln.init, "expectedCasualties")
    for (i <- 0 until idxPaths.length) {
      val path = idxPaths(i)
      pw.println(path.length, "path " + i)
      for (i <- 0 until path.length - 1) pw.print(path(i) + " ")
      if (path.length > 0) pw.print(path.last)
      pw.println()
    }
  }

  def toFile(fileName: String) = {
    val file = new java.io.File(fileName)
    val pw = PrintWriter(file)
    toPrintWriter(pw)
    pw.close()
  }

  def toFile(nInst: Int): String = {
    val fileName = Map
      .toFileName(rows, columns, nObjectives, nEntrances, nBlocks, cellDimension, nInst)
    toFile(fileName)
    return fileName
  }
}


