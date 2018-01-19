package opsbd

import java.io.File
import scala.util.Random

object Instance {
  def fromScanner(sc: Scanner, fileName: String, precomputeCache: Boolean = true): Instance = {
    val map = Map.fromScanner(sc)
    val nDetectors = sc.nextIntLn()
    val detectorRadius = sc.nextDoubleLn()
    val uniformPath = sc.nextBooleanLn()
    val instantaneousDetectionRate = sc.nextDoubleLn()
    val neutralizingProb = sc.nextDoubleLn()
    val neutralizingDistance = sc.nextDoubleLn()

    val inst = Instance(
      map,
      nDetectors,
      detectorRadius,
      uniformPath,
      instantaneousDetectionRate,
      neutralizingProb,
      neutralizingDistance,
      precomputeCache,
      fromFile = Some(fileName)
    )

    return inst
  }

  def fromFile(fileName: String, precomputeCache: Boolean = true): Instance = {
    val file = new File(fileName)
    val sc = Scanner(file)
    val inst = fromScanner(sc, fileName, precomputeCache)
    sc.close()
    return inst
  }

  def toFileName(
      rows: Int,
      columns: Int,
      nObjectives: Int,
      nEntrances: Int,
      nBlocks: Int,
      nDetectors: Int,
      detectorRadius: Double,
      cellDimension: Int,
      nInst: Int
  ): String = {
    val fmt =
      "./instances/rows_%d__cols_%d__objs_%d__ents_%d__blks_%d__detcs_%d__detcRad_%d__cellDim_%d__inst_%d.txt"
    val fileName = fmt.format(
      rows,
      columns,
      nObjectives,
      nEntrances,
      nBlocks,
      nDetectors,
      detectorRadius.toInt,
      cellDimension,
      nInst
    )
    return fileName
  }
}

case class Instance(
    map: Map,
    nDetectors: Int,
    detectorRadius: Double,
    uniformPath: Boolean,
    instantaneousDetectionRate: Double,
    neutralizingProb: Double,
    neutralizingDistance: Double,
    precomputeCache: Boolean = true,
    fromFile: Option[String] = None
) {

  def clone(nDetectors: Int, detectorRadius: Double): Instance = new Instance(
    map,
    nDetectors,
    detectorRadius,
    uniformPath,
    instantaneousDetectionRate,
    neutralizingProb,
    neutralizingDistance,
    precomputeCache,
    fromFile
  )

  // originalPath is as computed by Dijkstra
  // path is after extending begin of path and trimming its end
  case class PathInfo(idxPath: IdxPath, objectiveOrder: Int, path: Path)

  val pathsInfo = new Array[PathInfo](map.nEntrances * map.nObjectives)

  def toPrintWriter(pw: PrintWriter) = {
    map.toPrintWriter(pw)
    pw.println(nDetectors, "nDetectors")
    pw.println(detectorRadius, "detectorRadius")
    pw.println(uniformPath, "uniformPath")
    pw.println(instantaneousDetectionRate, "instantaneousDetectionRate")
    pw.println(neutralizingProb, "neutralizingProb")
    pw.println(neutralizingDistance, "neutralizingDistance")
  }

  def toFile(fileName: String) = {
    val file = new java.io.File(fileName)
    val pw = PrintWriter(file)
    toPrintWriter(pw)
    pw.close()
  }

  def toFile(nInst: Int): String = {
    val fileName = Instance.toFileName(
      map.rows,
      map.columns,
      map.nObjectives,
      map.nEntrances,
      map.nBlocks,
      nDetectors,
      detectorRadius.toInt,
      map.cellDimension,
      nInst
    )
    toFile(fileName)
    return fileName
  }

  def computePaths(idxPaths: Array[IdxPath]) {
    // Compute paths from entrances to objectives
    var nPaths = 0
    for (idxPath <- idxPaths) {
      val objOrder = map.objectives.indexOf(idxPath.last)
      val originalPath = idxPath.map(map.vertex2Coord)
      val path = Geometry
        .trimEnd(Geometry.extendBegin(originalPath, detectorRadius, map), neutralizingDistance)
      pathsInfo(nPaths) = PathInfo(idxPath, objOrder, path)
      nPaths += 1
    }
  }

  computePaths(map.idxPaths)

  val usePathProb = // per each path
    if (uniformPath) 1.0 / pathsInfo.length
    else sys.error("Instance: non-uniform probabilities of using different paths is not supported")

  // precompute cache of intersections
  val cache =
    if (precomputeCache) {
      fromFile match {
        case Some(fileName) if Cache.doesFileExists(fileName) => Cache.fromFile(this, fileName)
        case _                                                => Cache(this)
      }
    } else null
}

object ListInstances {
  val fileName =
    "data/instances/32/3/rows_32__cols_32__objs_3__ents_8__blks_15__detcs_5__detcRad_20__cellDim_5__inst_%d.txt"
  val goods = List(22, 8, 53)
  val bads = List(9, 84, 31)
}

object GenTests extends App {
  import opsbd.util.Timer

  val dim = 128
  val i = args(0).toInt

  for (nObjectives <- 2 to 20 by 2) for (nEntrancesPerSide <- 2 to 6) {
    //for(nBlocks <- 15 to 55 by 10) {
    def pctg(f: Int) = dim * dim * f / 100
    for (nBlocks <- List(5, 10, 20, 30).map(pctg(_))) {
      val nDetectors = 5
      val detRad = 20
      val cellDim = 5

      val tm = Timer()
      val fileName = Instance.toFileName(
        dim,
        dim,
        nObjectives,
        4 * nEntrancesPerSide,
        nBlocks,
        nDetectors,
        detRad,
        cellDim,
        i
      )

      val f = new java.io.File(fileName)
      var exists = false
      for (i <- 0 until 10) {
        Thread.sleep(new Random().nextInt(50))
        exists = f.exists() && !f.isDirectory()
      }

      if (exists) println("Already exists " + fileName)
      else {

        val file = new java.io.File(fileName)
        val pw = PrintWriter(file)
        pw.print(" ")
        pw.close()

        val inst0 = SomeInstances.randomExts(
          dim,
          nObjectives,
          nEntrancesPerSide,
          nBlocks,
          nDetectors,
          detRad,
          cellDim,
          i,
          precomputeCache = false
        )
        inst0.toFile(fileName)
        println(fileName)
      }
    }
  }
}

object GenTests2 extends App {
  import opsbd.util.Timer

  val dim = 128
  val i = args(0).toInt

  for (nObjectives <- 2 to 8 by 2) for (nEntrancesPerSide <- 2 to 4) {
    //for(nBlocks <- 15 to 55 by 10) {
    def pctg(f: Int) = dim * dim * f / 100
    for (nBlocks <- List(5).map(pctg(_))) {
      val nDetectors = 5
      val detRad = 20
      val cellDim = 5

      val tm = Timer()
      val fileName = Instance.toFileName(
        dim,
        dim,
        nObjectives,
        4 * nEntrancesPerSide,
        nBlocks,
        nDetectors,
        detRad,
        cellDim,
        i
      )

      val f = new java.io.File(fileName)
      var exists = false
      for (i <- 0 until 10) {
        Thread.sleep(new Random().nextInt(50))
        exists = f.exists() && !f.isDirectory()
      }

      if (exists) println("Already exists " + fileName)
      else {

        val file = new java.io.File(fileName)
        val pw = PrintWriter(file)
        pw.print(" ")
        pw.close()

        val inst0 = SomeInstances.randomExts(
          dim,
          nObjectives,
          nEntrancesPerSide,
          nBlocks,
          nDetectors,
          detRad,
          cellDim,
          i,
          precomputeCache = false
        )
        inst0.toFile(fileName)
        println(fileName)
      }
    }
  }
}
