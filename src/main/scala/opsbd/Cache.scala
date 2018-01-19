package opsbd

import java.io._

object Cache {
  def apply(instance: Instance): Cache = {
    val cache = new Cache(instance)
    cache.init()
    cache
  }

  private def cacheFileName(instanceFileName: String): String = { "./cache/" + instanceFileName }

  def fromFile(instance: Instance, instanceFileName: String): Cache = {
    val cache = new Cache(instance)
    cache.fromFile(cacheFileName(instanceFileName))
    cache
  }

  def doesFileExists(instanceFileName: String): Boolean = {
    val file = new File(cacheFileName(instanceFileName))
    return file.exists()
  }
}

class Cache private (instance: Instance) {
  import VerticesCache.Idx

  private val map = instance.map

  private val table = Array.ofDim[Double](map.verticesCache.size, instance.pathsInfo.length)
  val total = Array.ofDim[Double](map.verticesCache.size)
  val dominated = Array.ofDim[Int](map.verticesCache.size)

  def init(): Unit = {
    for (vtxIdx <- map.verticesCache) {
      val coordC = map.vertex2Coord(vtxIdx)
      var t = 0.0
      for (i <- 0 until instance.pathsInfo.length) {
        val path = instance.pathsInfo(i).path
        var inter = 0.0
        for (idx <- 0 until path.length - 1) inter +=
          Geometry.pathCircleIntersection(path(idx), path(idx + 1), coordC, instance.detectorRadius)
        table(vtxIdx)(i) = inter //total intersection of detector at row y, column x with i-th path
        t += inter
      }
      total(vtxIdx) = t
    }

    // compute times each cell is dominated by another one
    for (i <- 0 until map.verticesCache.size) {
      val vtxiIdx = map.verticesCache(i)
      if (!Cell.isAccessible(map.cells(vtxiIdx.x)(vtxiIdx.y)))
        dominated(i) = map.verticesCache.size - 1
      else {
        for (j <- 0 until map.verticesCache.size) {
          val vtxjIdx = map.verticesCache(j)
          if (i != j && Cell.isAccessible(map.cells(vtxjIdx.x)(vtxjIdx.y))) {
            var isDominated = true
            var worse = 0
            var p = 0
            while (isDominated && p < instance.pathsInfo.length) {
              val tip = table(i)(p)
              val tjp = table(j)(p)

              if (tip > tjp) isDominated = false else if (tip < tjp) worse += 1
              p += 1
            }
            if (isDominated && worse > 0) dominated(i) += 1
          }
        }
      }
    }
  }

  def intersection(idx: Idx)(path: Int): Double = table(idx)(path)

  def print(): Unit = {
    for (vtxIdx <- map.verticesCache) {
      printf("%6.2f ", total(vtxIdx))
      if ((1 + vtxIdx) % map.columns == 0) println
    }
  }

  def allCandidates(): scala.collection.mutable.BitSet = {
    // candidates to place detectors
    val candidates = new scala.collection.mutable.BitSet(map.verticesCache.size)
    for (idx <- 0 until map.verticesCache.size) {
      val v = map.verticesCache(idx)
      if (
        Cell.isAccessible(map.cells(v.x)(v.y)) && total(idx) > 0 &&
        (dominated(idx) < instance.nDetectors)
      ) candidates += idx
    }
    candidates
  }

  def toDataOutputStream(dos: DataOutputStream) {
    for (i <- table.indices) for (j <- table(i).indices) dos.writeDouble(table(i)(j))
    for (t <- total) dos.writeDouble(t)
    for (d <- dominated) dos.writeInt(d)
  }

  def toFile(instanceFileName: String) {
    val file = new File(Cache.cacheFileName(instanceFileName))
    val dos = new DataOutputStream(new FileOutputStream(file))
    toDataOutputStream(dos)
    dos.close()
  }

  def fromDataInputStream(dis: DataInputStream) {
    for (i <- table.indices) for (j <- table(i).indices) table(i)(j) = dis.readDouble()
    for (i <- total.indices) total(i) = dis.readDouble()
    for (i <- dominated.indices) dominated(i) = dis.readInt()
  }

  def fromFile(fileName: String) {
    val file = new File(fileName)
    val dis = new DataInputStream(new FileInputStream(file))
    fromDataInputStream(dis)
    dis.close()
  }
}
