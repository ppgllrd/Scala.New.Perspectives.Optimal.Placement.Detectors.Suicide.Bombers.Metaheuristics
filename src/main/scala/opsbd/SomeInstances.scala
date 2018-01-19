package opsbd

object Casualties {
  import scala.math._
  import scala.util.Random

  // As per Eq(2) in "Operational effectiveness of suicide-bomber-detector schemes: A best-case analysis"
  def computeExpectedCasualties(lambda: Double, b: Double, tau: Double) = 2 * Pi /
    (lambda * b * b) * (1 - (1 + lambda * b * tau) * exp(-lambda * b * tau))

  /* Random double in normal distribution  */
  def normal(rnd: Random, mu: Double = 0, sigma: Double = 1): Double = mu +
    sigma * rnd.nextGaussian()

  def expectedCasualties(
      rnd: Random,
      individualBaseWidth: Double = 0.5,
      targetAreaRadius: Double = 10
  ) = {
    val popDensity = normal(rnd, 0.4, 0.1)
    computeExpectedCasualties(popDensity, individualBaseWidth, targetAreaRadius)
  }
}

object RealInstances {

  def fractal(inst: Instance, seed: Int = 1): Instance = {

    val rnd = new scala.util.Random(seed)

    var nObjectives = 0
    var lines = List[String]()
    for (i <- 0 until inst.map.rows) {
      val sbLn = new StringBuffer()
      for (j <- 0 until inst.map.columns) {
        val oldCell = inst.map.cells(j)(i)
        if (oldCell == Cell.objective) nObjectives += 1

        val newCell =
          if (oldCell == Cell.entrance) {
            if (i < inst.map.rows - 1 && j < inst.map.columns - 1) oldCell else Cell.free
          } else oldCell
        sbLn.append(newCell.toChar)
      }
      val line = sbLn.toString
      lines ::= (line + line.reverse + "\n")
    }
    val temp = lines
    for (l <- temp) lines ::= l

    nObjectives *= 4

    println(inst.map.nObjectives, nObjectives)
    println(lines.fold("")(_ + _))

    val map = Map.fromString(
      lines.fold("")(_ + _),
      inst.map.cellDimension,
      Array.fill(nObjectives)(Casualties.expectedCasualties(rnd))
    )

    lazy val i = Instance(
      map,
      inst.nDetectors,
      inst.detectorRadius,
      inst.uniformPath,
      inst.instantaneousDetectionRate,
      inst.neutralizingProb,
      inst.neutralizingDistance
    )

    i
  }

  def inst1(nDetectors: Int, detectorRadius: Int = 20, seed: Int = 1) = {
    val rnd = new scala.util.Random(seed)

    val nObjectives = 61

    lazy val m1 = Map.fromString(
      """|EEE____________E________________E
      |E___OOO____________OOOOOOOO_____E
      |E________________________________
      |___***________________OOO________
      |___***_______________***OO_______
      |___***_______________***OO_____O_
      |_____________**_**___***_______O_
      |_____________**_**_____________O_
      |_____________**_**_____________O_
      |_____________**O**_____***_____O_
      |_____________**O**_____***_____O_
      |_____________**O**_____OOO_____O_
      |_____________**O**_____OOO__*____
      |_____________**O**__________*____
      |_____________**O**_______________
      |E_____**________________________E
      |E_____**________________________E
      |_____________**O**_______________
      |_____________**O**__*__**__*_____
      |_____________**O**__*__**__*_____
      |_____________**O**__*__**__*_____
      |E____________**O**__*__**__*_____
      |E____________**O**__*__**__*_____
      |_____________**_**__*__**__*_____
      |______**_____**_**__*__**__*___O_
      |______**_____**_**_____**__*___O_
      |____________________*__**______O_
      |_________________________________
      |E____________________******______
      |E________________________________
      |E____OOOOOOO_______OOOOOOOO_____E
      |EE_____________E________________E""".stripMargin,
      cellDimension = 5,
      Array.fill(nObjectives)(Casualties.expectedCasualties(rnd))
    )

    lazy val i1 = Instance(m1, nDetectors, detectorRadius = detectorRadius, true, 0.06, 0.6, 10)

    if (m1.nObjectives != nObjectives) sys.error("RealInstances.inst1: wrong nObjectives")

    i1
  }

  def inst2(nDetectors: Int, detectorRadius: Int = 20, seed: Int = 1) = {
    val rnd = new scala.util.Random(seed)

    val nObjectives = 25

    lazy val m2 = Map.fromString(
      """*****E_*******E****_E**E********
        |******__________**__***___O__***
        |***************____***__**____**
        |****************__****_******__E
        |***************__****__*******__
        |**************__*****_******___*
        |E______******__*****_O******_*_*
        |*****_________******_******__*_*
        |*****____________**__*****__**_*
        |*****________O**____******_***_*
        |*****________O*****___***__***_*
        |*****__*_____O*******_____****_*
        |*****____________*******___***_*
        |E_____________**___O****_*___*_*
        |*_***_******__*****______***____
        |*_***_******__********_*******_E
        |*_***_******O_********_*******_*
        |E_***_******__********_*******_*
        |*_***_******__*******__*******_*
        |*_***___****__*******_********_*
        |*_****___________****_********_*
        |*_****OOO***__**_______*******_*
        |*_****OOO***__*******_********_*
        |______OO****__*******_*____***_*
        |_*****__****__******____**_____*
        |E_***_OO****__*****__******OO__*
        |*_______****__****__*******OO__*
        |*_****__****__***__********OO__*
        |*_*****___**__**__*********OO__*
        |*_*******_________************_E
        |*_******__***__**___**********_*
        |*E******E****E_****__E********E*""".stripMargin,
      cellDimension = 7,
      Array.fill(nObjectives)(Casualties.expectedCasualties(rnd))
    )

    lazy val i2 = Instance(m2, nDetectors, detectorRadius = detectorRadius, true, 0.06, 0.6, 10)

    if (m2.nObjectives != nObjectives) sys.error("RealInstances.inst2: wrong nObjectives")

    i2
  }

  def inst3(nDetectors: Int, detectorRadius: Int = 20, seed: Int = 1) = {
    val rnd = new scala.util.Random(seed)

    val nObjectives = 61

    lazy val m3 = Map.fromString(
      """|EEE************E****************E
        |E___OOO____________OOOOOOOO_____E
        |E_______________________________*
        |*__***________________OOO_______*
        |*__***_______________***OO______*
        |*__***_______________***OO_____O*
        |*____________**_**___***_______O*
        |*____________**_**_____________O*
        |*____________**_**_____________O*
        |*____________**O**_____***_____O*
        |*____________**O**_____***_____O*
        |*____________**O**_____OOO_____O*
        |*____________**O**_____OOO__*___*
        |*____________**O**__________*___*
        |*____________**O**______________*
        |E_____**________________________E
        |E_____**________________________E
        |*____________**O**______________*
        |*____________**O**__*__**__*____*
        |*____________**O**__*__**__*____*
        |*____________**O**__*__**__*____*
        |E____________**O**__*__**__*____*
        |E____________**O**__*__**__*____*
        |*____________**_**__*__**__*____*
        |*_____**_____**_**__*__**__*___O*
        |*_____**_____**_**_____**__*___O*
        |*___________________*__**______O*
        |*_______________________________*
        |E____________________******_____*
        |E_______________________________*
        |E____OOOOOOO_______OOOOOOOO_____E
        |EE*************E****************E""".stripMargin,
      cellDimension = 5,
      Array.fill(nObjectives)(Casualties.expectedCasualties(rnd))
    )

    lazy val i3 = Instance(m3, nDetectors, detectorRadius = detectorRadius, true, 0.06, 0.6, 10)

    if (m3.nObjectives != nObjectives) sys.error("RealInstances.inst1: wrong nObjectives")

    i3
  }

  def inst4(nDetectors: Int, detectorRadius: Int = 20, seed: Int = 1) = {
    val rnd = new scala.util.Random(seed)

    val nObjectives = 9

    lazy val m4 = Map.fromString(
      """|E****E**************E***E*******
        |_****_**************_***_*******
        |_****_**************_***__******
        |_****_****OO********_****_******
        |______****__********_****__*****
        |_****_****__********__****_*****
        |_****_****__*********_****__****
        |_*********__*********_*****_****
        |_*********OO*********_*****_****
        |_______________________________E
        |E______________________________*
        |__*____________************____*
        |__*____________**************__*
        |_______________**************__*
        |__*____________**************__*
        |__*__*O________O_____O*******__*
        |__*__**________O_____O*******__*
        |_______________**************__*
        |__*____________**************__*
        |__*____________**************__*
        |________________________________
        |E______________________________E
        |_*******_____________*****_*****
        |_*********__**_______*****_*****
        |_*********__**_______*****_*****
        |_*********__**________****_*****
        |____________**_____*******_*****
        |_****__***__***___********_*****
        |_***__****__***__*********_*****
        |_**__*****__**************_*****
        |_*__******__**************_*****
        |E__*******E_**************E*****""".stripMargin,
      cellDimension = 11,
      Array.fill(nObjectives)(Casualties.expectedCasualties(rnd))
    )

    lazy val i4 = Instance(m4, nDetectors, detectorRadius = detectorRadius, true, 0.06, 0.6, 10)

    if (m4.nObjectives != nObjectives) sys.error("RealInstances.inst1: wrong nObjectives")

    i4
  }

  def inst5(nDetectors: Int, detectorRadius: Int = 20, seed: Int = 1) = {
    val rnd = new scala.util.Random(seed)

    val nObjectives = 61

    lazy val m5 = Map.fromString(
      """|EEE***********E****************E
        |E___OOO___________OOOOOOOO_____E
        |E______________________________*
        |*__***_______________OOO_______*
        |*__***______________***OO______*
        |*__***______________***OO_____O*
        |*___________**_**___***_______O*
        |*___________**_**_____________O*
        |*___________**_**_____________O*
        |*___________**O**_____***_____O*
        |*___________**O**_____***_____O*
        |*___________**O**_____OOO_____O*
        |*___________**O**_____OOO__*___*
        |*___________**O**__________*___*
        |*___________**O**______________*
        |E_____**_______________________E
        |E_____**_______________________E
        |*___________**O**______________*
        |*___________**O**__*__**__*____*
        |*___________**O**__*__**__*____*
        |*___________**O**__*__**__*____*
        |E___________**O**__*__**__*____*
        |E___________**O**__*__**__*____*
        |*___________**_**__*__**__*____*
        |*_____**____**_**__*__**__*___O*
        |*_____**____**_**_____**__*___O*
        |*__________________*__**______O*
        |*______________________________*
        |E___________________******_____*
        |E______________________________*
        |E____OOOOOOO______OOOOOOOO_____E
        |EE************E****************E""".stripMargin,
      cellDimension = 5,
      Array.fill(nObjectives)(Casualties.expectedCasualties(rnd))
    )

    lazy val i5 = Instance(m5, nDetectors, detectorRadius = detectorRadius, true, 0.06, 0.6, 10)

    if (m5.nObjectives != nObjectives) sys.error("RealInstances.inst1: wrong nObjectives")

    i5
  }

}

object SomeInstances {

  lazy val m0 = Map.fromString(
    """E______
        |_______
        |_______
        |___*___
        |__*____
        |_*_____
        |_______
        |_____O_
        |""".stripMargin,
    10,
    Array(37.322)
  )

  lazy val i0 = Instance(m0, 3, 10, true, 0.06, 0.6, 10)

  lazy val m1 = Map.fromString(
    """__E__E__
      |_*______
      |E_*__O_E
      |_______*
      |_*_O__*_
      |E____O_E
      |____*___
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322, 37.322, 37.322)
  )

  lazy val i1 = Instance(m1, 3, 10, true, 0.06, 0.6, 10)

  lazy val m2 = Map.fromString(
    """__E__E__
      |________
      |E____O_E
      |________
      |___O____
      |E____O_E
      |________
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322, 37.322, 37.322)
  )

  lazy val i2 = Instance(m2, 3, 10, true, 0.06, 0.6, 10)

  lazy val m3 = Map.fromString(
    """__E__E__
      |_*______
      |E_*__O_E
      |_______*
      |_*_O__*_
      |E______E
      |____*___
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322, 37.322)
  )

  lazy val i3 = Instance(m3, 3, 10, true, 0.06, 0.6, 10)

  lazy val m4 = Map.fromString(
    """__E__E__
      |________
      |E____O_E
      |________
      |___O____
      |E______E
      |________
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322, 37.322)
  )

  lazy val i4 = Instance(m4, 3, 10, true, 0.06, 0.6, 10)

  lazy val m5 = Map.fromString(
    """__E__E__
      |_*______
      |E_*____E
      |_______*
      |_*_O__*_
      |E______E
      |____*___
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322)
  )

  lazy val i5 = Instance(m5, 3, 10, true, 0.06, 0.6, 10)

  lazy val m6 = Map.fromString(
    """__E__E__
      |________
      |E______E
      |________
      |___O____
      |E______E
      |________
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322)
  )

  lazy val i6 = Instance(m6, 3, 10, true, 0.06, 0.6, 10)

  lazy val m7 = Map.fromString(
    """__E__E__
      |_*______
      |__*__O__
      |E______*
      |_*_O__*_
      |_____O_E
      |____*___
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322, 37.322, 37.322)
  )

  lazy val i7 = Instance(m7, 3, 10, true, 0.06, 0.6, 10)

  lazy val m8 = Map.fromString(
    """__E__E__
      |________
      |_____O__
      |E_______
      |___O____
      |_____O_E
      |________
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322, 37.322, 37.322)
  )

  lazy val i8 = Instance(m8, 3, 10, true, 0.06, 0.6, 10)

  lazy val m9 = Map.fromString(
    """__E__E__
      |_*______
      |__*__O__
      |E______*
      |_*_O__*_
      |_______E
      |____*___
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322, 37.322)
  )

  lazy val i9 = Instance(m9, 3, 10, true, 0.06, 0.6, 10)

  lazy val m10 = Map.fromString(
    """__E__E__
      |________
      |_____O__
      |E_______
      |___O____
      |_______E
      |________
      |__E__E__
      |""".stripMargin,
    10,
    Array(37.322, 37.322)
  )

  lazy val i10 = Instance(m10, 3, 10, true, 0.06, 0.6, 10)

  lazy val instances = Array(i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)

  /*
    lazy val nObjectives = 2
    lazy val m = Map.random(3, 10, 10, 10, 2, nObjectives, 6, Array.fill(nObjectives)(37.322))
    println(m)
    lazy val i = Instance(m, 3, 10, true, 0.06, 0.6, 10)

    lazy val randomInstances = Array(i)
   */

  import scala.math._
  // As per Eq(2) in Operational effectiveness of suicide-bomber-detector schemes: A best-case analysis
  def computeExpectedCasualties(lambda: Double, b: Double, tau: Double) = 2 * Pi /
    (lambda * b * b) * (1 - (1 + lambda * b * tau) * exp(-lambda * b * tau))

  def randomExts(
      dim: Int,
      nObjs: Int,
      nEntrancesPerSide: Int,
      nBlocks: Int,
      nDets: Int,
      detRad: Int,
      cellDim: Int,
      i: Int,
      precomputeCache: Boolean = true
  ) = {
    //val nObjs = 3, entrances = 2, detectors = 5

    import scala.util.Random
    val seed = dim +
      100 *
      (nObjs +
        10 *
        (nEntrancesPerSide +
          10 * (nDets + 10 * (detRad + 10 * (cellDim + 10 * (nBlocks + 10 * i))))))
    val rnd = new Random(seed)

    /* Double aleatorio en distribución normal, con media y desviación estándar dadas */
    def normal(mu: Double = 0, sigma: Double = 1): Double = mu + sigma * rnd.nextGaussian()

    def popDensity = normal(0.4, 0.1)
    val individualBaseWidth = 0.5
    val targetAreaRadius = 10
    def expCasualties = computeExpectedCasualties(popDensity, individualBaseWidth, targetAreaRadius)

    var wrong = true
    var inst: Instance = null

    while (wrong) {
      try {
        wrong = false
        //val m = Map.random(rnd, dim, dim, cellDimension = 5, nEntrancesPerSide = nEnts, nObjectives = nObjs, nBlocks = 15, Array.fill(nObjs)(30.7322))
        val m = Map.random(
          rnd,
          dim,
          dim,
          cellDimension = cellDim,
          nEntrancesPerSide = nEntrancesPerSide,
          nObjectives = nObjs,
          nBlocks = nBlocks,
          Array.fill(nObjs)(expCasualties)
        )
        //println(m)
        //inst = Instance(m, nDetectors = nDets, detectorRadius = 20, true, 0.06, 0.6, neutralizingDistance = 10)
        inst = Instance(
          m,
          nDetectors = nDets,
          detectorRadius = detRad,
          true,
          0.06,
          0.6,
          neutralizingDistance = 10,
          precomputeCache = precomputeCache
        )
      } catch {
        case e: Exception => {
          println(e)
          wrong = true
        }
      }
    }
    inst
  }

  def random(
      dim: Int,
      nObjs: Int,
      nEntrancesPerSide: Int,
      nDets: Int,
      detRad: Int,
      cellDim: Int,
      i: Int
  ) = {
    //val nObjs = 3, entrances = 2, detectors = 5

    import scala.util.Random
    val seed = dim +
      100 *
      (nObjs + 10 * (nEntrancesPerSide + 10 * (nDets + 10 * (detRad + 10 * (cellDim + 10 * i)))))
    val rnd = new Random(seed)

    /* Double aleatorio en distribución normal, con media y desviación estándar dadas */
    def normal(mu: Double = 0, sigma: Double = 1): Double = mu + sigma * rnd.nextGaussian()

    def popDensity = normal(0.4, 0.1)
    val individualBaseWidth = 0.5
    val targetAreaRadius = 10
    def expCasualties = computeExpectedCasualties(popDensity, individualBaseWidth, targetAreaRadius)

    var wrong = true
    var inst: Instance = null

    while (wrong) {
      try {
        wrong = false
        //val m = Map.random(rnd, dim, dim, cellDimension = 5, nEntrancesPerSide = nEnts, nObjectives = nObjs, nBlocks = 15, Array.fill(nObjs)(30.7322))
        val m = Map.random(
          rnd,
          dim,
          dim,
          cellDimension = cellDim,
          nEntrancesPerSide = nEntrancesPerSide,
          nObjectives = nObjs,
          nBlocks = 15,
          Array.fill(nObjs)(expCasualties)
        )
        //println(m)
        //inst = Instance(m, nDetectors = nDets, detectorRadius = 20, true, 0.06, 0.6, neutralizingDistance = 10)
        inst = Instance(
          m,
          nDetectors = nDets,
          detectorRadius = detRad,
          true,
          0.06,
          0.6,
          neutralizingDistance = 10
        )
      } catch {
        case e: Exception => {
          //println(e)
          wrong = true
        }
      }
    }
    inst
  }

  def randomOld(dim: Int, nObjs: Int, nEnts: Int, nDets: Int, i: Int) = {
    //val nObjs = 3, entrances = 2, detectors = 5

    import scala.util.Random
    val seed = dim + 100 * (nObjs + 10 * (nEnts + 10 * (nDets + 10 * i)))
    val rnd = new Random(seed)

    var wrong = true
    var inst: Instance = null

    while (wrong) {
      try {
        wrong = false
        //val m = Map.random(rnd, dim, dim, cellDimension = 5, nEntrancesPerSide = nEnts, nObjectives = nObjs, nBlocks = 15, Array.fill(nObjs)(30.7322))
        val m = Map.random(
          rnd,
          dim,
          dim,
          cellDimension = 5,
          nEntrancesPerSide = nEnts,
          nObjectives = nObjs,
          nBlocks = 15,
          Array.fill(nObjs)(30.7322)
        )
        //println(m)
        //inst = Instance(m, nDetectors = nDets, detectorRadius = 20, true, 0.06, 0.6, neutralizingDistance = 10)
        inst = Instance(
          m,
          nDetectors = nDets,
          detectorRadius = 20,
          true,
          0.06,
          0.6,
          neutralizingDistance = 10
        )
      } catch {
        case e: Exception => {
          //println(e)
          wrong = true
        }
      }
    }
    inst
  }

  lazy val random32Instances = Array.tabulate[Instance](10)(random(32, 3, 2, 5, 20, 5, _))
}

object TestToFromFile extends App {
  // Use English formats
  import java.util.Locale
  Locale.setDefault(new Locale.Builder().setLanguage("en").setRegion("US").build())

  import opsbd.util.Timer
  val timer1 = Timer()

  val inst = SomeInstances.random(32, 2, 2, 4, 20, 5, 0)
  println(timer1.elapsedTime())
  val fileName = inst.toFile(0)
  println(fileName)

  val timer2 = Timer()
  val inst2 = Instance.fromFile(fileName)
  println(timer2.elapsedTime())

}
