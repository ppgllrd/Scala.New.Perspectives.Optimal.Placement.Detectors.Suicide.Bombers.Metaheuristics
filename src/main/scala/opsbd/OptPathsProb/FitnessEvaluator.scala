package opsbd.OptPathsProb

import opsbd.util.Timer
import opsbd.{Instance, VerticesCache}


// alternative simplex algorithm in Java see http://scpsolver.org/

case class FitnessEvaluator(instance: Instance) {
  import VerticesCache.Idx

  val cache = instance.cache
  val map = instance.map

  val numVariables = instance.pathsInfo.length

  import scpsolver.constraints.LinearConstraint
  import scpsolver.constraints.LinearBiggerThanEqualsConstraint
  import scpsolver.constraints.LinearSmallerThanEqualsConstraint
  import scpsolver.constraints.LinearEqualsConstraint

  import scpsolver.lpsolver.LinearProgramSolver
  import scpsolver.lpsolver.SolverFactory
  import scpsolver.problems.LinearProgram

  import scpsolver.util.SparseVector

  val constraints = new Array[LinearConstraint](2 * numVariables + 1)
  var lastConstraint = 0

  // Each variable is a prob of taking a path

  // Each prob must be 0 <= prob <= 1
  for (i <- 0 until numVariables) {
    val sparseVector = new SparseVector(numVariables, 1)
    sparseVector.set(i, 1)
    constraints(lastConstraint) =
      new LinearSmallerThanEqualsConstraint(sparseVector, 1.0, "cSE" + i)
    lastConstraint += 1
  }

  for (i <- 0 until numVariables) {
    val sparseVector = new SparseVector(numVariables, 1)
    sparseVector.set(i, 1)
    constraints(lastConstraint) = new LinearBiggerThanEqualsConstraint(sparseVector, 0.0, "cBE" + i)
    lastConstraint += 1
  }

  // Sum of all probs must be == 1
  constraints(lastConstraint) =
    new LinearEqualsConstraint(Array.fill(numVariables)(1.0), 1.0, "cEQ")

  // LP objective to maximize. Depends on detectors so it's initialized in eval
  val c = new Array[Double](numVariables)

  def eval(detectors: Seq[Idx]): Double = {

    // define objective function
    for (pathIdx <- 0 until instance.pathsInfo.length) { // for all paths
      val pathInfo = instance.pathsInfo(pathIdx)

      var totalIntersection = 0.0
      for (dect <- detectors) totalIntersection += cache.intersection(dect)(pathIdx)

      c(pathIdx) = map.expectedCasualties(pathInfo.objectiveOrder) *
        (math.exp(-instance.instantaneousDetectionRate * totalIntersection) *
          instance.neutralizingProb + (1 - instance.neutralizingProb))
    }

    // Define LP problem
    val lp = new LinearProgram(c)
    for (constraint <- constraints) lp.addConstraint(constraint)
    lp.setMinProblem(false)

    // Solve LP problem

    val solver: LinearProgramSolver = SolverFactory.newDefault()
    //val glpkSolver = solver.asInstanceOf[GLPKSolver]
    //RemoveHook.remove(glpkSolver);

    val soledVars: Array[Double] = solver.solve(lp)

    var solValue = 0.0
    for (i <- 0 until soledVars.length) solValue += c(i) * soledVars(i)

    return solValue
  }

  def eval2(detectors: Seq[Idx]): (Double, Array[Int]) = {

    // val timer = Timer()

    // define objective function
    for (pathIdx <- 0 until instance.pathsInfo.length) { // for all paths
      val pathInfo = instance.pathsInfo(pathIdx)

      var totalIntersection = 0.0
      for (dect <- detectors) totalIntersection += cache.intersection(dect)(pathIdx)

      c(pathIdx) = map.expectedCasualties(pathInfo.objectiveOrder) *
        (math.exp(-instance.instantaneousDetectionRate * totalIntersection) *
          instance.neutralizingProb + (1 - instance.neutralizingProb))
    }

    // Define LP problem
    val lp = new LinearProgram(c)
    for (constraint <- constraints) lp.addConstraint(constraint)
    lp.setMinProblem(false)

    // Solve LP proble

    val solver: LinearProgramSolver = SolverFactory.newDefault()
    //val glpkSolver = solver.asInstanceOf[GLPKSolver]
    //RemoveHook.remove(glpkSolver);

    val soledVars: Array[Double] = solver.solve(lp)

    var solValue = 0.0
    for (i <- 0 until soledVars.length) solValue += c(i) * soledVars(i)

    return (solValue, (0 until soledVars.length).filter(soledVars(_) > 0).toArray)
  }

  def evalGreedy(detectors: Seq[Idx]): (Double, Array[Int]) = {

    var maximizingPathIdx = 0
    var maxV = -1.0

    // define objective function
    for (pathIdx <- 0 until instance.pathsInfo.length) { // for all paths
      val pathInfo = instance.pathsInfo(pathIdx)

      var totalIntersection = 0.0
      for (dect <- detectors) totalIntersection += cache.intersection(dect)(pathIdx)

      //val pNonDetection = math.exp(-instance.instantaneousDetectionRate * totalIntersection)
      //val v = pNonDetection * map.expectedCasualties(pathInfo.objectiveOrder)
      val v = map.expectedCasualties(pathInfo.objectiveOrder) *
        (math.exp(-instance.instantaneousDetectionRate * totalIntersection) *
          instance.neutralizingProb + (1 - instance.neutralizingProb))

      if (v > maxV) {
        maxV = v
        maximizingPathIdx = pathIdx
      }
    }

    // define objective function
    val solValue = {
      val pathIdx = maximizingPathIdx

      val pathInfo = instance.pathsInfo(pathIdx)

      var totalIntersection = 0.0
      for (dect <- detectors) totalIntersection += cache.intersection(dect)(pathIdx)

      map.expectedCasualties(pathInfo.objectiveOrder) *
        (math.exp(-instance.instantaneousDetectionRate * totalIntersection) *
          instance.neutralizingProb + (1 - instance.neutralizingProb))

    }

    return (solValue, Array(maximizingPathIdx))
  }
}

case class FitnessEvaluatorGreedy(instance: Instance) {
  import VerticesCache.Idx

  val cache = instance.cache
  val map = instance.map

  val numVariables = instance.pathsInfo.length

  import scpsolver.constraints.LinearConstraint
  import scpsolver.constraints.LinearBiggerThanEqualsConstraint
  import scpsolver.constraints.LinearSmallerThanEqualsConstraint
  import scpsolver.constraints.LinearEqualsConstraint

  import scpsolver.lpsolver.LinearProgramSolver
  import scpsolver.lpsolver.SolverFactory
  import scpsolver.problems.LinearProgram

  import scpsolver.util.SparseVector

  val constraints = new Array[LinearConstraint](2 * numVariables + 1)
  var lastConstraint = 0

  // Each variable is a prob of taking a path

  // Each prob must be 0 <= prob <= 1
  for (i <- 0 until numVariables) {
    val sparseVector = new SparseVector(numVariables, 1)
    sparseVector.set(i, 1)
    constraints(lastConstraint) =
      new LinearSmallerThanEqualsConstraint(sparseVector, 1.0, "cSE" + i)
    lastConstraint += 1
  }

  for (i <- 0 until numVariables) {
    val sparseVector = new SparseVector(numVariables, 1)
    sparseVector.set(i, 1)
    constraints(lastConstraint) = new LinearBiggerThanEqualsConstraint(sparseVector, 0.0, "cBE" + i)
    lastConstraint += 1
  }

  // Sum of all probs must be == 1
  constraints(lastConstraint) =
    new LinearEqualsConstraint(Array.fill(numVariables)(1.0), 1.0, "cEQ")

  // LP objective to maximize. Depends on detectors so it's initialized in eval
  val c = new Array[Double](numVariables)

  def eval(detectors: Seq[Idx]): Double = {

    val timer = Timer()

    // define objective function
    for (pathIdx <- 0 until instance.pathsInfo.length) { // for all paths
      val pathInfo = instance.pathsInfo(pathIdx)

      var totalIntersection = 0.0
      for (dect <- detectors) totalIntersection += cache.intersection(dect)(pathIdx)

      c(pathIdx) = map.expectedCasualties(pathInfo.objectiveOrder) *
        (math.exp(-instance.instantaneousDetectionRate * totalIntersection) *
          instance.neutralizingProb + (1 - instance.neutralizingProb))
    }

    // Define LP problem
    val lp = new LinearProgram(c)
    for (constraint <- constraints) lp.addConstraint(constraint)
    lp.setMinProblem(false)

    // Solve LP problem

    val solver: LinearProgramSolver = SolverFactory.newDefault()
    //val glpkSolver = solver.asInstanceOf[GLPKSolver]
    //RemoveHook.remove(glpkSolver);

    val soledVars: Array[Double] = solver.solve(lp)

    var solValue = 0.0
    for (i <- 0 until soledVars.length) solValue += c(i) * soledVars(i)

    return solValue
  }
}
