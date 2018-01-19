package opsbd.util

class Rnd(seed: Long) extends scala.util.Random(seed) {

  /* Random double in normal distribution  */
  def normal(mu: Double = 0, sigma: Double = 1): Double = mu + sigma * this.nextGaussian()
}

object Rnd {
  def apply(seed: Long) = new Rnd(seed)
}
