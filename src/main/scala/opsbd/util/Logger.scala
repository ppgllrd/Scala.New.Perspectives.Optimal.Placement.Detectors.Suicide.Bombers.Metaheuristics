/** ****************************************************************************
  *
  * Class to log evolution of solutions along execution time
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.util

import scala.collection.mutable.ArrayBuffer

case class Logger[Fitness](timer: Timer = new Timer(), echo: Boolean = true) {

  import Fitness.fitness2Format
  import opsbd.Seconds

  class Slot(val fitness: Fitness, val iter: Int, val time: Seconds) {
    override def toString = ("%10d\t" + fitness2Format(fitness) + "\t%10.2f")
      .format(iter, fitness, time)
  }

  class SlotWithFormat(fitness: Fitness, iter: Int, time: Seconds, format: String)
      extends Slot(fitness, iter, time) {
    override def toString = format.format(iter, fitness, time)
  }

  private val slots = new ArrayBuffer[Slot]()

  private def addSlot(slot: Slot) {
    slots += slot
    if (echo) println(slot)
  }

  def bestFitness: Fitness = slots.last.fitness

  def timeBestFitness: Seconds = slots.last.time max 0.00001

  def register(iter: Int, fitness: Fitness): Unit = {
    val slot = new Slot(fitness, iter, timer.elapsedTime())
    addSlot(slot)
  }

  def register(format: String, iter: Int, fitness: Fitness): Unit = {
    val slot = new SlotWithFormat(fitness, iter, timer.elapsedTime(), format)
    addSlot(slot)
  }

  def print(
      prefix: String = "",
      maxTime: Seconds = -1,
      step: Int = 50,
      pw: java.io.PrintWriter = new java.io.PrintWriter(System.out)
  ): Unit = {
    if (slots.isEmpty) {
      pw.println(prefix + "No solutions!")
      return
    }

    val maxTime1 = if (maxTime < 0) slots.last.time + step else maxTime
    val last = slots.last

    pw.println((prefix + "LAST: " + fitness2Format(last.fitness)).format(last.fitness))

    for (mt <- 0 to maxTime1.toInt by step) {
      val xs = slots.takeWhile(_.time <= mt)
      if (xs.nonEmpty) {
        val last = xs.last
        pw.println((prefix + "LAST %d: " + fitness2Format(last.fitness)).format(mt, last.fitness))
      }
    }

    val logFormat = "%d " + fitness2Format(last.fitness) + " %.2f  "
    pw.print(prefix + "LOG: ")
    for (s <- slots)
      //pw.print(logFormat.format(s.iter, s.fitness, s.time))
      pw.print(s.toString)
    pw.println()
    pw.flush()
  }
}
