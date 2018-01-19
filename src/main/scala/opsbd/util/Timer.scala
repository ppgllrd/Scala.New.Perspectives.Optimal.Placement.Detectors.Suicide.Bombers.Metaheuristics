/** ****************************************************************************
  *
  * Timer for measuring elapsed execution times
  *
  * @ Pepe Gallardo, 2016
  *
  * ***************************************************************************
  */

package opsbd.util

import java.lang.management.ManagementFactory

case class Timer() {

  private val nanosecond = 1000000000.0

  private val bean = ManagementFactory.getThreadMXBean
  if (!bean.isCurrentThreadCpuTimeSupported) sys
    .error("isCurrentThreadCpuTimeSupported not supported")

  // Only valid for single threaded applications
  // see: http://nadeausoftware.com/articles/2008/03/java_tip_how_get_cpu_and_user_time_benchmarking#TimingasinglethreadedtaskusingCPUsystemandusertime

  private def getCpuTime() = bean.getCurrentThreadCpuTime

  private var startTime = getCpuTime() // System.currentTimeMillis()

  // resets start of timer to current time
  def reset(): Unit = {
    startTime = getCpuTime() // System.currentTimeMillis()
  }

  // returns elapsed time (in seconds) since last reset
  def elapsedTime(): opsbd.Seconds = {
    val t = getCpuTime() // System.currentTimeMillis()
    val segs = (t - startTime).toDouble / nanosecond
    segs
  }
}
