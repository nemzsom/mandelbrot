package hu.nemzsom.mandelbrot

import scala.compat.Platform.currentTime

/** Common trait for specify a performance test */
trait PerformanceTestSpec {
  /** Test execution count */
  val times: Int
}

abstract class PerformanceTester[T <: PerformanceTestSpec] {

  def measure(spec: T): Unit = {
    // title
    println(s"Measure for $spec")
    println("---------------------------------------------------------------")
    // warm up
    print("Warm up")
    val warmEnd = currentTime + 4000 // 4 sec
    while (currentTime < warmEnd) {
      work(spec)
      print('.')
    }
    println()
    // calculate
    if (!verbose) print("Measure")
    val ts = (1 to spec.times).map { n =>
      val time = work(spec)
      if (verbose) println(s"$n: $time ms")
      else print('.')
      time
    }
    if (!verbose) println()
    cleanup()
    // end report
    println("---------------------------------------------------------------")
    println(s"average: ${ts.sum / spec.times} ms (min: ${ts.min}, max: ${ts.max}) [total: ${ts.sum}]")
    println()
  }

  /**
   * Performs the work to measure. 
   * @param spec Specification of the work
   * @return Elapsed time in milliseconds
   */
  def work(spec: T): Long

  /** Called after the measurement completed*/
  def cleanup(): Unit = {}

  def verbose: Boolean = false
}
