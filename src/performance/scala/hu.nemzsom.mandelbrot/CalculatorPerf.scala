package hu.nemzsom.mandelbrot

import scala.concurrent._
import rx.lang.scala.Subscription
import scala.concurrent.duration.Duration

trait PerformanceTest {
  val times: Int
}

case class CalcSpec(width: Int, height: Int, topLeft: Complex, realRange: Double, times: Int) extends PerformanceTest

class CalculatorPerf(implicit ec: ExecutionContext) {

  def measure(spec: CalcSpec): Unit = {
    // title
    println(s"Measure for $spec")
    println("---------------------------------------------------------------")
    // warm up
    print("Warm up")
    (1 to 5).foreach {_ =>
      oneCalculation(spec)
      print('.')
    }
    println()
    // calculate
    val ts = (1 to spec.times).map { n =>
      val time = oneCalculation(spec)
      println(s"$n: $time ms")
      time
    }
    // end report
    println("---------------------------------------------------------------")
    println(s"avarage: ${ts.sum / spec.times} ms (min: ${ts.min}, max: ${ts.max})")
  }
  
  private def oneCalculation(spec: CalcSpec): Long = {
    val area = Area(spec.topLeft, spec.realRange / (spec.height - 1), spec.width, spec.height)
    val calculator = new Calculator(area, Mock.Plotter)
    val p = promise[Unit]()
    val done = p.future
    var time = System.nanoTime
    var subscription: Subscription = null
    subscription = calculator.calculate().subscribe (
      stat => stat match {
        case CalcStat(_, _, maxIter) => if (maxIter > 1000) subscription.unsubscribe()
      },
      error => println(error),
      () => {
        time = System.nanoTime - time
        p.success()
      }
    )
    Await.result(done, Duration.Inf)
    time / 1000000
  }
}
