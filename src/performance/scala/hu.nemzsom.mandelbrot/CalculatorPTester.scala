package hu.nemzsom.mandelbrot

import scala.concurrent._
import rx.lang.scala.Subscription
import scala.concurrent.duration.Duration
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.compat.Platform

case class CalcPTest(width: Int, height: Int, topLeft: Complex, mathHeight: Double, maxIter: Int, times: Int) extends PerformanceTestSpec

class CalculatorPTester(threads: Int) extends PerformanceTester[CalcPTest] {

  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(threads).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  override def work(spec: CalcPTest): Long = {
    val area = Area(spec.topLeft, Scale(spec.mathHeight / (spec.height - 1)), spec.width, spec.height)
    val calculator = new Calculator(area, Mock.Plotter)
    val p = promise[Unit]()
    val done = p.future
    var time = System.nanoTime
    var subscription: Subscription = null
    subscription = calculator.calculate().subscribe (
      stat => stat match {
        case CalcStat(_, _, maxIter) => if (maxIter >= spec.maxIter) subscription.unsubscribe()
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

  override def cleanup(): Unit = {
    executor.shutdown()
  }

  override val verbose = false
}

object CalculatorPTester extends App {

  val testCases: List[(CalcPTest, Range)] = List(
    CalcPTest(640, 480, Complex(-2, -1.65), 2.5, 1000, 100) -> (1 to 8),
    CalcPTest(640, 480, Complex(-1.4048486487084642, -3.546832668775599E-10), 5.092590793509544E-10, 25000, 5) -> 4,
    CalcPTest(1920, 1080, Complex(-0.2281555005217314, -1.115142415586574), 5.69794211813246E-13, 80, 50) -> 4,
    CalcPTest(640, 480, Complex(BigDecimal(-2), BigDecimal(-1.65)), 2.5, 1000, 5) -> 4
  )

  testCases foreach { case (spec, threadRanges) =>
    threadRanges foreach { threads =>
      print(s"With $threads threads - ")
      new CalculatorPTester(threads).measure(spec)
      Platform.collectGarbage()
    }
  }

  implicit def Int2Range(n: Int): Range = n to n
}
