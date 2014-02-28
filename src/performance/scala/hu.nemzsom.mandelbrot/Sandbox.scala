package hu.nemzsom.mandelbrot

import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent._

object Sandbox extends App {

  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(8).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  val width = 640
  val height = 480
  val area = Area(Complex(-2, -1.65), 2.5 / (width - 1), width, height)

  val perfTester = new CalculatorPerf

  perfTester.measure(CalcSpec(640, 480, Complex(-2, -2), 4, 100))
  executor.shutdown()

}

object Mock {
  object Plotter extends Plotter {
    override def finish(points: Traversable[Point], cancel: () => Boolean): Unit = {}

    override def plot(p: Point): Unit = {}
  }
}
