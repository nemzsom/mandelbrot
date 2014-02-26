package hu.nemzsom.mandelbrot

import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent._
import scala.concurrent.duration.Duration
import rx.lang.scala.Subscription

object Sandbox extends App {

  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(8).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  val width = 640
  val height = 480
  val area = Area(Complex(-2, -2), 4.0 / (width - 1), width, height)

  val calculator = new Calculator(area, Mock.Plotter)

  val p = promise[Unit]()
  val done = p.future

  var time = System.nanoTime
  val subscription: Subscription = calculator.calculate().subscribe (
    stat => stat match {
      case CalcStat(_, _, maxIter) => if (maxIter > 1000) subscription.unsubscribe()
    },
    error => println(error),
    () => {
      println("done")
      time = System.nanoTime - time
      p.success()
    }
  )

  Await.result(done, Duration.Inf)
  println(s"Calc done in ${time / 1000000} ms.")

}

object Mock {
  object Plotter extends Plotter {
    override def finish(points: Traversable[Point], cancel: () => Boolean): Unit = {}

    override def plot(p: Point): Unit = {}
  }
}
