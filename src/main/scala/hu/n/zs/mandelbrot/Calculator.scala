package hu.n.zs.mandelbrot

import scala.annotation.tailrec
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent._
import scala.math._

/**
 * It can calculate an area's points.
 */
trait Calculator {

  val area: Area

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs *2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  def calc() : Unit = {
    area.subArea(0, 0, area.width - 1, 1)
    area.subArea(area.width - 1, 0, 1, area.height - 1)
    area.subArea(1, area.height - 1, area.width - 1, 1)
    area.subArea(0, 1, 1, area.height - 1)
  }
}

class Updater(val area: Area, val iterationStep: Int, val maxIter: Int) {

  val scale = area.scale

  // TODO remove test
  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs *2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)
  // end test

  def this(area: Area, iterationStep: Int) = this(area, iterationStep, iterationStep)

  lazy val update: Future[Updater] = future {
    updateArea()
    new Updater(area, iterationStep, maxIter + iterationStep)
  }

  private def updateArea(): Unit = {
    area.foreach { point =>
      if (point.location == Unsettled){
        logPoint(s"updatePoint to $maxIter", point)
        if (point.iter == 0 && preCheck_isInside(point.complexValue)) point.location = Inside
        else iterate(point)
        logPoint(s"updateResult", point)
      }
    }
  }

  private def iterate(point: Point): Unit = {
    val c = Point.complexAt(point.x, point.y, scale)
    logPoint(s"iterate to $maxIter", point)
    @tailrec def loop(iter: Int, z: Complex): Unit = {
      val escaped = z.escaped
      logPoint(s"loop $iter. escaped: $escaped", point)
      if (iter == maxIter || escaped) {
        point.iter = iter
        point.iterValue = z
        if (escaped) point.location = Outside(iter)
      }
      else loop(iter+1, z*z + c)
    }
    val z = point.iterValue
    loop(point.iter+1, z*z + c)
    logPoint(s"iteration result", point)
  }

  /** Optimization: Cardioid / bulb checking
    *  from http://en.wikipedia.org/wiki/Mandelbrot_set#Cardioid_.2F_bulb_checking
    */
  def preCheck_isInside(c: Complex): Boolean = {
    val q = pow(c.re - 0.25, 2) + pow(c.im, 2)
    q*(q + (c.re - 0.25)) < pow(c.im, 2) / 4 ||
      pow(c.re + 1, 2) + pow(c.im, 2) < 0.0625
  }

  implicit class ComplexOps(c: Complex) {

    def escaped: Boolean = c.re*c.re + c.im*c.im > 2*2
  }
}


