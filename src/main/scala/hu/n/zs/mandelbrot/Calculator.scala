package hu.n.zs.mandelbrot

import scala.annotation.tailrec
import PointLoc._
import java.util.concurrent.{Executors, ThreadPoolExecutor}
import scala.concurrent.ExecutionContext
import scala.math._

/**
 * It can calculate an area's points.
 */
trait Calculator {

  val area: Area
  lazy val scale = area.scale

  val numOfProcs = Runtime.getRuntime.availableProcessors
  val executor: ThreadPoolExecutor = Executors.newFixedThreadPool(numOfProcs *2).asInstanceOf[ThreadPoolExecutor]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(executor)

  def calc() : Unit = {
    area.subArea(0, 0, area.width - 1, 1)
    area.subArea(area.width - 1, 0, 1, area.height - 1)
    area.subArea(1, area.height - 1, area.width - 1, 1)
    area.subArea(0, 1, 1, area.height - 1)
  }

  def calculate(maxIter: Int): Unit = {
    area.update { point =>
      if (point.location == UNSETTLED){
        if (point.iter == 0 && preCheck_isInside(point.complexValue)) point.location == INSIDE
        else iterate(maxIter, point)
      }
    }
  }

  private def iterate(maxIter: Int, point: Point): Unit = {
    val c = Point.complexAt(point.x, point.y, scale)
    @tailrec def loop(iter: Int, z: Complex): Unit = {
      val escaped = z.escaped
      if (iter == maxIter || z.escaped) {
        point.iter = iter
        point.location = if (escaped) OUTSIDE else UNSETTLED
      }
      else loop(iter+1, z*z + c)
    }
    loop(point.iter, point.iterValue)
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


