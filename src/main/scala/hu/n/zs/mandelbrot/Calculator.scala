package hu.n.zs.mandelbrot

import scala.annotation.tailrec
import PointLoc._

/**
 * It can calculate an area's points.
 */
trait Calculator {

  val area: Area
  lazy val scale = area.scale

  def calculate(maxIter: Int): Unit = {
    area.update { point =>
      if (point.location == UNSETTLED) iterate(maxIter, point)
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

  implicit class ComplexOps(c: Complex) {

    def escaped: Boolean = c.re*c.re + c.im*c.im > 2*2
  }
}


