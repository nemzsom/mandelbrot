package hu.n.zs.mandelbrot

object PointLoc extends Enumeration {
  type PointLoc = Value
  /** Point is inside the mandelbrot set */
  val INSIDE = Value("INSIDE")
  /** We don't know at the current iteration */
  val UNSETTLED = Value("UNSETTLED")
  /** Point is outside the mandelbrot set */
  val OUTSIDE = Value("OUTSIDE")
}

import PointLoc._
import java.awt.Dimension

class Point(val x: Int, val y: Int, val scale: Double, var iter: Int, var iterValue: Complex, var location: PointLoc) {
  def complexValue = Complex(y * scale, x * scale)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Point]

  override def equals(other: Any): Boolean = other match {
    case that: Point =>
      (that canEqual this) &&
        x == that.x &&
        y == that.y &&
        scale == that.scale &&
        iter == that.iter &&
        iterValue == that.iterValue &&
        location == that.location
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(x, y, scale, iter, iterValue, location)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString(): String = {
    s"Point($x, $y, scale: $scale, iter: $iter, iterValue: $iterValue, loc: $location)"
  }
}

object Point {
  implicit def point2Dimensions(p: Point): Dimension = new Dimension(p.x, p.y)
  implicit def point2AwtPoint(p: Point) = new java.awt.Point(p.x, p.y)
}

class Area(val scale: Double, val data: Array[Point], val lineStride: Int, val startAt: Int, val width: Int, val height: Int) {

  def topLeft: Point = data(startAt)

  def pointAt(x: Int, y: Int): Point = data(startAt + x + y * lineStride)

  def update(f: Point => Unit): Unit = {
    for (x <- 0 until width; y <- 0 until height) {
      updateOne(pointAt(x, y))(f)
    }
  }

  protected def updateOne(point: Point)(f: Point => Unit): Unit = {
    f(point)
  }
}


